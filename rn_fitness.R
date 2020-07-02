rn_fitness <- function(y, ks = 1:50, ds = 1:50, initial = NULL, learners = 20,
                localMins = 0, threshold = 1, tournament = FALSE, elitism = 0,
                distance = "manhattan", error_measure = "RMSE",
                weight = "proportional", ensemble = "proportional", n_threads = 8) {
  library(zoo)
  library(forecast)
  library(tseries)
  library(knnp)

  # Elitism must be a positive number less or equal to the wanted learners
  if (elitism < 0 || elitism > learners) {
    stop(paste0("Incorrect value for elitism: ", elitism))
  }

  # Local minimums' range can't be negative
  if (localMins < 0) {
    stop(paste0("Incorrect value for localMins: ", localMins))
  }
  
  # Threshold range must be between 0 and 1
  if (threshold < 0 || threshold > 1) {
    stop(paste0("Threshold value must be between 0 and 1, defined:: ", threshold))
  }
  # Threshold range must leave at least 2 combinations
  if (ceiling(length(ks) * length(ds) * threshold) <= 1) {
    stop(paste0("Threshold value too low"))
  }

  # Set all the optimization parameters
  n <- NROW(y)

  if (is.null(initial)) {
    warning("No 'initial' index given, assuming 90% of series as known")
    initial <- floor(n * 0.9)
  }
  if (all(ensemble != c("proportional", "average"))) {
    stop(paste0("Ensemble metric '", ensemble, "' unrecognized."))
  }

  # Set the random seed. Uncomment for reproducibility
  # set.seed(1)

  # Get errors matrix, and best k and d combination
  res <- knn_param_search(y = y, k = ks, d = ds, initial = initial, distance = distance, 
                          error_measure = error_measure, weight = weight, threads = n_threads)

  chosK <- vector(mode = "integer", length = 0)
  chosD <- vector(mode = "integer", length = 0)
  probs <- 1/res$errors

  # If elitism is set, the best K-D combinations will be taken directly
  if (elitism > 0) {
    KDs <- which(res$errors <= sort.int(res$errors, partial = elitism)[elitism], arr.ind = TRUE)
    chosK <- ks[as.vector(KDs[,1])]
    chosD <- ds[as.vector(KDs[,2])]
  }
  
  # If threshold is set, only errors better than given quantile will be taken into account
  if (threshold < 1) {
    probs[which(res$errors > quantile(res$errors, threshold))] <- 0
  }


  # If a radius for local minimums is given, only K-D combinations better than their surrounding 
  # combinations, considering given radius as maximum distance, will be taken into account
  if (localMins > 0) {
    selectK <- vector(mode = "integer", length = length(res$errors))
    selectD <- vector(mode = "integer", length = length(res$errors))

    # Initialize number of minimumns found
    act <- 0
    # Set search radius
    delta <- seq(-localMins, localMins)
    for (c in 1:NCOL(res$errors)) for (r in 1:NROW(res$errors)) {
      # From actual position, get all nearby at given radius
      rows <- r + delta
      # If any position falls out of errors matrix, remove them
      rows <- rows[(rows > 0 & rows <= NROW(res$errors))]
      # From actual position, get all nearby at given radius
      cols <- c + delta
      # If any position falls out of errors matrix, remove them
      cols <- cols[(cols > 0 & cols <= NCOL(res$errors))]
      # If evaluated error is lower than surrounding, excluding NAs previously excluded by threshold, we take it
      if (probs[r, c] > 0 && all(res$errors[r,c] <= res$errors[rows, cols], na.rm = TRUE)) {
        act <- act + 1
        selectK[act] <- ks[r]
        selectD[act] <- ds[c]
      }
    }

    if (act == 0) {
      warning(paste0("No local minimums were find with a radius of ", radius))
      return(list(chosK = NULL, chosD = NULL, weights = NULL))
    }

    # Remove unused positions
    selectK <- selectK[1:act]
    selectD <- selectD[1:act]

    # Convert selected combinatons into matrix positions
    chosens <- matrix(data = c(selectK, selectD), nrow = length(selectK), ncol = 2)

    # Remove all combinations that doesn't represent local minimums
    probs[,] <- 0
    probs[chosens] <- 1/res$errors[chosens]
  }

  # Count how many learners are remaining
  remaining <- learners - length(chosK)
  if (remaining > 0) {
    # If tournament is chosen, combinations will be taken by pairs and only the best one will be taken
    if (tournament) {

      chosens <- chos_errs <- matrix(nrow = remaining, ncol = 2)

      # Generate first round of combinations and get their errors
      chosens[,1] <- sample(x = 1:length(res$errors), size = remaining, replace = TRUE, prob = probs)
      chos_errs[,1] <- -res$errors[chosens[,1]]
      # Generate second round of combinations and get their errors
      chosens[,2] <- sample(x = 1:length(res$errors), size = remaining, replace = TRUE, prob = probs)
      chos_errs[,2] <- -res$errors[chosens[,2]]

      # Get the best combination of each couple
      winners <- matrix( data = c(1:remaining, max.col(chos_errs)), nrow = remaining, ncol = 2)

      chosK <- c(chosK, ks[((chosens[winners] - 1) %% length(ks)) + 1])
      chosD <- c(chosD, ds[ceiling(chosens[winners] / length(ks))])
    }

    else {
      # Random combinations are taken, with greater probability for lower error combinations
      ##### WARNING: careful about Error Measures that can be negative (as ME)
      chosen <- sample(x = 1:length(res$errors), size = remaining, replace = TRUE, prob = probs)

      chosK <- c(chosK, ks[((chosen - 1) %% length(ks)) + 1])
      chosD <- c(chosD, ds[ceiling(chosen / length(ks))])
    }
  }

  weights <- switch(ensemble,
                   proportional = 1/res$errors[matrix(data = c(chosK, chosD), ncol = 2)],
                   rep.int(1, length(chosK))
  )
  
  return(list(chosK = chosK, chosD = chosD, weights = weights))
}
