  rn_localMins <- function(y, ks = 1:50, ds = 1:50, initial = NULL, 
                    learners = 15, radius = 1, threshold = 1,
                    distance = "manhattan", n_threads = 8, ensemble = "proportional",
                    error_measure = "RMSE", weight = "proportional") {
  library(zoo)
  library(forecast)
  library(tseries)
  library(knnp)
  library(foreach)

  # Local minimums' range must be greater than 0
  if (radius <= 0) {
    stop(paste0("Incorrect value for radius: ", radius))
  }

  # Set all the optimization parameters
  n <- NROW(y)
  if (is.null(initial)) {
    warning("No 'initial' index given, assuming 90% of series as known")
    initial <- floor(n * 0.9)
  }

  # Get errors matrix, and best k and d combination
  res <- knn_param_search(y = y, k = ks, d = ds, initial = initial, distance = distance, 
                          error_measure = error_measure, weight = weight, threads = n_threads)

  chosK <- vector(mode = "integer", length = 0)
  chosD <- vector(mode = "integer", length = 0)
  probs <- 1/res$errors

  # If threshold is set, only errors below given quantile will be taken into account
  if (threshold < 1) {
    probs[which(res$errors > quantile(res$errors, threshold))] <- 0
  }

  # Only K-D combinations better than their surrounding, considering given
  # radius as maximum distance, will be taken into account
  selectK <- vector(mode = "integer", length = length(res$errors))
  selectD <- vector(mode = "integer", length = length(res$errors))

  # Initialize number of minimumns found
  act <- 0
  # Set search radius
  delta <- seq(-radius, radius)
  for (c in 1:NCOL(res$errors)) for (r in 1:NROW(res$errors)) {
    # From actual position, get all nearby at given radius
    rows <- r + delta
    # If any position falls out of errors matrix, remove them
    rows <- rows[(rows > 0 & rows <= NROW(res$errors))]
    # From actual position, get all nearby at given radius
    cols <- c + delta
    # If any position falls out of errors matrix, remove them
    cols <- cols[(cols > 0 & cols <= NCOL(res$errors))]
    # If evaluated error is lower than surrounding, excluding NAs and those 
    # previously excluded by threshold, we take it
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
  chosK <- selectK[1:act]
  chosD <- selectD[1:act]

  # Convert selected combinations into their matrix positions
  chosens <- matrix(data = c(chosK, chosD), nrow = act, ncol = 2)
  # weights <- probs[chosens]
  weights <- switch(ensemble,
                    proportional = probs[chosens],
                    rep.int(1, length(chosK))
  )

  if (!is.null(learners) && learners > 0) {
    if (act > learners) {
      message(paste0("More local minimums than established learners, trimming"))

      # Sort combinations by error and pick only 'learners' best
      shortered <- head(sort.int(weights, decreasing = TRUE,
                         partial = 1:learners, index.return = TRUE)$ix, learners)

      chosK <- chosK[shortered]
      chosD <- chosD[shortered]
      weights <- weights[shortered]
    }

    else if (act < learners) {
      warning(paste0("Less local minimums than established maximum"))
    }
  }

  return(list(chosK = chosK, chosD = chosD, weights = weights))
}
