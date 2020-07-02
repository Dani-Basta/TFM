rn_forecast <- function(y, learners, initial = NULL, distance = "manhattan",
                          weight = "proportional", n_threads = 5) {
  library(zoo)
  library(forecast)
  library(tseries)

  # A list containing both vectors can be provided
  if (typeof(learners) == "list") {
    if ( is.null(learners$chosK) || is.null(learners$chosD) ||
         is.null(learners$weights) ) 
      stop("Missing parameters in learners")
  
    if ( (length(learners$chosK) != length(learners$chosD)) ||
         (length(learners$chosK) != length(learners$weights)) )
      stop("Input vectors' length mismatch")

    ks <- learners$chosK
    ds <- learners$chosD
    weights <- learners$weights
  }
  else stop("Invalid input type")

  # Set all the optimization parameters
  n <- NROW(y)
  if (is.null(initial)) {
    warning("No 'initial' index given, assuming 90% of series as known")
    initial <- floor(n * 0.9)
  }


  predictions <- matrix(nrow = length(ks), ncol = n - initial)

  # For each K-D combination store the prediction in one row
  for (i in 1:length(ks))
    predictions[i,] <- knn_past(y = y, k = ks[i], d = ds[i], distance = distance,
                                initial = initial, weight = weight, threads = n_threads)$fitted


  # Generate prediction by getting mean value between all predictions
  result <- colSums(predictions * weights) / sum(weights)

  # Measure it's accuracy with forecast's function
  accu <- accuracy(result, tail(y, NCOL(predictions)))

  # Calculate prediction's residuals
  residu <- tail(y, length(result)) - result

  return(list(prediction = result, accu = accu, residu = residu))
}
