rn_samples <- function(y, ks = 1:50, ds = 1:50, initial = NULL, learners = 20, 
                  n_instants = 80, distance = "manhattan", n_threads = 2, 
                  error_measure = "RMSE", weight = "proportional", ensemble = "proportional") {
  library(zoo)
  library(forecast)
  library(tseries)
  library(foreach)
  library(knnp)

  # Set all the optimization parameters
  n <- NROW(y)

  # Set the length of the initial chunk of the series to leave behind as work-map
  if (is.null(initial)) {
    warning("No 'initial' index given, assuming around 30% of series as warm-up")
    initial <- max(floor(n * 0.3), max(ks)+max(ds)+1)
  }
  
  if (all(ensemble != c("proportional", "average"))) {
    stop(paste0("Ensemble metric '", ensemble, "' unrecognized."))
  }

  clust <- parallel::makeCluster(n_threads)
  doParallel::registerDoParallel(cl = clust)

  chosKDW <- foreach(i = 1:learners, .combine = cbind, .export = c("knn_param_search_discr", "knn_elements"),
                    .packages = c("forecast", "parallelDist", "knnp") ) %dopar% {
    # Set the random seed in each thread. Uncomment for reproducibility
    # set.seed(i)

    # Take a sample of instants to predict and measure error
    instants <- sample(x = (initial + 1):n, size = n_instants, replace = TRUE)
    
    learn <- knn_param_search_discr(y = head(y, max(instants)), k = ks, d = ds,
                    indexes = instants, distance = distance, weight = weight,
                      error_measure = error_measure, v = 1, threads = 1)

    weight <- switch(ensemble,
                     proportional = 1 / learn$errors[learn$opt_k, learn$opt_d],
                     average = 1
    )

    matrix(c(learn$opt_k, learn$opt_d, weight), nrow = 3, ncol = 1)
  }

  foreach::registerDoSEQ()
  parallel::stopCluster(clust)

  return(list(chosK = chosKDW[1,], chosD = chosKDW[2,], weights = chosKDW[3,]))
}
