rn_blocks <- function(y, ks = 1:50, ds = 1:50, initial = NULL, learners = 20, 
                      train_length = 50, distance = "manhattan", n_threads = 2, 
                      weight = "proportional", ensemble = "proportional", error_measure = "RMSE") {
  library(zoo)
  library(forecast)
  library(tseries)
  library(knnp)
  library(foreach)
  
  n <- NROW(y)

  if (!is.null(initial) && initial < max(ks)+max(ds)+1) {
    stop(paste0("Not enogh series to predict. Expand initial or reduce k and/or d"))
  }

  if (is.null(initial)) {
    warning("No 'initial' index given, assuming around 30% of series as warm-up")
    initial <- max(floor(n * 0.3), max(ks)+max(ds)+1)
  }
  
  if (all(ensemble != c("proportional", "average"))) {
    stop(paste0("Ensemble metric '", ensemble, "' unrecognized."))
  }
  
  clust <- parallel::makeCluster(n_threads)
  doParallel::registerDoParallel(cl = clust)

  chosKDW <- foreach(i = 1:learners, .combine = cbind,
                    .packages = c("forecast", "parallelDist", "knnp") ) %dopar% {
    # Set the random seed in each thread. Uncomment for reproducibility
    # set.seed(i)

    # Define tolerances of lateral sampling
    left_toler <- ceiling(train_length*0.9)
    right_toler <- ceiling(train_length*0.1)

    # Generate the beginning of the chunk to predict
    init <- sample(x = (initial - left_toler):(n - right_toler), size = 1)
    # Set the end of the chunk, avoiding going out of the end of the series
    fin <- min(init + train_length, n)
    # In case the sampling start was defined as previous to init by tolerance, fix it
    init <- max(init, initial)

    # Test each k and D for that chunk and get the best one
    learn <- knn_param_search(y = head(y, fin), k = ks, d = ds, initial = init, distance = distance, 
                              error_measure = error_measure, weight = weight, threads = 1)
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
