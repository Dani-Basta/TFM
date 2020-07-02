library(zoo)
library(foreach)
library(forecast)
library(tseries)
library(knnp)

### Choose which series to use. If it has timestamps use them for plotting, else use the index
# y <- nottem
# y <- datasets::sunspot.month
# dates <- as.Date(time(y))
# full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))


# y <- forecast::taylor
# # y <- tail(y, length(y)*0.9)
# dates <- 1:length(y)
# full_dates <- c(dates, length(y) + 1)


# y <- ts(data = get("rain"), start = as.Date("1914-01-01"), frequency = 365)
# # seq.POSIXt(from = as.POSIXct("2000-01-05 00:00:00"), to = as.POSIXct("2000-08-27 00:00:00"), by = "30 min")
# dates <- as.Date("1914-01-01") + 0:(length(y) - 1)
# full_dates <- as.Date("1914-01-01") + 0:length(y)


# y <- ts(airNO2)
# dates <- seq.POSIXt(from = as.POSIXct("2017-10-01 00:00"), by = "hour",  length.out = length(airNO2))
# # y <- xts::xts(x = airNO2, order.by = dates)
# full_dates <- c(dates, tail(dates,1) + (60*60) )




# y <- as.matrix(interCompFore, ncols = 1)
# dates <- 1:length(y)
# full_dates <- 1:(length(y)+1)

# y <- as.matrix(btcnWeekRend, ncols = 1)
# dates <- as.Date(read.csv("D:/Daniel/Investigación/BTC-EUR-weekly.csv")[,1])
# full_dates <- as.Date(ts( c(y,1), start = time(y)[1], frequency = frequency(y) ))

# y <- ts(snpLogRet)
# dates <- snpDates
# full_dates <- c(dates, as.Date("2020-01-01"))


# Then we set all the optimization parameters
n <- NROW(y)
train_init <- floor(n * 0.6)
test_init <- floor(n * 0.9)
y_train <- head(y, test_init)
distance <- "manhattan"
error_measure <- "RMSE"
weight <- "proportional"
n_threads <- 15
ks <-  1:50
ds <- 1:50
min_y <- min(y)
max_y <- max(y)

# Get errors matrix, and best k and d combination
res <- knn_param_search(y = y_train, k = ks, d = ds, initial = train_init, distance = distance,
                        error_measure = error_measure, weight = weight, threads = n_threads)

# Generate the forecast from the beginning of the train to the end of the series
optimal <- knn_past(y = y, k = res$opt_k, d = res$opt_d, initial = train_init, 
                    distance = distance, weight = weight, threads = n_threads)

# And split the forecast between train and test intervals
optimal_train <- head(optimal$fitted, test_init - train_init )
optimal_test <- tail(optimal$fitted, n - test_init )

# Split series to measure train and test accuracy
y_err <- tail(y, (n - train_init )) 
y_train_err <- head(y_err, test_init - train_init) 
y_test_err <- tail(y, n - test_init) 

# Get train+test dates for plots
sub_dates <- tail(dates, length(y) - train_init)

# Generate naive forecast
naive <- y[train_init:(n - 1)]

# minimums <- head(sort.int(res$errors, index.return = TRUE)$ix , 5)
minimums <- sort.int(res$errors, index.return = TRUE)$ix

# Indexes of K and D minimums
x_minims <- ((minimums - 1) %% length(ks)) + 1
y_minims <- ceiling(minimums/length(ks))


# Data for residuals
residuals_matrix <- matrix(nrow = 5, ncol = length(y_err))
residuals_matrix[1, ] <- y_err - as.vector(optimal$fitted)
residuals_matrix[2, ] <- y_err - as.vector(naive)

# Data for errors table
names_col <- c("Optimal", "Naive", "Seasonal Naive", "", "")

optimal_train_error <- accuracy(optimal_train, y)
optimal_test_error  <- accuracy(optimal_test, y)

naive_train_error <- accuracy(naive[1:length(y_train_err)], y_train_err)
naive_test_error  <- accuracy(naive[(length(y_train_err) + 1):length(naive)], y_test_err)
naive_total_error <- naive_train_error[switch(error_measure, "ME" = 1, "RMSE" = 2, "MAE" = 3, "MPE" = 4, "MAPE" = 5)]

# Errors heatmap plot information
cont_min <- min(res$errors)
cont_max <- max(res$errors)
cont_max_fix <- (cont_max - (cont_max - cont_min) * 0.4)
num_contours <- 18

# Generate errors matrix for error table in Optimization tab
errors_matrix <- matrix(nrow = 2, ncol = 10)
colnames(errors_matrix) <- c("Train-ME", "RMSE", "MAE", "MPE", "MAPE", "Test-ME", "RMSE", "MAE", "MPE", "MAPE")
rownames(errors_matrix) <- c("kNN", "Naive") #, "ets", "SeasNai")
errors_matrix[1, ] <- c(optimal_train_error[1:5], optimal_test_error[1:5])
errors_matrix[2, ] <- c(naive_train_error[1:5], naive_test_error[1:5])

errors_matrix_tab1 <- errors_matrix
errors_matrix_tab2 <- errors_matrix

# Data for selected methods
selected_methods <- rep(FALSE, 5)

# Data for selected points in contour
selected_points <- matrix(rep(FALSE, NROW(res$errors) * NCOL(res$errors)), nrow = NROW(res$errors), ncol = NCOL(res$errors))
# selected_points_aux <<- selected_points
previous_countour <<- "default"

# Standard deviation of distances to k-nearest neighbors of all predictions 
future_values <- matrix(y[optimal$neighbors], nrow = nrow(optimal$neighbors))
neighs_stdev <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
rm(future_values)

# Set confidence interval for loess prediction
confid_int <- 0.975

# Loess model for Error depending on Distance
loess_mod_DisErr <- loess(abs(residuals_matrix[1, ]) ~ optimal$knn_dists, span = 0.75)
loess_ind_DisErr <- sort.int(loess_mod_DisErr$x[,1], index.return = TRUE)$ix
loess_mean_DisErr <- predict(loess_mod_DisErr, se = TRUE)
loess_ci_DisErr <- qt(confid_int, loess_mean_DisErr$df) * loess_mean_DisErr$se.fit

# Loess model for Error depending on Deviation
loess_mod_DevErr <- loess(abs(residuals_matrix[1, ]) ~ neighs_stdev, span = 0.75)
loess_ind_DevErr <- sort.int(loess_mod_DevErr$x[,1], index.return = TRUE)$ix
loess_mean_DevErr <- predict(loess_mod_DevErr, se = TRUE)
loess_ci_DevErr <- qt(confid_int, loess_mean_DevErr$df) * loess_mean_DevErr$se.fit

# Generate loess model for all combinations of distances and deviations
loess_points <- 100
loess_distances <- seq.int(from = min(optimal$knn_dists), to = max(optimal$knn_dists), length.out = loess_points)
loess_deviations <- seq.int(from = min(neighs_stdev), to = max(neighs_stdev), length.out = loess_points)
loess_DisDev_Err <- loess(abs(residuals_matrix[1, ]) ~ optimal$knn_dists + neighs_stdev)
loess_DisDev_heatmap <- matrix(predict(object = loess_DisDev_Err, newdata = matrix( 
  c(rep(loess_distances, each = loess_points), rep(loess_deviations, times = loess_points)), ncol = 2)), nrow = loess_points)
if (!error_measure %in% c("ME", "MPE")) {
  loess_DisDev_heatmap[loess_DisDev_heatmap < 0] <- NA
}
