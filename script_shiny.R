library(plotly)
library(shiny)
library(shinyWidgets)
library(DT)
library(rdist)

selected_points <- matrix(FALSE, nrow = NROW(res$errors), ncol = NCOL(res$errors))
combinations <- matrix(FALSE, nrow = NROW(res$errors), ncol = NCOL(res$errors))

server <- function(input, output, session) {
    distsReactVals <- reactiveValues(
        actK = res$opt_k, 
        actD = res$opt_d, 
        absErr = TRUE, 
        thresh = 0.8, 
        click = NULL,
        actPred = NULL,
        pastPred = NULL)

    observeEvent(input$buttSeriesPlot, {
        actK <- input$selKtabDist
        distsReactVals$actK <- ifelse(actK == "", res$opt_k, as.numeric(actK))
        actD <- input$selDtabDist
        distsReactVals$actD <- ifelse(actD == "", res$opt_d, as.numeric(actD))
        distsReactVals$click <- NULL
        distsReactVals$pastPred <- NULL
        distsReactVals$absErr <- input$chbabsDist
        if ( actK == res$opt_k && actD == res$opt_d ) {
            distsReactVals$actPred <- NULL
        }
        else {
            distsReactVals$actPred <- knn_past(y = y, k = actK, d = actD, 
                initial = train_init, distance = distance, weight = weight, threads = n_threads)
        }            
    })


    observeEvent(event_data("plotly_click", source = "main", priority = "event"), {
        click <- match(as.Date(event_data("plotly_click", source = "main", priority = "event")[[3]]), dates)
        distsReactVals$click <- click
        # Generate prediction to know past information
        distsReactVals$pastPred <- knn_forecast(y = head(y, click), k = distsReactVals$actK,
            d = distsReactVals$actD, distance = distance, weight = weight, threads = n_threads)
    })

    # Plot of series + errors + Std. Deviations. 
    # Default optimal k-d but can be changed dynamically. 
    # Also clickable for interactivity
    output$elemsPlot <- renderPlotly({
        pMain <<- pMainBase
        
        # Update reactive values
        actK <- distsReactVals$actK
        actD <- distsReactVals$actD
        newPred <- distsReactVals$actPred
        absErr <- distsReactVals$absErr
        x_click <- distsReactVals$click
        
        distances <- 0
        deviations <- 0
        
        # If selected K-D is the optimal or leaved blank, use pre-made plots as base
        if ( is.null(newPred) ) {
            
            # Plot optimal k-d combination forecast
            pMain <<- add_trace(pMain, x = sub_dates, y = optimal$fitted, line = list(color = colPalette[2]), 
                                name = paste0("Optimal (k = ", res$opt_k, ", d = ", res$opt_d, ")"), legendgroup = "optim")
            
            errors <- residuals_matrix[1, ]
            
            # Save 'y' range for plotted lines when clicked
            y_low  <- min_y - 0.05 * (max_y - min_y)
            y_high <- max_y + 0.05 * (max_y - min_y)
            
            # Forecast errors in absolute value or not, depending on check. Also save Errors range for click event
            if (absErr == TRUE) {
                pError <<- plot_ly(x = sub_dates, y = abs(errors), legendgroup = "optim", hoverinfo = "x+y", 
                                   name = "Optimal error", type = "bar", marker = list(color = colPalette[2]) )
                err_low  <- min(abs(errors)) - 0.05 * (max(abs(errors)) - min(abs(errors)))
                err_high <- max(abs(errors)) + 0.05 * (max(abs(errors)) - min(abs(errors)))
            }
            else {
                pError <<- plot_ly(x = sub_dates, y = errors, legendgroup = "optim", hoverinfo = "x+y", 
                                   name = "Optimal error", type = "bar", marker = list(color = colPalette[2]) )
                err_low  <- min(errors) - 0.05 * (max(errors) - min(errors))
                err_high <- max(errors) + 0.05 * (max(errors) - min(errors))
            }
            pkNNDist <- pkNNDistOptim
            pNeighStdev <- pNeighStdevOptim
            
            distances <- 
            deviations <- neighs_stdev
        }
        
        # Non-optimal case. Make all dynamically
        else {
            preds <- newPred$fitted
            
            pMain <<- add_trace(pMain, x = sub_dates, y = preds, name = paste0("k = " , actK, ", d = " , actD, " prediction"), 
                                legendgroup = paste("k", actK, "d", actD), line = list(color = colPalette[2]))
            
            errors <- y_err - preds
            
            y_low  <- min(preds) - 0.05 * (max(preds) - min(preds))
            y_high <- max(preds) + 0.05 * (max(preds) - min(preds))
            
            if (absErr == TRUE) {
                pError <<- plot_ly(x = sub_dates, y = abs(errors), legendgroup = paste("k", actK, "d", actD), hoverinfo = "x+y",
                                   name = paste0("k = " , actK, ", d = " , actD, " error"), type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(abs(errors)) - 0.05 * (max(abs(errors)) - min(abs(errors)))
                err_high <- max(abs(errors)) + 0.05 * (max(abs(errors)) - min(abs(errors)))
            }
            else {
                pError <<- plot_ly(x = sub_dates, y = errors, legendgroup = paste("k", actK, "d", actD), hoverinfo = "x+y",
                                   name = paste0("k = " , actK, ", d = " , actD, " error"), type = "bar", marker = list(color = colPalette[2]) ) 
                err_low  <- min(errors) - 0.05 * (max(errors) - min(errors))
                err_high <- max(errors) + 0.05 * (max(errors) - min(errors))
            }
            
            distances <- newPred$knn_dists/actK
            # Generate plot of mean distance to all neighbors
            pkNNDist <- plot_ly(name = "Neighbors distances", showlegend = TRUE, hoverinfo = "x+y",
                                type = "bar", marker = list(color = colPalette[3]), y = distances,
                                # x = sub_dates, y = newPred$knn_dists/actK)
                                x = head(tail(dates, length(y) + 1 - train_init), length(sub_dates)))
            
            
            # Calculate standard deviation for next value of k-neighbors
            future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
            deviations <- rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1)
            pNeighStdev <- plot_ly(name = "Future values Std.Dev.", showlegend = TRUE, hoverinfo = "x+y",
                                   type = "bar", marker = list(color = colPalette[1]), y = deviations,
                                   # x = sub_dates, 
                                   x = head(tail(dates, length(y) + 1 - train_init), length(sub_dates)))
        }
        
        # Clicked on main plot
        if (!is.null(x_click)) {
            # Add line where clicked
            pMain <<- add_segments(pMain, x = dates[x_click], xend = dates[x_click], y = y_low, yend = y_high, 
                                   name = "Knn", showlegend = FALSE, hoverinfo = "x", # text = "Knn",  
                                   legendgroup = "knn", line = list(color = "black", width = 1.5, dash = "dash"))
            
            # Also if clicked element is not the last one, plot it's corresponding error
            if ( x_click < n ) {
                pError <<- add_segments(pError, x = full_dates[x_click+1], xend = full_dates[x_click+1], y = err_low, yend = err_high, 
                                        name = "Knn", showlegend = FALSE, hoverinfo = "text", type = "line", mode = "line",  
                                        legendgroup = "knn", line = list(color = "black", width = 1.5, dash = "dash"), text = "Prediction error \n for selected")
                pkNNDist <- add_segments(pkNNDist, x = full_dates[x_click+1], xend = full_dates[x_click+1], y = min(distances), yend = max(distances), 
                                        name = "Knn", showlegend = FALSE, hoverinfo = "text", type = "line", mode = "line",  
                                        legendgroup = "knn", line = list(color = "black", width = 1.5, dash = "dash"), text = "Prediction distance \n for selected")
                pNeighStdev <- add_segments(pNeighStdev, x = full_dates[x_click+1], xend = full_dates[x_click+1], y = min(deviations), yend = max(deviations), 
                                        name = "Knn", showlegend = FALSE, hoverinfo = "text", type = "line", mode = "line",  
                                        legendgroup = "knn", line = list(color = "black", width = 1.5, dash = "dash"), text = "Prediction std. dev. \n for selected")
            }
            
            # Save the number of shapes already present in the main plot, in order to add new shaded areas on each neighbor
            shapesInd <- length(pMain[["x"]][["layoutAttrs"]][[ pMain[["x"]][["cur_data"]] ]][["shapes"]]) + 1
            
            # Get indexes of the neighbors
            neighbors <- knn_forecast(y = head(y, x_click), k = actK, d = actD, distance = distance, 
                                      weight = weight, threads = n_threads)$neighbors
            
            ind <- 1
            for (i in neighbors) {
                # Add a vertical line on the neighbor
                pMain <<- add_segments(pMain, x = dates[i], xend = dates[i], y = y_low, yend = y_high, name = "Knn",
                                       showlegend = FALSE, text = paste0(ind, "-nearest"), hoverinfo = "x+text",
                                       legendgroup = "knn", line = list(color = "red", width = 1.5, dash = "dash"))
                
                # And add a rectangle with lenth D close to the line
                pMain[["x"]][["layoutAttrs"]][[pMain[["x"]][["cur_data"]]]][["shapes"]][[shapesInd]] <<- 
                    list(type = "rect", fillcolor = "red", line = list(color = "red"), opacity = 0.2,
                         x0 = dates[(i + 1 - actD)], x1 = dates[i], xref = "x", yref = "y",
                         y0 = 0.95 * min(y[(i + 1 - actD):i]), y1 = 1.05 * max(y[(i + 1 - actD):i])) 
                
                ind <- ind + 1
                shapesInd <- shapesInd + 1
            }
        } 
        s <- subplot(pMain, pkNNDist, pNeighStdev, pError, nrows = 4, shareX = TRUE)
        s$x$source <- "main"
        s
    })


    # Plot of relation between neighbors' Distances and prediction errors
    output$scattDistErr <- renderPlotly({
        # Update reactive values
        actK <- distsReactVals$actK
        actD <- distsReactVals$actD
        newPred <- distsReactVals$actPred

        # If optimal K-D combination use pre-made plot
        if ( is.null(newPred) ) {
            subplot(pScatDistErrOptim,pScatStddevErrOptim,pDisDevHeatOptim, nrows = 1, widths = c(0.25,0.25,0.5),
                    titleX = TRUE, titleY = TRUE)
        }
        # In case another combination is chosen
        else {
            # Generate forecast for selected combination 
            
            #Generate loess model
            loess_mod <- loess(abs(y_err - newPred$fitted) ~ newPred$knn_dists, span = 0.75)
            loess_ind <- sort.int(loess_mod$x[,1], index.return = TRUE)$ix
            loess_mean <- predict(loess_mod, se = TRUE)
            loess_ci <- qt(confid_int, loess_mean$df) * loess_mean$se.fit
            
            # Plot of relation between Distances and Errors, including Loess stimation with confidence interval
            p1 <- plot_ly(name = "Distances vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                          x = loess_mod$x[,1][loess_ind], y = loess_mean$fit[loess_ind], showlegend = FALSE) %>%
                add_trace(y = (loess_mean$fit - loess_ci)[loess_ind]) %>%
                add_trace(y = (loess_mean$fit + loess_ci)[loess_ind], 
                          fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
                add_trace(x = newPred$knn_dists, y = abs(y_err - newPred$fitted), mode = "markers", marker = list(opacity = 0.4),
                          hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
                layout(xaxis = list(title = "Distances"), yaxis = list(title = "Errors"))
            
            # Calculate standard deviation of neighbors
            future_values <- matrix(y[newPred$neighbors], nrow = nrow(newPred$neighbors))
            std_devs <- sqrt(rowSums((t(future_values) - colMeans(future_values))**2) / (nrow(future_values) - 1))
            
            #Generate loess model
            loess_mod <- loess(abs(y_err - newPred$fitted) ~ std_devs, span = 0.75)
            loess_ind <- sort.int(loess_mod$x[,1], index.return = TRUE)$ix
            loess_mean <- predict(loess_mod, se = TRUE)
            loess_ci <- qt(confid_int, loess_mean$df) * loess_mean$se.fit
            
            # Plot of relation between Std. Deviations and Errors, including Loess stimation with confidence interval
            p2 <- plot_ly(name = "Std. Dev. vs Errors", type = 'scatter', mode = "lines", hoverinfo = "x+y",
                          x = loess_mod$x[,1][loess_ind], y = loess_mean$fit[loess_ind], showlegend = FALSE) %>%
                add_trace(y = (loess_mean$fit - loess_ci)[loess_ind]) %>%
                add_trace(y = (loess_mean$fit + loess_ci)[loess_ind], 
                          fill = "tonexty", fillcolor = "rgba(0,0,100,0.35)") %>%
                add_trace(x = std_devs, y = abs(y_err - newPred$fitted), mode = "markers", marker = list(opacity = 0.4),
                          hoverinfo = "x+y+text", text = head(tail(dates, length(y) + 1 - train_init), length(sub_dates))) %>%
                layout(xaxis = list(title = "Std. Dev."), yaxis = list(title = "Errors"))
            
            
            # Generate loess model for all combinations of distances and deviations
            points <- 100
            dist <- seq.int(from = min(newPred$knn_dists), to = max(newPred$knn_dists), length.out = points)
            dev <- seq.int(from = min(std_devs), to = max(std_devs), length.out = points)
            DisDev_Err <- loess(abs(y_err - newPred$fitted) ~ newPred$knn_dists + std_devs)
            DisDev_heatmap <- matrix(predict(object = DisDev_Err, 
                                             newdata = matrix( 
                                                 c(rep(dist, each = points), rep(loess_deviations, times = points)), 
                                                 ncol = 2)), nrow = points)
            DisDev_heatmap[DisDev_heatmap < 0] <- NA
            
            
            # Heatmap of all possible combinations that could exist between Distances, Deviations and Errors
            p3 <- plot_ly(x = dist, y = dev, z = DisDev_heatmap, type = "contour", 
                          colorscale = "Jet", hoverinfo = "x+y+z", showlegend = FALSE) %>% 
                add_trace(x = newPred$knn_dists, y = std_devs, type = "scatter", mode = "markers", hoverinfo = "x+y",
                          marker = list(color = "rgba(0,0,0,0.7)", size = 3, line = list(color = "rgba(255,255,255,0.7)",  width = 1))) %>%
                layout(xaxis = list(title = "Distance"), yaxis = list(title = "Std. Dev.") ) %>% colorbar(title = "Error")
            
            subplot(p1,p2,p3, nrows = 1, widths = c(0.25,0.25,0.5), titleX = TRUE, titleY = TRUE)
        }
    })


    output$neighsHeader <- renderText({
        if (!is.null(distsReactVals$click)) {
            return("Distances to all neighbors")
        }
        return("")
    })
    
    # Plot of k-nearest neighbors of selected instant and produced forecast
    output$neighborsPlot <- renderPlotly({
        # Update reactive values
        actK <- distsReactVals$actK
        actD <- distsReactVals$actD
        absErr <- distsReactVals$absErr
        x_click <- distsReactVals$click
        past <- distsReactVals$pastPred
        
        # This plot only shows if main Plot is clicked
        if (!is.null(x_click)) {
            # Plot selected element, with d observations
            pKNN <- plot_ly(type = "scatter",  mode = "lines+markers", showlegend = TRUE) %>% 
                add_trace(x = (-(actD - 1)):0, y = y[(x_click + 1 - actD):x_click], line = list(color = colPalette[1], width = 5), 
                          name = paste0("Observed (", dates[x_click], ")"), marker = list(color = colPalette[1], size = 7), hoverinfo = "text+y", 
                          legendgroup = "Observed", text = paste0("Observed, \n", format(dates[(x_click + 1 - actD):x_click], format = "%B %Y")) ) %>%
                layout(title = list(text = paste0(actK, "-nearest neighboors"), font = list(size = 17) ), 
                       xaxis = list(zerolinecolor = "black", zerolinewidth = 2) )
            
            # If clicked element is not the last one, also print it's next value
            if ( x_click < n ) {
                pKNN <- add_trace(pKNN, x = 0:1, y = y[x_click:(x_click + 1)], line = list(color = colPalette[1], width = 5, dash = "dash"), 
                                  name = "Observed", marker = list(color = colPalette[1], size = 7), hoverinfo = "text+y", legendgroup = "Observed",
                                  text = paste0("Observed,\n", format(dates[x_click:(x_click + 1)], format = "%B %Y")), showlegend = FALSE)
            }
            
            # Plot generated forecast witch actual K-D combination
            pKNN <- add_trace(pKNN, x = 1, y = past$mean, name = paste0("Prediction (", full_dates[(x_click + 1)], ")"), 
                        legendgroup = "Prediction", mode = "marker", marker = list(color = colPalette[2], size = 8),
                        hoverinfo = "text+y", text = paste0("Prediction for ", full_dates[(x_click + 1)]) )
            
            # To count how many neighbors have been plot
            ind <- 1
            
            # Set color intensity and transparency degradation as all neighbors are plotted
            redDecr   <- (250 - 150) / (actK - 1)
            transDecr <- (0.9 - 0.3) / (actK - 1)
            
            # If optimal K-D combination and in range of train+test, re-use data            
            if (actK == res$opt_k && actD == res$opt_d && x_click + 1 >= train_init && x_click < n) {
                # For each of k-neighbors, plot it's d-observations and next value
                for (i in optimal$neighbors[,(x_click + 1 - train_init)]) {
                    pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[(i + 1 - actD):i], name = paste0(ind,"-NN (", dates[(i)], ")" ), legendgroup = paste0(ind,"-NN"),
                                      line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4)),
                                      marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                      text = paste0(ind,"-nearest, \n", format(dates[(i + 1 - actD):i], format = "%B %Y")), hoverinfo = "text+y") %>%
                        add_trace(pKNN, x = 0:1, y = y[i:(i+1)], showlegend = FALSE, name = paste0(ind,"-NN (", dates[(i)], ")" ),legendgroup = paste0(ind,"-NN"),
                                  line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4), dash = "dash"),
                                  marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                  text = paste0(ind,"-nearest, \n", format(dates[(i):(i + 1)], format = "%B %Y")), hoverinfo = "text+y")
                    
                    ind <- ind + 1
                }
            }
            # Required data is not already calculated
            else {
                # Call forecast function to obtain k-neighbors of selected instant
                neighbors <- past$neighbors
                
                # For each of k-neighbors, plot it's d-observations and next value
                for (i in neighbors ) {
                    pKNN <- add_trace(pKNN, x = (-(actD - 1)):0, y = y[(i + 1 - actD):i], name = paste0(ind,"-NN (", dates[(i)], ")" ), legendgroup = paste0(ind,"-NN"),
                                      line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4)),
                                      marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                      text = paste0(ind,"-nearest, \n", format(dates[(i + 1 - actD):i], format = "%B %Y")), hoverinfo = "text+y") %>%
                        add_trace(pKNN, x = 0:1, y = y[(i):(i + 1)], showlegend = FALSE, name = paste0(ind,"-NN (", dates[(i)], ")" ),legendgroup = paste0(ind,"-NN"),
                                  line = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), width = ifelse(ind>actK/2, 3, 4), dash = "dash"),
                                  marker = list(color = paste0("rgba(", 250 - (ind-1)*redDecr, ",40,40," , 0.9 - (ind-1)*transDecr ), size = ifelse(ind>actK/2, 5, 6)),
                                  text = paste0(ind,"-nearest, \n", format(dates[(i):(i+1)], format = "%B %Y")), hoverinfo = "text+y")
                    
                    ind <- ind + 1
                }
            }
            pKNN
        }
        # There wasn't any click on main plot
        else {NULL}
    })
    

    observeEvent(input$buttDistsPlot, { distsReactVals$thresh <- input$selPerctabDist })


    # Plot of distances to all previous elements
    output$distsPlot <- renderPlotly({
        # Update reactive values
        actK <- distsReactVals$actK
        actD <- distsReactVals$actD
        absErr <- distsReactVals$absErr
        x_click <- distsReactVals$click
        percentile <- distsReactVals$thresh
        percentile <- ifelse(distsReactVals$thresh == "", 0.25, as.numeric(distsReactVals$thresh))
        
        past <- distsReactVals$pastPred
        
        # This plot only shows if main Plot is clicked
        if (!is.null(x_click)) {
            # Get all distances from selected element to all previous by calling Forecast function
            distances <- past$distances
            
            # Parse quaZ threshold of distances to plot
            index <- which(distances <= quantile(distances, probs = percentile))
            
            # Plot from dark color to light color 
            distColors <- c("darkcyan", "lightskyblue", "lightcyan")
            distColors <- colorRamp(distColors)
            # distColors <- rev(distColors) # REVERSE COLORS
            
            # RELATIVE TO MAX DISTANCE
            # distances <- (max(distances)*1.1) - distances
            
            # RELATIVE TO DISTANCE BETWEEN MIN AND MAX DISTANCES
            # distances <- 1 - ((distances-(min(distances))) / (max(distances) - min(distances)))
            
            # INVERSELY PROPORTIONAL TO MIN DISTANCE
            # distances <- min(distances) / distances
            
            # Calculate distances between selected element's next value and each other element
            sumDists <- as.vector(cdist(y[x_click+1], y[(actD+1):x_click], metric = distance))
            
            # Plot distances to previous elements
            pDists <<- plot_ly(x = dates[actD:x_click]) %>%
                add_trace(x = tail(head(dates, length(distances) + actD - 1), length(distances))[index], y = distances[index], 
                          name  = "Knn distances", showlegend = FALSE, hoverinfo = "x+y", type = "bar", color = distances[index], 
                          colors = distColors, yaxis = "y2") %>%
                add_trace(x = dates[actD:x_click], y = y[actD:x_click], name  = "Time series", showlegend = FALSE, yaxis = "y",
                          hoverinfo = "x+y", type = "scatter", mode = "lines", line = list(color = colPalette[1])) %>%
                layout(xaxis = list( range = list(dates[(actD)], dates[(actD + length(distances) - 1)]),
                          rangeslider = list( range = list(dates[ (actD)], dates[(actD + length(distances) - 1)]))),
                       yaxis = list(side = "left", range = c(min(y[actD:x_click]), max(y[actD:x_click])), overlaying = "y2"), yaxis2 = list( side = "right"))
            
            # Plot distances to previous element's next values
            pNeighs <- plot_ly(x = tail(head(dates, length(distances) + actD - 1), length(distances))[index], y = sumDists[index], 
                               name  = "Next-value distances", showlegend = FALSE, hoverinfo = "x+y", type = "bar")
            
                subplot(pDists, pNeighs, nrows = 2, shareX = TRUE)
        } 
        else {NULL}
    })


    # Plot of relationship between all distances to neighbors and distances to their next values 
    output$distScatPlot <- renderPlotly({
        # Update reactive values
        actK <- distsReactVals$actK
        actK <- ifelse(actK == "", res$opt_k, as.numeric(actK))
        actD <- distsReactVals$actD
        actD <- ifelse(actD == "", res$opt_d, as.numeric(actD))
        absErr <- distsReactVals$absErr
        x_click <- distsReactVals$click
        percentile <- distsReactVals$thresh
        percentile <- ifelse(distsReactVals$thresh == "", 0.25, as.numeric(distsReactVals$thresh))
        
        past <- distsReactVals$pastPred
        
        # This plot only shows if main Plot is clicked
        if (!is.null(x_click)) {
            # Get all distances from selected element to all previous by calling Forecast function
            distances <- past$distances
            
            # Parse quaZ threshold of distances to plot
            index <- which(distances <= quantile(distances, probs = percentile))
            
            # Calculate distances between selected element's next value and each other element
            sumDists <- as.vector(cdist(y[x_click+1], y[(actD+1):x_click], metric = distance))
            
            plot_ly(type = "scatter", mode = "markers", x = distances[index], y = sumDists[index]) %>%
                layout(xaxis = list(title = "Neighbors distances"), yaxis = list(title = "Next-value distances"))

        } 
        else {NULL}
    })


    #############################################################################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    ######################################    Tab 2   ###########################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    
    
    optimContReactVals <- reactiveValues(
        contourType = "default", 
        contMins = 5, 
        locMins = FALSE, 
        locRadi = 0, 
        numLocs = 0,
        localMins = NULL)
    
    getLocalMins <- function() {
        radius <- optimContReactVals$locRadi

        act <- 0
        delta <- seq(-radius, radius)
        selectKD <- matrix(ncol = 2, nrow = length(res$errors))
        
        for (c in 1:NCOL(res$errors)) for (r in 1:NROW(res$errors)) {
            # From actual position, get all nearby at given radius
            rows <- r + delta
            cols <- c + delta

            # If any position falls out of errors matrix, remove them
            rows <- rows[(rows > 0 & rows <= NROW(res$errors))]
            cols <- cols[(cols > 0 & cols <= NCOL(res$errors))]


            # If evaluated error is lower than surrounding, excluding NAs previously excluded by threshold, we take it
            if (all(res$errors[r,c] <= res$errors[rows, cols], na.rm = TRUE)) {
                act <- act + 1
                selectKD[act, 1] <- ks[r]
                selectKD[act, 2] <- ds[c]
            }
        }

        # Truncate size to number of minimums found
        selectKD <- selectKD[1:act, ]

        # Sort combinations by error
        sortered <- sort.int(res$errors[selectKD], decreasing = FALSE, index.return = TRUE)$ix

        selectKD <- selectKD[sortered,]

        return(selectKD)
    }
    
    observeEvent(input$buttContPlot, {
        optimContReactVals$contourType  <- input$contourType
        contMins <- input$contourMinims
        optimContReactVals$contMins  <- ifelse(contMins == "", 5, as.numeric(contMins))
        optimContReactVals$locMins <- input$chbLocalMins
        locRadi <- input$localMinRad
        locRadi <- ifelse(locRadi == "", 1, as.numeric(locRadi))

        if (input$chbLocalMins && locRadi > 0) {
            if ( optimContReactVals$locRadi != locRadi) {
                optimContReactVals$locRadi <- locRadi
                optimContReactVals$localMins <- getLocalMins()
            }
        }
        else 
            optimContReactVals$localMins <- NULL

        numLocs <- input$localMinDots
        optimContReactVals$numLocs <- ifelse(numLocs == "", 0, as.numeric(numLocs))
    })
    
    
    observeEvent(event_data("plotly_click", source = "contour", priority = "event"), {
        click <- event_data("plotly_click", source = "contour", priority = "event")
        if (!is.null(click)) {
            # Process indexes of click
            print("He detectado un click en contour")
            k <- click[[3]]
            d <- click[[4]]
            
            # Negate selection
            selected_points[k, d] <<- !selected_points[k, d]

            if ( !selected_points[k,d] ) {
                plotlyProxy("contourPlot", session) %>% plotlyProxyInvoke("deleteTraces", list(-1) )
                if (any(selected_points)) {
                    selected <- which(selected_points, arr.ind = TRUE)
                    if (NROW(selected) == 1) {
                        selected <- matrix(rep(selected, 2), ncol = 2, byrow = TRUE)
                    }
                    plotlyProxy("contourPlot", session) %>% plotlyProxyInvoke("addTraces",
                      list(x = selected[,1], y = selected[,2], type = 'scatter', mode = 'markers',
                           text = as.character(signif(res$errors[selected], digits = 4)), 
                           marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                           hoverinfo = "x+y+text", showlegend = FALSE, name = "manual"), list(-1) )
                }
            }
            else {
                selected <- which(selected_points, arr.ind = TRUE)
                if (NROW(selected) == 1) {
                    selected <- matrix(rep(selected, 2), ncol = 2, byrow = TRUE)
                    plotlyProxy("contourPlot", session) %>% plotlyProxyInvoke("addTraces",
                      list(x = selected[,1], y = selected[,2], type = 'scatter', mode = 'markers',
                           text = as.character(signif(res$errors[selected], digits = 4)), 
                           marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                           hoverinfo = "x+y+text", showlegend = FALSE, name = "manual"), list(-1) )
                }
                else {
                    plotlyProxy("contourPlot", session) %>% plotlyProxyInvoke("extendTraces",
                      list(text = list(list(as.character(signif(res$errors[k,d], digits = 4)))),
                           x = list(list(k)), y = list(list(d))) , list(-1) )
                }
            }
        }
    })
    
    # Plot of all tested k-d combinations and their errors
    output$contourPlot <- renderPlotly({
        # Update reactive values
        actType <- optimContReactVals$contourType

        nDots <- optimContReactVals$contMins

        radius <- optimContReactVals$locRadi

        maximum <- optimContReactVals$numLocs
        
        selectKD <- optimContReactVals$localMins


        # Check actual contour mode
        if (actType == "trim") {
            pContour <<- pContourTrim
        }
        else if (actType == "naive") {
            pContour <<- pContourNaive
        }
        else {
            pContour <<- pContourBase
        }

        # If more than 1 point is selected, add all of them to the plot
        if (nDots > 1 ) {
            # Generate as many texts as selected dots minus one
            texts <- c("2nd best \n")
            if (nDots > 2 ) {
                texts <- c(texts, "3rd best \n")
                if (nDots > 3 ) {
                    texts <- c(texts, paste0( 4:nDots, rep("th best \n", nDots - 3) ))
                }
            }

            # Generate as many colors as selected dots minus one
            colors <- paste0( "rgba(40," , 230 - (0:(nDots-1)) * ((230 - 128) / (nDots - 1)), " ,40, 0.95)" )

            coords <- matrix(c(x_minims[2:nDots], y_minims[2:nDots]), ncol = 2)

            # Plot all dots corresponding to selected minimums
            pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = coords[,1], y = coords[,2],
                                   text = paste0(texts, as.character( signif(res$errors[coords], digits = 8)) ),
                                   # marker = list(color = 0:(nDots-2), colorscale = c("seagreen", "chartreuse")), size = 8,
                                   marker = list(color = colors, size = 8),
                                   hoverinfo = "x+y+text", showlegend = FALSE, opacity = 1)
        }

        if (optimContReactVals$locMins) {
            if (maximum != 0 && NROW(selectKD) > maximum) {
                selectKD <- head(selectKD, maximum)
            }

            pContour <<- add_trace(pContour, type = "scatter", mode = "markers", x = selectKD[, 1], y = selectKD[, 2], 
                                   text = as.character(signif(res$errors[selectKD], digits = 4)), hoverinfo = "x+y+text", showlegend = FALSE,
                                   marker = list(color = "black", line = list(color = "white",  width = 2)))
        }
        
        # If any point is selected manually, plot all of them
        if (any(selected_points)) {
            selected <- which(selected_points, arr.ind = TRUE)
            pContour <<- add_trace(pContour, x = selected[,1], y = selected[,2], type = 'scatter', showlegend = FALSE, mode = 'markers',
                                   text = as.character(signif(res$errors[selected], digits = 4)), name = "manual", hoverinfo = "x+y+text",
                                   marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)",  width = 3)))
        }
        pContour
    })

    
    optimSeriesReactVals <- reactiveValues(
        naive = FALSE,
        seaNaive = FALSE,
        sNaivLag = 12,
        absErr = TRUE,
        custSer = FALSE,
        path = ""
    )

    observeEvent(input$buttOptPlot, {
        optimSeriesReactVals$naive <- input$chbNaiveOpt
        optimSeriesReactVals$seaNaive <- input$chbSeasNaiveOpt
        optimSeriesReactVals$sNaivLag <- input$seasNaivOptLag
        optimSeriesReactVals$absErr <- input$chbabs_tab2
        optimSeriesReactVals$custSer <- input$chbload
        optimSeriesReactVals$path <- input$path
    })

    observeEvent(input$browse, {updateTextInput(session, "path",  value = file.choose()) })

    # Plot of comparison between forecast generated by best k-d combination and other methods
    output$optPlot <- renderPlotly({
        # Update reactive values
        activNaive <- optimSeriesReactVals$naive
        activSeasNaiv <- optimSeriesReactVals$seaNaive
        seasLag <- as.numeric(optimSeriesReactVals$sNaivLag)
        absErr <- optimSeriesReactVals$absErr
        activCust <- optimSeriesReactVals$custSer
        custPath <- optimSeriesReactVals$path
        
        # If only best K-D combination is plotted, re-use information
        if ( all( selected_points == FALSE ) && activNaive == 0 && activSeasNaiv == 0 && activCust == 0) {
            pErrorsOpt <<- pBarsOptBase
            # Whether absolute error is chosen or not
            if (absErr) { 
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            # Line where train set begins
            pErrorsOpt <- add_segments(pErrorsOpt, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            # Line where test set begins
            pErrorsOpt <- add_segments(pErrorsOpt, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            combPlotOpt <<- subplot(pOptBase, pErrorsOpt, nrows = 2, shareX = TRUE)
        }
        else {
            # Initialize plots
            pOpt <<- pOptBase
            pErrorsOpt <<- pLinesBaseOpt
            
            # Whether absolute error is chosen or not
            if (absErr) { 
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            # Count how many series will be plotted
            totalSeries <- sum(selected_points)
            if (activNaive) {
                totalSeries <- totalSeries + 1
            }
            if (activSeasNaiv) {
                totalSeries <- totalSeries + 1
            }
            if (activCust) {
                totalSeries <- totalSeries + 1
            }
            
            # Depending on the number of series the type of plot would be Bars or Lines
            if (totalSeries > 1) {
                pComparOptim <<- plot_ly(type = "scatter", mode = "lines" , hoverinfo = "x+y" ) # %>% layout(title = "Errors comparison")
            }
            else {
                pComparOptim <<- plot_ly(type = "bar", hoverinfo = "x+y" ) # %>% layout(title = "Errors comparison")
            }
            
            # Initialize minimums and maximums
            min_compar <- Inf
            max_compar <- -Inf
            colorIndex <- 5
            
            # Naive activated with checkbox
            if (activNaive) {
                # Add Naive forecast to plot and it's errors
                pOpt <<- add_trace(pOpt, x = sub_dates, y = naive, name = "Naive", legendgroup = "naive", line = list(color = colPalette[3]))
                if (absErr) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[2, ]), line = list(color = colPalette[3]),
                                             name = "Naive error", legendgroup = "naive", showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[2, ], line = list(color = colPalette[3]),
                                             name = "Naive error", legendgroup = "naive", showlegend = FALSE)
                }
                
                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[2, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[3]),
                                               name = "Naive comparison", legendgroup = "naive", showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[3]), 
                                               name = "Naive comparison", legendgroup = "naive", showlegend = FALSE)
                }
                
                # Update min and max
                min_compar <- min( min_compar, dif)
                max_compar <- max( max_compar, dif)
            }
            
            # Seasonal naive activated with checkbox
            if (activSeasNaiv) {
                # Generate seasonal forecast and calculate error
                isolate({
                    snaive <- ts(y[(train_init - seasLag + 1):(n - seasLag)])
                })
                residuals_matrix[3, ] <- y_err - snaive
                
                # Add Seasonal Naive forecast to plot and it's errors
                pOpt <<- add_trace(pOpt, x = sub_dates, y = snaive, name = paste0("S. Naive (", seasLag, ")"), legendgroup = "snaive", line = list(color = colPalette[4]))
                if (absErr) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[3, ]), line = list(color = colPalette[4]),
                                             name = "S. Naive error", legendgroup = "snaive", showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[3, ], line = list(color = colPalette[4]),
                                             name = "S. Naive error", legendgroup = "snaive", showlegend = FALSE)
                }
                
                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[3, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[4]),
                                               name = "S. Naive comparison", legendgroup = "snaive", showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[4]), 
                                               name = "S. Naive comparison", legendgroup = "snaive", showlegend = FALSE)
                }
                
                # Update min and max
                min_compar <- min(min_compar, dif)
                max_compar <- max(max_compar, dif)
            }
            
            # Load custom data activated with checkbox
            if (activCust) {
                isolate({
                    new_ts <- readRDS(custPath)
                    name <- basename(custPath)
                })
                pOpt <- add_trace(pOpt, x = sub_dates, y = new_ts, name = name, legendgroup = name)
                
                residuals_matrix[5, ] <- y_err - new_ts

                if (absErr) {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(residuals_matrix[5, ]), line = list(color = colPalette[5]),
                                             name = paste(name, "error"), legendgroup = name, showlegend = FALSE)
                }
                else {
                    pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = residuals_matrix[5, ], line = list(color = colPalette[5]),
                                             name = paste(name, "error"), legendgroup = name, showlegend = FALSE)
                }

                dif <- abs(residuals_matrix[1, ]) - abs(residuals_matrix[5, ])
                if (totalSeries > 1) {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[5]),
                                               name = paste(name, "comparison"), legendgroup = name, showlegend = FALSE)
                }
                else {
                    pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[5]), 
                                               name = paste(name, "comparison"), legendgroup = name, showlegend = FALSE)
                }

                # Update min and max
                min_compar <- min( min_compar, dif)
                max_compar <- max( max_compar, dif)
            }
            
            # Plot all k-d combinations selected
            if (any( selected_points ) ) {
                for (i_ind in 1:NROW(selected_points)) for (j_ind in 1:NCOL(selected_points)) 
                    if (selected_points[i_ind, j_ind]) {
                        i <- ks[i_ind]
                        j <- ds[j_ind]
                        
                        # Generate corresponding forecast
                        newPred <- knn_past(y = y, k = i, d = j, initial = train_init, distance = distance, 
                                            weight = weight, threads = n_threads)$fitted
                        newPred <- as.vector(newPred)
                        
                        # Add forecast to series Plot
                        pOpt <<- add_trace(pOpt, x = sub_dates, y = newPred, name = paste0("k = " , i, ", d = " , j, " prediction"), 
                                           legendgroup = paste("k", i, "d", j), line = list(color = colPalette[colorIndex]))
                        
                        # It's error to the plot
                        error <- y_err - newPred
                        if (absErr) {
                            pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = abs(error), line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                     name = paste0("k = " , i, ", d = " , j, "", " error"), legendgroup = paste("k", i, "d", j))
                            mini <- min( mini, abs(error))
                            maxi <- max( maxi, abs(error))
                        }
                        else {
                            pErrorsOpt <<- add_trace(pErrorsOpt, x = sub_dates, y = error, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                     name = paste0("k = " , i, ", d = " , j, "", " error"), legendgroup = paste("k", i, "d", j))
                            mini <- min( mini, error)
                            maxi <- max( maxi, error)
                        }
                        
                        # And to the third plot of errors comparison
                        dif <- abs(residuals_matrix[1, ]) - abs(error)
                        if (totalSeries > 1) {
                            pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                       name = paste0("k = " , i, ", d = " , j, "", " comparison"), legendgroup = paste("k", i, "d", j))
                        }
                        else {
                            pComparOptim <<- add_trace(pComparOptim, x = sub_dates, y = dif, marker = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                                       name = paste0("k = " , i, ", d = " , j, "", " comparison"), legendgroup = paste("k", i, "d", j))
                        }
                        
                        # Update min and max
                        min_compar <- min( min_compar, dif)
                        max_compar <- max( max_compar, dif)
                        colorIndex <- colorIndex + 1
                    }
            }
            
            # Add lines of Train and Test sets to both plots
            pErrorsOpt <<- add_segments(pErrorsOpt, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                        yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                        hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pErrorsOpt <<- add_segments(pErrorsOpt, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                        yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                        hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            pComparOptim <<- add_segments(pComparOptim, x = dates[train_init], xend = dates[train_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                          yend = max_compar + 0.05 * (max_compar - min_compar), name = "Train", showlegend = FALSE, text = "Train", 
                                          hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pComparOptim <<- add_segments(pComparOptim, x = dates[test_init], xend = dates[test_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                          yend = max_compar + 0.05 * (max_compar - min_compar), name = "Test", showlegend = FALSE, text = "Test", 
                                          hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            
            combPlotOpt <<- subplot(pOpt, pErrorsOpt, pComparOptim, nrows = 3, shareX = TRUE )
        }
        
        # output$optPlot <- renderPlotly({
        return(combPlotOpt)
    })

    # Table of errors
    output$table_OptimTab <- renderDataTable({
        # Update reactive values
        activNaive <- optimSeriesReactVals$naive
        activSeasNaiv <- optimSeriesReactVals$seaNaive
        seasLag <- as.numeric(optimSeriesReactVals$sNaivLag)
        activCust <- optimSeriesReactVals$custSer
        custPath <- optimSeriesReactVals$path

        # Obtain names and errors matrix
        names_col_local <- c(names_col[1])
        errors_matrix_local <- matrix(errors_matrix[1, ], nrow = 1)
        
        # Naive activated with checkbox
        if (activNaive) {
            names_col_local <- c(names_col_local, names_col[2])
            errors_matrix_local <- rbind(errors_matrix_local, errors_matrix[2, ])
        }
        
        # Seasonal naive activated with checkbox
        if (activSeasNaiv) {
            names_col_local <- c(names_col_local, paste0(names_col[3], " (", seasLag, ")"))
            isolate({
                snaive <- ts(y[(train_init - seasLag + 1):(n - seasLag)])
            })
            train_error <- forecast::accuracy(snaive[1:length(y_train_err)], y_train_err)
            test_error <- forecast::accuracy(snaive[(length(y_train_err) + 1):length(snaive)], y_test_err)
            errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
        }
        
        # Plot all k-d combinations selected
        if (any( selected_points ) ) {
            for (i_ind in 1:NROW(selected_points)) for (j_ind in 1:NCOL(selected_points))
                if (selected_points[i_ind, j_ind]) {
                    i <- ks[i_ind]
                    j <- ds[j_ind]
                    # Add it's name
                    names_col_local <- c(names_col_local, paste0("k = " , i, ", d = " , j))
                    # Generate forecast
                    newPred <- knn_past(y = y, k = i, d = j, initial = train_init, distance = distance, 
                                        weight = weight, threads = n_threads)$fitted
                    newPred <- as.vector(newPred)
                    
                    # Measure forecast errors
                    train_error <- forecast::accuracy(newPred[1:length(y_train_err)], y_train_err)
                    test_error <- forecast::accuracy(newPred[(length(y_train_err) + 1):length(newPred)], y_test_err)
                    
                    errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
                }
        }
        # Generate associated datatable
        DT::datatable(data.frame(
            Name = names_col_local, 
            trainME = signif(errors_matrix_local[, 1], digits = 8), 
            trainRMSE = signif(errors_matrix_local[, 2], 8),
            trainMAE = signif(errors_matrix_local[, 3], digits = 8), 
            testME = signif(errors_matrix_local[, 6], digits = 8),
            testRMSE = signif(errors_matrix_local[, 7], 8), 
            testMAE = signif(errors_matrix_local[, 8], digits = 8)
        ), colnames = c("Name", "ME (train)", "RMSE (train)", "MAE (train)", 
                        "ME (test)", "RMSE (test)", "MAE (test)"))        
    })



    #############################################################################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    ######################################    Tab 3   ###########################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    #############################################################################################
    
    
    
    combForeReactVals <- reactiveValues(
        forceRepaint = FALSE,
        absErr = TRUE,
        weights = FALSE,
        selected = NULL,
        source = "manual",
        lastSource = "manual",
        RDSlearners = NULL)
    
    actCombPredict <- reactive({
        print("Actualizando valor de la prediccion")
        # selected <- which(combinations, arr.ind = TRUE)
        selected <- combForeReactVals$selected
        
        # Check if any combination has been selected
        if ( NROW(selected) == 0 ) {
            return(NULL)
        }
        
        # Update reactive values
        absErr <- combForeReactVals$absErr
        
        learners <- list()
        learners$chosK <- selected[, 1]
        learners$chosD <- selected[, 2]
        
        # Check source of combination
        if (combForeReactVals$source == "manual") {
            # Check selected weighting method
            if (combForeReactVals$weights == "proportional") {
                learners$weights <- 1/res$errors[selected]
            }
            else if (combForeReactVals$weights == "linear") {
                ord <- sort.int(res$errors[selected], index.return = TRUE)$ix
                learners$chosK <- learners$chosK[ord]
                learners$chosD <- learners$chosD[ord]
                learners$weights <- length(learners$chosK):1
            }
            else {
                learners$weights <- rep(x = 1, length(learners$chosK))
            }
        }
        else
            learners$weights <- combForeReactVals$weights

        # Generate corresponding forecast
        print("Generando prediccion con: ")
        print(learners)
        
        rn_forecast(y = y, learners = learners, initial = train_init, distance = distance, weight = weight)
    })
    
    observeEvent(input$buttCombPlot, {
        combForeReactVals$absErr <- input$chbabs_Comb
        combForeReactVals$selected <- which(combinations, arr.ind = TRUE)
        print(paste0("La ultima fuente guardada ha sido: ", combForeReactVals$lastSource))
        if (combForeReactVals$lastSource == "file") {
            learnersMatrix <- matrix(data = FALSE, nrow = NROW(combinations), ncol = NCOL(combinations))
            learnersKD <- matrix( c(combForeReactVals$RDSlearners$chosK, combForeReactVals$RDSlearners$chosD), ncol = 2)
            learnersMatrix[learnersKD] <- TRUE
            if (all(combinations == learnersMatrix)) {
                print("Usando pesos de fichero")
                combForeReactVals$selected <- learnersKD
                combForeReactVals$weights <- combForeReactVals$RDSlearners$weights
                combForeReactVals$source <- "file"
            }
            else{
                print(paste0("Se ha modificado la seleccion. Usando opcion: ", input$learnWeights))
                combForeReactVals$weights <- input$learnWeights
                combForeReactVals$source <- "manual"
            }
        }
        else {
            combForeReactVals$weights <- input$learnWeights
            combForeReactVals$source <- "manual"
        }
    })
    
    observeEvent(input$buttClearComb, {
        if (any(combinations)) {
            combinations[ , ] <<- FALSE
            plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("deleteTraces", list(-1) )
        }
    })
    
    observeEvent(input$buttFromRDS, {
        print("Cambiando fuente guardada a file")
        newCombin <- readRDS(input$pathRDS)
        selected <- matrix( c(newCombin$chosK, newCombin$chosD), ncol = 2)
        anySelect <- any(combinations)
        combinations[ , ] <<- FALSE
        combinations[selected] <<- TRUE
        combForeReactVals$RDSlearners <- newCombin
        combForeReactVals$lastSource <- "file"
        
        if (NROW(selected) == 1) {
            selected <- matrix(rep(selected, 2), ncol = 2, byrow = TRUE)
        }
        if (anySelect) {
            plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("deleteTraces", list(-1) ) %>%
                plotlyProxyInvoke("addTraces", list(x = selected[,1], y = selected[,2], type = 'scatter',
                    mode = 'markers', text = as.character(signif(res$errors[selected], digits = 4)),
                    marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                    hoverinfo = "x+y+text", showlegend = FALSE, name = "manual"), list(-1))
        }
        else {
            plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("addTraces",
                list(x = selected[,1], y = selected[,2], type = 'scatter', mode = 'markers',
                   text = as.character(signif(res$errors[selected], digits = 4)),
                   marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                   hoverinfo = "x+y+text", showlegend = FALSE, name = "manual"), list(-1))
        }
    })
    
    observeEvent(input$browseRDS, {updateTextInput(session, "pathRDS",  value = file.choose())})
    
    output$combinationPlot <- renderPlotly({
        if ( any(combinations)) {
            print("Se va a repintar el gráfico de combinaciones con el force")
            selected <- which(combinations, arr.ind = TRUE)
            print("y los seleccionados son: ")
            print(selected)
            resulPlot <- pManualComb %>% add_trace(x = selected[,1], y = selected[,2], type = "scatter", mode = "markers",
                            text = as.character(signif(res$errors[selected], digits = 4)), 
                            marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                            hoverinfo = "x+y+text", showlegend = FALSE)
        }
        else {
            print("Se va a repintar el gráfico de combinaciones sin nada")
            resulPlot <- pManualComb
        }
        resulPlot
    })
    
    observeEvent(event_data("plotly_click", source = "manual", priority = "event"), {
        click <- event_data("plotly_click", source = "manual", priority = "event")
        if (!is.null(click)) {
            # Process indexes of click
            k <- click[[3]]
            d <- click[[4]]
            print(paste0("He detectado un click en manual en: ", k, " , ", d))

            combForeReactVals$lastSource <- "manual"
            updateTabsetPanel(session, inputId = "combSource", selected = "manual")

            # Negate selection
            combinations[k, d] <<- !combinations[k, d]

            if ( !combinations[k,d] ) {
                if (any(combinations)) {
                    selected <- which(combinations, arr.ind = TRUE)
                    if (NROW(selected) == 1) {
                        selected <- matrix(rep(selected, 2), ncol = 2, byrow = TRUE)
                    }
                    plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("deleteTraces", list(-1) ) %>%
                    plotlyProxyInvoke("addTraces",
                          list(x = selected[,1], y = selected[,2], type = 'scatter', mode = 'markers',
                               text = as.character(signif(res$errors[selected], digits = 4)),
                               marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                               hoverinfo = "x+y+text", showlegend = FALSE, name = "manual"), list(-1))
                }
                else{
                    plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("deleteTraces", list(-1) )
                }
            }
            else {
                selected <- which(combinations, arr.ind = TRUE)
                if (NROW(selected) == 1) {
                    selected <- matrix(rep(selected, 2), ncol = 2, byrow = TRUE)
                    plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("addTraces",
                          list(x = selected[,1], y = selected[,2], type = 'scatter', mode = 'markers',
                               text = as.character(signif(res$errors[selected], digits = 4)), 
                               marker = list(color = "red", size = 8, line = list(color = "rgba(0,0,0,0.7)", width = 3)),
                               hoverinfo = "x+y+text", showlegend = FALSE), list(1) )
                }
                else {
                    plotlyProxy("combinationPlot", session) %>% plotlyProxyInvoke("extendTraces",
                      list(text = list(list(as.character(signif(res$errors[k,d], digits = 4)))),
                           x = list(list(k)), y = list(list(d))), list(-1)) 
                }
            }
        }
    })


    output$combForePlot <- renderPlotly({
        # Update reactive values
        absErr <- combForeReactVals$absErr
        
        combModel <- actCombPredict()
        
        # If only best K-D combination is plotted, re-use information
        if ( is.null(combModel) ) {
            pErrorsComb <- plot_ly(type = "bar", hoverinfo = "x+y")
            # Whether absolute error is chosen or not
            if (absErr) { 
                pErrorsComb <- add_trace(pErrorsComb, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsComb <- add_trace(pErrorsComb, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", marker = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }
            
            # Line where train set begins
            pErrorsComb <- add_segments(pErrorsComb, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            # Line where test set begins
            pErrorsComb <- add_segments(pErrorsComb, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                       yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                       hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            combPlot <- subplot(pOptBase, pErrorsComb, nrows = 2, shareX = TRUE)
        }
        else {
            # Initialize plots
            pCombs <- pOptBase
            pErrorsComb <- pLinesBaseOpt
            pComparComb <- plot_ly(type = "bar", hoverinfo = "x+y" )
            
            # Whether absolute error is chosen or not
            if (absErr) { 
                pErrorsComb <- add_trace(pErrorsComb, x = sub_dates, y = abs(residuals_matrix[1, ]), showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( abs(residuals_matrix[1, ]) )
                maxi <- max( abs(residuals_matrix[1, ]) )
            }
            else {
                pErrorsComb <- add_trace(pErrorsComb, x = sub_dates, y = residuals_matrix[1, ], showlegend = FALSE,
                                         name = "Optimal Error", legendgroup = "optim", line = list(color = colPalette[2]))
                mini <- min( residuals_matrix[1, ])
                maxi <- max( residuals_matrix[1, ])
            }

            newPred <- as.vector(combModel$prediction)

            colorIndex <- 5

            # Add forecast to series Plot
            pCombs <- add_trace(pCombs, x = sub_dates, y = newPred, name = "Comb. fore. predic.", 
                               legendgroup = "Comb. fore.", line = list(color = colPalette[colorIndex]))
            
            # It's error to the plot
            error <- combModel$residu
            if (absErr) {
                pErrorsComb <- add_trace(pErrorsComb, x = sub_dates, y = abs(error), line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                         name = "Comb. fore. error", legendgroup = "Comb. fore.")
                mini <- min(abs(error))
                maxi <- max(abs(error))
            }
            else {
                pErrorsComb <- add_trace(pErrorsComb, x = sub_dates, y = error, line = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                         name = "Comb. fore. error", legendgroup = "Comb. fore.")
                mini <- min(error)
                maxi <- max(error)
            }
            
            # And to the third plot of errors comparison
            dif <- abs(residuals_matrix[1, ]) - abs(error)
            pComparComb <- add_trace(pComparComb, x = sub_dates, y = dif, marker = list(color = colPalette[colorIndex]), showlegend = FALSE,
                                           name = paste0("Comb. fore. compar."), legendgroup = paste("Comb. fore."))
            
            # Update min and max
            min_compar <- min(dif)
            max_compar <- max(dif)
            colorIndex <- colorIndex + 1
            
            # Add lines of Train and Test sets to both plots
            pErrorsComb <- add_segments(pErrorsComb, x = dates[train_init], xend = dates[train_init], y = mini - 0.05 * (maxi - mini), 
                                        yend = maxi + 0.05 * (maxi - mini), name = "Train", showlegend = FALSE, text = "Train", 
                                        hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pErrorsComb <- add_segments(pErrorsComb, x = dates[test_init], xend = dates[test_init], y = mini - 0.05 * (maxi - mini), 
                                        yend = maxi + 0.05 * (maxi - mini), name = "Test", showlegend = FALSE, text = "Test", 
                                        hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            pComparComb <- add_segments(pComparComb, x = dates[train_init], xend = dates[train_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                          yend = max_compar + 0.05 * (max_compar - min_compar), name = "Train", showlegend = FALSE, text = "Train", 
                                          hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            pComparComb <- add_segments(pComparComb, x = dates[test_init], xend = dates[test_init], y = min_compar - 0.05 * (max_compar - min_compar), 
                                          yend = max_compar + 0.05 * (max_compar - min_compar), name = "Test", showlegend = FALSE, text = "Test", 
                                          hoverinfo = "text", legendgroup = "lines", line = list(color = "gray", width = 1.5, dash = "dash"))
            
            
            combPlot <- subplot(pCombs, pErrorsComb, pComparComb, nrows = 3, shareX = TRUE )
        }

        return(combPlot)
    })
    
    output$table_CombinTab <- renderDT({
        # # Update reactive values
        combModel <- actCombPredict()
        
        # Obtain names and errors matrix
        names_col_local <- c(names_col[1])
        errors_matrix_local <- matrix(errors_matrix[1, ], nrow = 1)
        
        if ( !is.null(combModel) ) {
            newPred <- as.vector(combModel$prediction)
            
            names_col_local <- c(names_col_local, "Combinated kNNs")
            
            # Measure forecast errors
            train_error <- forecast::accuracy((newPred[1:length(y_train_err)]), y_train_err)
            test_error <- forecast::accuracy((newPred[(length(y_train_err) + 1):length(newPred)]), y_test_err)
            
            errors_matrix_local <- rbind(errors_matrix_local, c(train_error, test_error))
        }
        
        # Generate associated datatable
        DT::datatable(data.frame(
            Name = names_col_local, 
            trainME = signif(errors_matrix_local[, 1], digits = 8), 
            trainMAE = signif(errors_matrix_local[, 3], digits = 8), 
            trainRMSE = signif(errors_matrix_local[, 2], digits = 8),
            testME = signif(errors_matrix_local[, 6], digits = 8),
            testMAE = signif(errors_matrix_local[, 8], digits = 8), 
            testRMSE = signif(errors_matrix_local[, 7], digits = 8)
        ), colnames = c("Name", "ME (train)", "MAE (train)", "RMSE (train)", 
                        "ME (test)", "MAE (test)", "RMSE (test)")) 
    })

} 

ui <- navbarPage("", selected = "Distances",
        tabPanel("Distances",
                headerPanel("Forecast accuracy and kNN internal measures"),
                mainPanel(width = 11,
                    plotlyOutput("elemsPlot", width = "100%", height = "600px")
                ),
                sidebarPanel(width = 1,
                    tags$head(
                        tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}"))
                    ),
                    h4(strong("Parameters")),
                    br(),
                    numericInput(inputId = "selKtabDist", label = "K", value = res$opt_k, 
                              width = NULL, min = 1),
                    numericInput(inputId = "selDtabDist", label = "D", value = res$opt_d, 
                              width = NULL, min = 1),
                    hr(),
                    materialSwitch(inputId = "chbabsDist", label = "Absolute Error", value = TRUE, status = "primary")
                    ,hr()
                    ,actionButton(inputId = "buttSeriesPlot", "Update plots")
                ),
                
                mainPanel(width = 12,
                    h3("Loess regressions"),
                    plotlyOutput("scattDistErr", height = "500px") ),
                mainPanel(width = 12,
                    h3(textOutput(outputId = "neighsHeader")),
                    plotlyOutput("neighborsPlot", height = "440px")
                )
        ),
            
            #############################################################################################
            #############################################################################################
            #############################################################################################
            #############################################################################################
            ####################################### TAB 2 ###############################################
            #############################################################################################
            #############################################################################################
            #############################################################################################
            #############################################################################################
            #############################################################################################
            
            
            
        tabPanel("Optimization",
            headerPanel(HTML(paste0("Error surface for each <em>K</em>-<em>D</em> combination and optimal one (", error_measure, " error)"))),
            mainPanel( width = 10,
                helpText("Click to add/erase other kNN predictions to compare"),
                plotlyOutput("contourPlot", height = "600px")
            ),
            sidebarPanel(width = 2,
                h4(strong("Contour plot customization"), align = "center"),
                br(),
                radioButtons(inputId = "contourType", label = "Type of contour", selected = "default",
                    choices = list("Default" = "default", "Contour lines under Naive" = "naive",
                       "Top-values color trimmed" = "trim")),
                hr(),
                numericInput(inputId = "contourMinims", label = "Number of absolute minimums to plot:",
                    value = 5, min = 0),
                hr(),
                checkboxInput("chbLocalMins", label = "Local Minimums", value = FALSE),
                numericInput(inputId = "localMinRad", label = "Local radius:", value = 1, min = 1, width = "100%"),
                numericInput(inputId = "localMinDots", label = "Max. dots:", value = 0, min = 0, width = "100%"),
                hr(),
                actionButton(inputId = "buttContPlot", label = "Update contour plot")
            ),
            mainPanel( width = 10,
                h3("Forecasts comparison", align = "left"),
                plotlyOutput("optPlot", height = "480px") 
            ),
            sidebarPanel( width = 2, 
                tags$head(tags$style(HTML("hr {border-top: 1px solid #cbcbcb;}")) ),
                h4(strong("Forecasts customization"), align = "center"),
                hr(),
                checkboxInput("chbNaiveOpt", label = "Naive", value = FALSE),
                hr(),
                checkboxInput("chbSeasNaiveOpt", label = "Seasonal Naive", value = FALSE), 
                numericInput(inputId = "seasNaivOptLag", label = "Lag:", value = 12, min = 1),
                hr(),
                checkboxInput("chbload", label = "Custom", value = FALSE),
                textInput("path", "File:", placeholder = "Absolute path to the file"),
                actionButton("browse", "Browse"),
                hr(),
                materialSwitch(inputId = "chbabs_tab2", label = "Absolute Error", value = TRUE, status = "primary"),
                hr(),
                actionButton("buttOptPlot", "Update predict plot")
            ),
            headerPanel("Errors Table"),
            sidebarPanel(width = 11,
                DT::dataTableOutput("table_OptimTab")
            )
        ),
            
        #############################################################################################
        #############################################################################################
        #############################################################################################
        #############################################################################################
        ####################################### TAB 3 ###############################################
        #############################################################################################
        #############################################################################################
        #############################################################################################
        #############################################################################################
        #############################################################################################
            
            
        tabPanel("Combinations",
            headerPanel("Combinated forecast of multiple kNN forecast"),
            mainPanel( width = 10,
                h3(HTML(paste0("Individual errors for each <em>k</em> and <em>d</em> combination"))),
                helpText("Click to add other kNN forecasts to combinated forecast"),
                plotlyOutput("combinationPlot", height = "450px")
            ),
            sidebarPanel(width = 2,
                h4(strong("Combinated forecast customization")),
                tabsetPanel(id = "combSource", type = "tabs",
                    tabPanel(value = "manual", title = "Manual",
                        br(),
                        radioButtons(inputId = "learnWeights", label = "Prediction weight of each learner:",
                             selected = "average", choices = list("Average: all equal" = "average",
                                "Proportional to minor error" = "proportional", "Linear from K to 1" = "linear"))
                    ),
                    tabPanel(value = "file", title = "File",
                        br(),
                        textInput("pathRDS", "File:", placeholder = "Absolute path to the file"),
                        actionButton("browseRDS", "Browse"),
                        actionButton(inputId = "buttFromRDS", label = "Set")
                    )
                ),
                hr(),
                materialSwitch(inputId = "chbabs_Comb", label = "Absolute predictions error", value = TRUE, status = "primary"),
                hr(),
                actionButton(inputId = "buttClearComb", label = "Clear selection"),
                actionButton(inputId = "buttCombPlot", label = "Confirm selection")
            ),
            mainPanel(width = 10,
                h3("Time Series and combinated forecast"),
                plotlyOutput("combForePlot", height = "480px") 
            ),
            headerPanel("Errors Table"),
            sidebarPanel(width = 11, DT::DTOutput(outputId = "table_CombinTab") )
        )
    )

# Now run the following: 
runApp(shinyApp(server = server, ui = ui, options = list("quiet" = TRUE)))
