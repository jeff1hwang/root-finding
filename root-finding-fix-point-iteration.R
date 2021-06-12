#######################
## Root Finding with fixed point iteration
## Copyright (c) Jeff Hwang
#######################

library(ggplot2)
fixedpoint_show <- function(ftn, x0, iter = 5){
  # applies fixed-point method to find x such that ftn(x) = x
  # ftn is a user-defined function
  # df_points_1 and df_points_2 are used to track each update
  # it will be used to plot the line segments showing each update
  # each line segment connects the points (x1, y1) to (x2, y2)
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0))
  df_points_2 <- df_points_1
  xnew <- x0
  cat("Starting value is:", xnew, "\n")
  
  # iterate the fixed point algorithm
  for (i in 1:iter) {
    xold <- xnew
    xnew <- ftn(xold)
    cat("Next value of x is:", xnew, "\n")
    # vertical line segments, where x1 = x2
    df_points_1[i, ] <- c(x1 = xold, y1 = xold, x2 = xold, y2 = xnew)
    # horizontal line segments, where y1 = y2
    df_points_2[i, ] <- c(x1 = xold, y1 = xnew, x2 = xnew, y2 = xnew)
  }
  # use ggplot to plot the function and the segments for each iteration
  # determine the limits to use for the plot
  # start is the min of these values. we subtract .1 to provide a small margin
  plot_start <- min(df_points_1$x1, df_points_1$x2, x0) - 0.1  
  # end is the max of these values
  plot_end <- max(df_points_1$x1, df_points_1$x2, x0) + 0.1
  
  # calculate the value of the funtion fx for all x
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) {
    fx[i] <- ftn(x[i])
  }
  function_data <- data.frame(x, fx) # data frame containing the function values
  
  p <- ggplot(function_data, aes(x = x, y = fx)) + 
    geom_line(color = "royalblue", size = 1) +  # plot the function
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), 
                 data = df_points_1, color = "black", lty = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2), 
                 data = df_points_2, color = "red", lty = 2) +
    geom_abline(intercept = 0, slope = 1) + # plot the line y = x
    coord_equal() + theme_bw()
  
  print(p) # produce the plot
  xnew # value that gets returned
}

##### Function 1
f <- function(x) cos(x)

## using x0 = 1
fixedpoint_show(f, 1, iter= 10)

## using x0 = 3
fixedpoint_show(f, x0 = 3, iter = 10)

## Function 2
f1 <- function(x){
  exp(exp(-x))
}
## using x0 = 2
fixedpoint_show(f1, x0 = 2, iter = 10)

## Function 3
f2 <- function(x) {
  x - log(x) + exp(-x)
}
## using x0 = 2
fixedpoint_show(f2, x0 = 2, iter = 5)

