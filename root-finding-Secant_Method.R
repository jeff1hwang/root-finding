#######################
## Root Finding with Secant Method
## Copyright (c) Jeff Hwang
#######################

secant_show <- function(ftn, x0, x1, iter = 5) {
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0))
  df_points_2 <- df_points_1
  df_points_3 <- df_points_1
  
  
  xold0 <- x0
  xold1 <- x1
  cat("Starting value is:", xold0, "\n")
  
  for(i in 1:iter) {
    f_xold1 <- ftn(xold1)
    f_xold0 <- ftn(xold0)
    xnew <- xold1 - f_xold1 * (xold1 - xold0) / (f_xold1 - f_xold0)
    f_xnew <- ftn(xnew)
    cat("Next x value:", xnew, "\n")
    
    # line segments
    df_points_1[i,] <- c(x1 = xold0, y1 = f_xold0, x2 = xold1, y2 = f_xold1)
    df_points_2[i,] <- c(x1 = xold1, y1 = f_xold1, x2 = xnew, y2 = 0)
    df_points_3[i,] <- c(x1 = xnew, y1 = 0, x2 = xnew, y2 = f_xnew)
    
    xold0 <- xold1
    xold1 <- xnew
  }
  
  # start is the min of these values
  plot_start <- min(df_points_1$x1, df_points_1$x2, x0) - 0.1
  # end is the max of these values
  plot_end <- max(df_points_1$x1, df_points_1$x2, x0) + 0.1
  
  # calculate the value of the function fx for all x
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for(i in seq_along(x)) {
    fx[i] <- ftn(x[i])
  }
  
  # Add function_data to store function values
  function_data <- data.frame(x, fx)
  
  p <- ggplot(function_data, aes(x = x, y = fx)) +
    geom_line(color = "royalblue", size = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_1, color = "black", lty = 1, size = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_2, color = "red", lty = 2) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_3, color = "red", lty = 2) +
    geom_abline(intercept = 0, slope = 0) +
    theme_bw()
  
  
  print(p)
  
  xnew
}

##### testing 
f <- function(x) {
  (x^3)-4*x+5
}
secant_show(f,5,4)

f1 <- function(x) {
  x^2 - 4
}
secant_show(f1, x0 = 10, x1 = 8, iter = 7)
