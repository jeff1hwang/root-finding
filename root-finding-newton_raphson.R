#######################
## Root Finding with Newton Raphson
## Copyright (c) Jeff Hwang
#######################

newtonraphson_show <- function(ftn, x0, iter = 5) {
  # applies Newton-Raphson to find x such that ftn(x)[1] == 0
  # ftn is a function of x. it returns two values, f(x) and f'(x)
  # x0 is the starting point
  
  # df_points_1 and df_points_2 are used to track each update
  df_points_1 <- data.frame(
    x1 = numeric(0),
    y1 = numeric(0),
    x2 = numeric(0),
    y2 = numeric(0))
  df_points_2 <- df_points_1
  
  xnew <- x0
  cat("Starting value is:", xnew, "\n")
  
  # the algorithm
  for(i in 1:iter){
    xold <- xnew
    f_xold <- ftn(xold)
    xnew <- xold - f_xold[1] / f_xold[2]
    cat("Next x value:", xnew, "\n")
    
    # the line segments. You will need to replace the NAs with the appropriate values
    df_points_1[i,] <- c(x1 = xold, y1 = 0, x2 = xold, y2 = f_xold[1]) # vertical segment 
    df_points_2[i,] <- c(x1 = xold, y1 = f_xold[1], x2 = xnew, y2 = 0) # tangent segment 
  }
  
  # start is the min of these values
  plot_start <- min(df_points_1$x1, df_points_1$x2, x0) - 0.1
  # end is the max of these values
  plot_end <- max(df_points_1$x1, df_points_1$x2, x0) + 0.1
  
  # calculate the value of the function fx for all x
  x <- seq(plot_start, plot_end, length.out = 200)
  fx <- rep(NA, length(x))
  for (i in seq_along(x)) {
    fx[i] <- ftn(x[i])[1]
  }
  
  # Add function_data to store function values
  function_data <- data.frame(x, fx)
  
  # Write your code
  p <- ggplot(function_data, aes(x = x, y = fx)) +
    geom_line(color = "royalblue", size = 1) + # plot the function
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_1, color = "black", lty = 1) +
    geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
                 data = df_points_2, color = "red", lty = 2) +
    geom_abline(intercept = 0, slope = 0) +
    theme_bw()
  
  print(p)
  
  xnew # value that gets returned
}


# example of how your functions could be written
a <- function(x){
  value <- cos(x) - x   # f(x)
  derivative <- -sin(x) - 1  # f'(x)
  c(value, derivative) # the function returns a vector with two values
} 

## Testing
newtonraphson_show(a, 3, iter = 8)




