#######################
## Coordinate Descent Algorithm for Optimization
## Copyright (c) Jeff Hwang
#######################


golden = function(f, lower, upper, tolerance = 1e-5)
{
  golden.ratio = 2/(sqrt(5) + 1)
  
  ## Use the golden ratio to find the initial test points
  x1 <- lower + golden.ratio * (upper - lower)
  x2 <- upper - golden.ratio * (upper - lower)
  
  ## the arrangement of points is:
  ## lower ----- x2 --- x1 ----- upper
  
  ### Evaluate the function at the test points
  f1 <- f(x1)
  f2 <- f(x2)
  
  while (abs(upper - lower) > tolerance) {
    if (f2 > f1) {
      # the minimum is to the right of x2
      lower <- x2  # x2 becomes the new lower bound
      x2 <- x1     # x1 becomes the new x2
      f2 <- f1     # f(x1) now becomes f(x2)
      x1 <- lower + golden.ratio * (upper - lower)  
      f1 <- f(x1)  # calculate new x1 and f(x1)
    } else {
      # then the minimum is to the left of x1
      upper <- x1  # x1 becomes the new upper bound
      x1 <- x2     # x2 becomes the new x1
      f1 <- f2
      x2 <- upper - golden.ratio * (upper - lower)
      f2 <- f(x2)  # calculate new x2 and f(x2)
    }
  }
  (lower + upper)/2 # the returned value is the midpoint of the bounds
}

##############################
g <- function(x,y) { 
  5 * x ^ 2 - 6 * x * y + 5 * y ^ 2
}
## starting point
x <- seq(-1.5, 1, len = 100)
y <- seq(-1.5, 1, len = 100)

##############################
####Graph for starting point x = -1.5, and y = -1.5.
##############################
df_points1 <- data.frame(
  x1 = numeric(0),
  y1 = numeric(0),
  z1 = numeric(0),
  x2 = numeric(0),
  y2 = numeric(0),
  z2 = numeric(0))
df_points2 <- df_points1

xnew <- -1.5
ynew <- -1.5

cat("Starting value: x:", xnew, "y:", ynew, "\n")

for(i in 1:15) {
  xold <- xnew
  yold <- ynew
  
  # Update x
  g1 <- function(x) {
    5 * x ^ 2 - 6 * x * yold + 5 * yold ^ 2
  }
  xnew <- golden(g1, lower = -1.5, upper = 1.5)
  
  # Update y
  g2 <- function(y) {
    5 * xnew ^ 2 - 6 * xnew * y + 5 * y ^ 2
  }
  ynew <- golden(g2, lower = -1.5, upper = 1.5)
  
  # print out current value of x and y
  cat(i, "current x:", xnew, "current y:", ynew, "\n")
  cat("-------------------------------------------\n")
  
  df_points1[i,] <- c(x1 = xold, y1 = yold, z1 = 0, x2 = xnew, y2 = yold, z2 = 0)
  df_points2[i,] <- c(x1 = xnew, y1 = yold, z1 = 0, x2 = xnew, y2 = ynew, z2 = 0)
}

g <- function(x,y) { 
  5 * x ^ 2 - 6 * x * y + 5 * y ^ 2
}

# draw the plot
x <- seq(-1.5, 1, len = 100)
y <- seq(-1.5, 1, len = 100)

contour_df <- data.frame(
  x = rep(x, each = 100),
  y = rep(y, 100),
  z = outer(x, y, g)[1:100^2]
)

ggplot(contour_df, aes(x = x, y = y, z = z)) +
  geom_contour(binwidth = 0.9) +
  geom_segment(aes(x = x1, y = y1, z = z1, xend = x2, yend = y2, zend = z2), 
               data = df_points1, color = "red", lty = 1) +
  geom_segment(aes(x = x1, y = y1, z = z1, xend = x2, yend = y2, zend = z2),
               data = df_points2, color = "red", lty = 1) +
  theme_bw()



#######################################
### Graph for starting point x = -1.5, and y = 1.
#######################################
df_points1 <- data.frame(
  x1 = numeric(0),
  y1 = numeric(0),
  z1 = numeric(0),
  x2 = numeric(0),
  y2 = numeric(0),
  z2 = numeric(0))
df_points2 <- df_points1

xnew <- -1.5
ynew <- 1

cat("Starting value: x:", xnew, "y:", ynew, "\n")

for(i in 1:15) {
  xold <- xnew
  yold <- ynew
  
  # Update x
  g1 <- function(x) {
    5 * x ^ 2 - 6 * x * yold + 5 * yold ^ 2
  }
  xnew <- golden(g1, lower = -1.5, upper = 1.5)
  
  # Update y
  g2 <- function(y) {
    5 * xnew ^ 2 - 6 * xnew * y + 5 * y ^ 2
  }
  ynew <- golden(g2, lower = -1.5, upper = 1.5)
  
  # print out current value of x and y
  cat(i, "current x:", xnew, "current y:", ynew, "\n")
  cat("-------------------------------------------\n")
  
  df_points1[i,] <- c(x1 = xold, y1 = yold, z1 = 0, x2 = xnew, y2 = yold, z2 = 0)
  df_points2[i,] <- c(x1 = xnew, y1 = yold, z1 = 0, x2 = xnew, y2 = ynew, z2 = 0)
}

g <- function(x,y) { 
  5 * x ^ 2 - 6 * x * y + 5 * y ^ 2
}

# draw the plot
x <- seq(-1.5, 1, len = 100)
y <- seq(-1.5, 1, len = 100)

contour_df <- data.frame(
  x = rep(x, each = 100),
  y = rep(y, 100),
  z = outer(x, y, g)[1:100^2]
)

ggplot(contour_df, aes(x = x, y = y, z = z)) +
  geom_contour(binwidth = 0.9) +
  geom_segment(aes(x = x1, y = y1, z = z1, xend = x2, yend = y2, zend = z2), 
               data = df_points1, color = "red", lty = 1) +
  geom_segment(aes(x = x1, y = y1, z = z1, xend = x2, yend = y2, zend = z2),
               data = df_points2, color = "red", lty = 1) +
  theme_bw()




















