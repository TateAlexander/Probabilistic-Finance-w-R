# Using simulation to approximate integrals------------

# Uses Simulation to approximate the integral of 1/x
x <- runif(50000, min = 1, max = 5)
t <- 1 / x
integral_approx1 <- 4 * mean(t)
integral_approx1

# Uses Simulation to approximate the integral of ln(x)
w <- log(x)
integral_approx2 <- 4 * mean(w)
integral_approx2
