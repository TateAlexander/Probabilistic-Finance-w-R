stock_vasieck <- function(k, eq_trend, sigma, init_price, n){
  walk = numeric(n)
  dw <- rnorm(n, mean = 0, sd = 1)
  for (i in 1:n){
    if(i == 1){
      delta_r <- k * (eq_trend[i] - init_price) + sigma * dw[i]
      walk[i] <- init_price + delta_r
    }else{
      delta_r <- k * (eq_trend[i] - walk[i-1]) + sigma * dw[i]
      walk[i] <- walk[i-1] + delta_r
    }
  }
  walk
}

n <- 200
t <- seq(1, n)
eq_trend <- 0.2 * t + 5

v_res <- stock_vasieck(0.4 , eq_trend, 1, 5, n)

plot(v_res)
lines(x = t, y = eq_trend)

# Back-out the mean reversion parameter from the data

backout_k <- function(trend, prices){
  
  # Drop the first trend element
  k_est <- trend[-1]
  # Drop the last price element
  k_est_comp <- prices[-length(prices)]
  
  A <- cbind(k_est, k_est_comp)
  
  soln <- qr.solve(A, prices[-1])
  soln
}

backedout <- backout_k(eq_trend, v_res)
backedout[1]

est_sigma <- function(k, eq_trend, prices){
  sigs = numeric(length(prices) - 1)
  
  for (i in 2:length(prices)){
    sig <- prices[i] - prices[i-1]
    sigs[i-1] <- sig - k * (eq_trend[i] - prices[i-1])
  }
  
  sigs
}

backout_sigma <- est_sigma(backedout[1], eq_trend, v_res)
backout_sigma

# Now make confidence intervals for the rates


# Attempt to model real stock data and perform a ks test to verify
# the validity of the model.

#-------Calibrate from real time data---------

aste_prices <- read.csv("C:\\Users\\tate2\\OneDrive\\Desktop\\R code personal\\asteStockPrices.csv")$Close
aste_prices
aste_t <- seq(1, length(aste_prices))

aste_lm <- lm(aste_prices ~ aste_t)
aste_coeffs <- aste_lm$coefficients
aste_reg_line <- aste_coeffs["(Intercept)"] + aste_t * aste_coeffs["aste_t"]

plot(aste_prices)
lines(aste_t, aste_reg_line)

aste_k <- backout_k(aste_reg_line, aste_prices)[1]
aste_k

aste_sigma <- est_sigma(aste_k, aste_reg_line, aste_prices)

mean(aste_sigma)
sd(aste_sigma)
hist(aste_sigma)

library(ks)
kde_obj <- kde(aste_sigma)

plot(kde_obj)

x_sim <- rkde(kde_obj, n = 1000)
x_sim


# Now we'll use k & sigma to simulate aste prices & test the validity of the simulation with 2 sample ks test

library(energy)

aste_sim <- stock_vasieck(aste_k, aste_reg_line, sd(aste_sigma), aste_prices[1], length(aste_prices))
plot(aste_sim)
x <- c(aste_prices, aste_sim)
  
res <- eqdist.etest(
  x,
  sizes = c(length(aste_prices), length(aste_sim)),
  R = 20000
)
  
res

# To improve this, we can take the kde of the dist

#----------------------------------1/23/2026 update-------------------------------------
stock_vasieck <- function(k, eq_trend, stoch_samples, init_price, n){
  walk = numeric(n)
  dw <- rnorm(n, mean = 0, sd = 1)
  for (i in 1:n){
    if(i == 1){
      delta_r <- k * (eq_trend[i] - init_price) + stoch_samples[i]
      walk[i] <- init_price + delta_r
    }else{
      delta_r <- k * (eq_trend[i] - walk[i-1]) + stoch_samples[i]
      walk[i] <- walk[i-1] + delta_r
    }
  }
  walk
}

# Back-out the mean reversion parameter from the data

backout_k <- function(trend, prices){
  
  # Drop the first trend element
  k_est <- trend[-1]
  # Drop the last price element
  k_est_comp <- prices[-length(prices)]
  
  A <- cbind(k_est, k_est_comp)
  
  soln <- qr.solve(A, prices[-1])
  soln
}

est_sigma <- function(k, eq_trend, prices){
  sigs = numeric(length(prices) - 1)
  
  for (i in 2:length(prices)){
    sig <- prices[i] - prices[i-1]
    sigs[i-1] <- sig - k * (eq_trend[i] - prices[i-1])
  }
  
  sigs
}

# Now make confidence intervals for the rates

# Attempt to model real stock data and perform a ks test to verify
# the validity of the model.

#-------Calibrate from real time data---------

aste_prices <- read.csv("C:\\Users\\tate2\\OneDrive\\Desktop\\R code personal\\amStockPrices.csv")$Close
aste_prices
aste_t <- seq(1, length(aste_prices))

aste_lm <- lm(aste_prices ~ aste_t)
aste_coeffs <- aste_lm$coefficients
aste_reg_line <- aste_coeffs["(Intercept)"] + aste_t * aste_coeffs["aste_t"]

plot(aste_prices)
lines(aste_t, aste_reg_line)

aste_k <- backout_k(aste_reg_line, aste_prices)[1]
aste_k

aste_sigma <- est_sigma(aste_k, aste_reg_line, aste_prices)

mean(aste_sigma)
sd(aste_sigma)
hist(aste_sigma)

library(ks)
kde_obj <- kde(aste_sigma)

plot(kde_obj)

x_sim <- rkde(kde_obj, n = length(aste_prices))
x_sim

# Now we'll use k & sigma to simulate aste prices & test the validity of the simulation with 2 sample ks test

library(energy)

aste_sim <- stock_vasieck(aste_k, aste_reg_line, x_sim, aste_prices[1], length(aste_prices))
plot(aste_sim)

#---------Test distribution equivalency-----------

# plot the two sample distributions to see the similarity
as <- diff(aste_sim)
hist(as)
mean(as)
sd(as)

rp <- diff(aste_prices)
hist(rp)
mean(rp)
sd(rp)

eq <- ks.test(as, rp)$p.value
eq
#---------Vasieck appropriated stock Model-------------1/26/2026 update---------------------------------------------------

stock_vasieck <- function(k, eq_trend, stoch_samples, init_price, n){
  walk = numeric(n)
  dw <- rnorm(n, mean = 0, sd = 1)
  for (i in 1:n){
    if(i == 1){
      delta_r <- k * (eq_trend[i] - init_price) + stoch_samples[i]
      walk[i] <- init_price + delta_r
    }else{
      delta_r <- k * (eq_trend[i] - walk[i-1]) + stoch_samples[i]
      walk[i] <- walk[i-1] + delta_r
    }
  }
  walk
}

# Back-out the mean reversion parameter from the data

backout_k <- function(trend, prices){
  
  # Drop the first trend element
  k_est <- trend[-1]
  # Drop the last price element
  k_est_comp <- prices[-length(prices)]
  
  A <- cbind(k_est, k_est_comp)
  
  soln <- qr.solve(A, prices[-1])
  soln
}

est_sigma <- function(k, eq_trend, prices){
  sigs = numeric(length(prices) - 1)
  
  for (i in 2:length(prices)){
    delta_p <- prices[i] - prices[i-1]
    sigs[i-1] <- delta_p - k * (eq_trend[i] - prices[i-1])
  }
  
  sigs
}

# Now make confidence intervals for the rates

# Attempt to model real stock data and perform a ks test to verify
# the validity of the model.

#-------Calibrate from real time data---------

aste_prices <- read.csv("C:\\Users\\tate2\\OneDrive\\Desktop\\R code personal\\cienStockPrices.csv")$Close
aste_prices
aste_t <- seq(1, length(aste_prices))

aste_lm <- lm(aste_prices ~ aste_t)
aste_coeffs <- aste_lm$coefficients
aste_reg_line <- aste_coeffs["(Intercept)"] + aste_t * aste_coeffs["aste_t"]

plot(aste_prices)
lines(aste_t, aste_reg_line)

aste_k <- backout_k(aste_reg_line, aste_prices)[1]
aste_k

aste_sigma <- est_sigma(aste_k, aste_reg_line, aste_prices)
sd(aste_sigma)

library(ks)
kde_obj <- kde(aste_sigma)

plot(kde_obj)

# Now we'll use k & sigma to simulate aste prices & test the validity of the simulation with 2 sample ks test

library(energy)

avg_pval <- replicate(5000, expr = {
  x_sim <- rkde(kde_obj, n = length(aste_prices))
  aste_sim <- stock_vasieck(aste_k, aste_reg_line, x_sim, aste_prices[1], length(aste_prices))
  
  as <- diff(aste_sim)
  rp <- diff(aste_prices)
  
  ks.test(as, rp)$p.value
})
mean(avg_pval)
#---------Test distribution equivalency-----------

mean_trend <- function(k_est, price_walk, eq_trend){
  trend_means <- numeric(length(price_walk))
  trend_means <- price_walk[1]
  for(i in 2:length(eq_trend)){
    val <- k_est * (eq_trend[i]) + (1-k_est) * price_walk[i-1]
    trend_means[i] <- val
  }
  trend_means
}

a <- function(means, rw, sigma, alpha){
  upper <- numeric(length(means))
  lower <- numeric(length(means))
  
  for(i in 1:length(means)){
    upper[i] <- means[i] + sd(sigma) * qnorm(1 - alpha/2)
    lower[i] <- means[i] - sd(sigma) * qnorm(1 - alpha/2)
  }
  return(list(lower = lower, upper = upper))
}
x_sim <- rkde(kde_obj, n = length(aste_prices))

m <- mean_trend(aste_k, aste_prices, aste_reg_line)
bounds <- a(m, aste_prices, aste_sigma, 0.05)

x <- seq(1, length(m))

# Estimate the linear correlation between time and price
cor(x, aste_prices)

plot(aste_prices)
lines(aste_reg_line)
lines(m, col = "green")
lines(bounds$lower, col = "red")
lines(bounds$upper, col = "red")


# Simulate the rw to see the prob of reaching that residual magnitude

last_equil <- aste_reg_line[length(aste_prices)]
last_equil
last_price <- aste_prices[length(aste_prices)]
last_price
residual_prob <- replicate(10000, expr = {
  stoch <- rkde(kde_obj, n = length(aste_reg_line))
  rw <- stock_vasieck(aste_k, aste_reg_line, stoch, aste_prices[1], length(aste_reg_line))
  min(rw - aste_reg_line)
})
mean(residual_prob < last_price - last_equil)

# Obtain the residual distribution

resid_dist <- function(prices, trend){
  resid <- numeric(length(prices))
  for (i in 1:length(prices)){
    resid[i] <- prices[i] - trend[i]
  }
  resid
}

sim_resid_dist <- replicate(10000, expr = {
  sim <- rkde(kde_obj, n = length(aste_prices))
  rw_prices <- stock_vasieck(aste_k, aste_reg_line, sim, aste_prices[1], length(aste_prices))
  resid_dist(rw_prices, aste_reg_line)
})

resid_q <- quantile(sim_resid_dist, probs = c(0.005, 0.995))
sd(res_dist)

lines(aste_reg_line + resid_q[1], col = "blue")
lines(aste_reg_line + resid_q[2], col = "blue")
