rounds <- 10000

# How much should I bet to minimize total capital draw-down and long run expected profit

# Gets the minimal draw-down for the num of rounds specified
sim_rw <- function(init_cap, betting_amt){
  returns <- rnorm(rounds, 0.01,0.05)
  capital <- numeric(rounds + 1)
  capital[1] <- init_cap
  bets <- numeric(rounds+1)
  bets[1] <- init_cap * betting_amt
  
  for(i in 1:rounds){
    change <- bets[i] * (1 + returns[i])
    capital[i+1] <- (capital[i] - bets[i]) + change
    bets[i+1] <- capital[i+1] * betting_amt
  }
  min(capital)
}


res <-replicate(10000, sim_rw(1, 0.05))
hist(res)

# Vary the probability of winning and compute the distribution of the greatest drawdown.

# What is the probability of having more than 3 consecutive down days?
# What is the probability od having at least 15 up days in one month (20 trading days)?

#----------------------------------------------------------------------------------------
rounds <- 500

# How much should I bet to minimize total capital draw-down and long run expected profit

# Gets the minimal draw-down for the num of rounds specified
sim_rw <- function(init_cap, betting_amt){
  returns <- rnorm(rounds, 0.02, 0.25)
  capital <- numeric(rounds + 1)
  capital[1] <- init_cap
  bets <- numeric(rounds+1)
  bets[1] <- init_cap * betting_amt
  
  for(i in 1:rounds){
    change <- bets[i] * (1 + returns[i])
    capital[i+1] <- (capital[i] - bets[i]) + change
    bets[i+1] <- capital[i+1] * betting_amt
  }
  
  c(min(capital), capital[length(capital)])
}

betting_amts <- seq(0.05, 1, by = 0.01)

opt_betting_amt <- function(ba){
  opt <- numeric(length(ba))
  
  for (i in 1:length(ba)){
    res <-replicate(10000, sim_rw(1, ba[i]))
    dd_prob <- mean((res[1, ] <= 0.3))
    expected_gain <- mean(res[2, ])
    opt[i] <- expected_gain / dd_prob
  }
  opt
}
drawDowns <- opt_betting_amt(betting_amts)

plot(betting_amts, drawDowns)

#---------------------------------------------------------------
rounds <- 500
prob_winning <- 0.5
# How much should I bet to minimize total capital draw-down and long run expected profit

# Computes the ratio of expected return / pr( max draw down <= 0.1)
sim_rw <- function(init_cap, betting_amt){
  returns <- sample(c(2, 0.5), rounds, replace = TRUE, prob = c(prob_winning, 1-prob_winning))
  capital <- numeric(rounds + 1)
  capital[1] <- init_cap
  bets <- numeric(rounds+1)
  bets[1] <- init_cap * betting_amt
  
  for(i in 1:rounds){
    change <- bets[i] * returns[i]
    capital[i+1] <- (capital[i] - bets[i]) + change
    bets[i+1] <- capital[i+1] * betting_amt
  }
  
  c(min(capital), capital[length(capital)])
}

betting_amts <- seq(0.05, 1, by = 0.01)

opt_betting_amt <- function(ba){
  opt <- numeric(length(ba))
  
  for (i in 1:length(ba)){
    res <-replicate(10000, sim_rw(1, ba[i]))
    dd_prob <- mean((res[1, ] <= 0.1))
    expected_gain <- mean(res[2, ])
    opt[i] <- expected_gain / dd_prob
  }
  opt
}
drawDowns <- opt_betting_amt(betting_amts)

plot(betting_amts, drawDowns)
