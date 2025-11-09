rounds <- 10000

# How much should I bet to minimize total capital draw-down and long run expected profit

# Gets the minimal draw-down for the num of rounds specified
sim_rw <- function(init_cap, betting_amt){
  returns <- rnorm(rounds, 0.01,0.05)
  capital <- numeric(rounds + 1)
  capital[1] <- init_cap
  bets = numeric(rounds+1)
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

