set.seed(123) 
N <- 10000 

p_x <- 0.3
p_y <- 0.5

estimate_probability <- function(N) {
  X <- rgeom(N, prob = p_x)
  Y <- rgeom(N, prob = p_y)
  Z <- Y^2
  mean(X < Z)
}

prob_estimate <- estimate_probability(N)
cat("Probabilitatea estimată P(X < Y^2):", prob_estimate, "\n")

z <- qnorm(0.975)  
epsilon <- 0.005

n_required <- (z / epsilon)^2 * prob_estimate * (1 - prob_estimate)
n_required <- ceiling(n_required)
cat("Numărul necesar de simulări pentru o eroare de ±0.005:", n_required, "\n")

prob_estimate_final <- estimate_probability(n_required)
cat("Probabilitatea estimată finală P(X < Y^2):", prob_estimate_final, "\n")
