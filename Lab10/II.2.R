set.seed(123)  
N <- 50000
lambda <- 3

u <- rexp(N, rate = lambda)

f <- function(u) exp(-2 * u^2)

g <- function(u, lambda) lambda * exp(-lambda * u)

integral_estimate <- mean(f(u) / g(u, lambda))
cat("Estimarea integralei folosind metoda Monte Carlo îmbunătățită:", integral_estimate, "\n")

exact_value <- sqrt(pi / 8)
cat("Valoarea exactă a integralei:", exact_value, "\n")

relative_error <- abs(integral_estimate - exact_value) / exact_value
cat("Eroarea relativă:", relative_error, "\n")
