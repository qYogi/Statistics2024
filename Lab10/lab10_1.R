set.seed(123) 
n <- 10000
x <- runif(n, min = 0, max = 2)
y <- runif(n, min = 0, max = 2)


f <- function(x) -2x^2 + 5x - 2

points_under_curve <- sum(y <= f(x))

rect_area <- 2 * 2

area_estimate <- (points_under_curve / n) * rect_area
cat("Aria estimată prin metoda Monte Carlo:", area_estimate, "\n")

integrand <- function(x) -2x^2 + 5x - 2
exact_area <- integrate(integrand, lower = 0.5, upper = 2)$value
cat("Aria exactă prin integrare:", exact_area, "\n")

relative_error <- abs(area_estimate - exact_area) / exact_area
cat("Eroarea relativă:", relative_error, "\n")