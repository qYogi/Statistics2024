set.seed(123)
N <- 10000  

lambda1 <- 4
lambda2 <- 12

prob_mecanic1 <- 3 / 4
prob_mecanic2 <- 1 / 4

mechanic_choice <- runif(N)
service_times <- ifelse(mechanic_choice <= prob_mecanic1, 
                        rexp(N, rate = lambda1), 
                        rexp(N, rate = lambda2))

mean_service_time <- mean(service_times)
cat("Media estimată a timpului de servire:", mean_service_time, "ore\n")
