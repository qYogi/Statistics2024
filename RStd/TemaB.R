#B1
in_torus <- function(x1, x2, x3, R, r) {
  dist_centru_cerc <- sqrt(x1^2 + x2^2)
  dist_punct_cerc <- sqrt((dist_centru_cerc - R)^2 + x3^2)
  return(dist_punct_cerc < r)
}

estimare_volum <- function(R, r, N) {
  puncte_interior <- 0
  
  for (i in 1:N) {
    x1 <- runif(1, -R-r, R+r)
    x2 <- runif(1, -R-r, R+r)
    x3 <- runif(1, -r, r)
    
    if (in_torus(x1, x2, x3, R, r)) {
      puncte_interior <- puncte_interior + 1
    }
  }
  
  volum_estimat <- ((2*R + 2*r)^3) * (puncte_interior / N)
  return(volum_estimat)
}

R <- 10
r <- 3
N <- c(10000, 20000, 50000) 

volum_exact <- 2 * pi^2 * R * r^2
cat("Volumul exact al torului este: ", volum_exact, "\n")

for (n in N) {
  volum_estimat <- estimare_volum(R, r, n)
  eroare_relativa <- abs(volum_estimat - volum_exact) / volum_exact * 100
  cat(paste("Pentru", n, "puncte, volumul estimat este:", volum_estimat,", eroarea relativa este:", eroare_relativa, "%\n"))
}

#B2
in_triunghi <- function(x, y) {
  return(y >= 0 && y <= 2*x && y <= 6 - 3*x)
}

# Funcie Monte Carlo
estimare_arie <- function(N) {
  puncte_interior <- 0
  
  for (i in 1:N) {
    x <- runif(1, 0, 2) # intervalul x
    y <- runif(1, 0, 3) # intervalul y
    
    if (in_triunghi(x, y)) {
      puncte_interior <- puncte_interior + 1
    }
  }
  
  arie_estimata <- 6 * (puncte_interior / N) # Aria dreptunghiului este 6
  return(arie_estimata)
}

N <- 20000 

arie_estimata <- estimare_arie(N)

cat("Aria estimata a triunghiului este:", arie_estimata, "\n")

#B3
f_a <- function(x) (2*x - 1) / (x^2 - x - 6)
integral_a_approx <- integrate(f_a, lower = -1, upper = 1)$value
exact_a <- log(3) - log(2)
cat("(a) Aproximare:", integral_a_approx, "\tExact:", exact_a, "\n")

f_b <- function(x) (x + 4) / (x - 3)^(1/3)
a <- 3 + 0.0001
integral_b_approx <-integrate(f_b, lower = a, upper = 11)$value
exact_b <- 61.2
cat("(b) Aproximare:", integral_b_approx, "\tExact:", exact_b, "\n")

f_c <- function(x) x * exp(-x^2)
integral_c_approx <- integrate(f_c, lower = 0, upper = Inf)$value
exact_c <- 1/2
cat("(c) Aproximare:", integral_c_approx, "\tExact:", exact_c, "\n")

#B4
B4_a <- function(num_utilizatori_initial, num_utilizatori_doriti, n, p, q) {
  ani <- 0
  num_utilizatori_actual <- num_utilizatori_initial
  while (num_utilizatori_actual < num_utilizatori_doriti) {
    ani <- ani + 1
    noi_utilizatori <- rbinom(1, n, p)
    utilizatori_retragere <- rbinom(1, num_utilizatori_actual, q)
    num_utilizatori_actual <- num_utilizatori_actual + noi_utilizatori - utilizatori_retragere
  }
  return(ani)
}

B4_b <- function(num_simulari, num_utilizatori_initial, num_utilizatori_doriti, n, p, q) {
  count_succes <- 0
  for (simulare in 1:num_simulari) {
    ani_totali <- 40 + 10/12
    num_utilizatori_final <- num_utilizatori_initial
    for (ani in 1:floor(ani_totali)) {
      noi_utilizatori <- rbinom(1, n, p)
      utilizatori_retragere <- rbinom(1, num_utilizatori_final, q)
      num_utilizatori_final <- num_utilizatori_final + noi_utilizatori - utilizatori_retragere
    }
    if (num_utilizatori_final >= num_utilizatori_doriti) {
      count_succes <- count_succes + 1
    }
  }
  probabilitate <- count_succes / num_simulari
  return(probabilitate)
}

B4_c <- function(num_utilizatori_initial, num_utilizatori_doriti, n, p, q, eroare, confidenta) {
  z <- qnorm((1 + confidenta) / 2)
  num_simulari <- 0
  prob_actuala <- 0
  var_estimata <- 1
  while (TRUE) {
    num_simulari <- num_simulari + 1000
    prob_actuala <- B4_b(num_simulari, num_utilizatori_initial, num_utilizatori_doriti, n, p, q)
    var_estimata <- prob_actuala * (1 - prob_actuala) / num_simulari
    eroare_actuala <- z * sqrt(var_estimata)
    if (eroare_actuala <= eroare) {
      break
    }
  }
  return(list(probabilitate = prob_actuala, num_simulari = num_simulari))
}

num_utilizatori_initial <- 10000
num_utilizatori_doriti <- 15000
n <- 1000
p <- 0.25
q <- 0.01
eroare <- 0.01
confidenta <- 0.99

rezultat_a <- B4_a(num_utilizatori_initial, num_utilizatori_doriti, n, p, q)
cat("Numărul mediu de ani:", rezultat_a, "\n")

rezultat_b <- B4_b(10000, num_utilizatori_initial, num_utilizatori_doriti, n, p, q)
cat("Probabilitatea:", rezultat_b, "\n")

rezultat_c <- B4_c(num_utilizatori_initial, num_utilizatori_doriti, n, p, q, eroare, confidenta)
cat("Numarul de simulari necesare pentru o eroare de maxim ±0.01 cu o probabilitate de 0.99:", rezultat_c$num_simulari, "\n")
cat("Probabilitatea estimata cu eroare maxima de ±0.01:", rezultat_c$probabilitate, "\n")


