calc_probs <- function(lambda, p, n, k, m) {
  poisson_probs <- dpois(k:m, lambda)
  geometric_probs <- dgeom(k:m, p)
  binomial_probs <- dbinom(k:m, n, p)
  return(list(poisson = poisson_probs, geometric = geometric_probs, binomial = binomial_probs))
}

plot_pmfs <- function(probs, lambda, p, n) {
  x <- k:m
  par(mfrow = c(1, 3))
  barplot(probs$poisson, names.arg = x, main = "Poisson(λ)")
  barplot(probs$geometric, names.arg = x, main = "Geometrica(p)")
  barplot(probs$binomial, names.arg = x, main = "Binomiala(n, p)")
}

find_k0 <- function(lambda) {
  k0 <- 0
  while (ppois(k0, lambda) < 1 - 1e-6) {
    k0 <- k0 + 1
  }
  return(k0)
}

lambda <- 3 
p <- 0.2    
n <- 10    
k <- 2     
m <- 8     

probabilities <- calc_probs(lambda, p, n, k, m)
plot_pmfs(probabilities, lambda, p, n)
k0 <- find_k0(lambda)
cat("Cel mai mic k0 pentru Poisson(λ =", lambda, ") este:", k0, "\n")

#################################################################################
#################################################################################
#################################################################################

analizeaza_note <- function() { 
  date <- read.csv("/Users/bogdan/Downloads/RStd/note_PS.csv", header = TRUE) 
  note_p <- date$P
  note_s <- date$S
  frecvente_p <- table(note_p) / length(note_p)
  frecvente_s <- table(note_s) / length(note_s)
  frecvente_p <- as.vector(frecvente_p)
  frecvente_s <- as.vector(frecvente_s)
  media_p <- mean(note_p)
  media_s <- mean(note_s)
  return(list(
    frecvente_p = frecvente_p, frecvente_s = frecvente_s,
    media_p = media_p, media_s = media_s
  ))
}

elimina_aberante_si_grafic <- function(nume_esantion) { 
  date <- read.csv("/Users/bogdan/Downloads/RStd/note_PS.csv", header = TRUE)  
  if (nume_esantion == "P") {
    note <- date$P
  }
  else{
    note <- date$S
  }
  Q1 <- quantile(note, 0.25)
  Q3 <- quantile(note, 0.75)
  IQR <- Q3 - Q1
  limita_inf <- Q1 - 1.5 * IQR
  limita_sup <- Q3 + 1.5 * IQR
  note_curate <- note[note >= limita_inf & note <= limita_sup]
  breaks <- seq(1, 10, by = 1)
  hist(note_curate, breaks = breaks, right = FALSE, main = paste("Distributia frecventelor pentru ", nume_esantion), xlab = "Note", ylab = "Frecventa")
  return(note_curate)
}


rezultate <- analizeaza_note()
cat("Frecvente P:", rezultate$frecvente_p, "\n")
cat("Frecvente S:", rezultate$frecvente_s, "\n")
cat("Media P:", rezultate$media_p, "\n")
cat("Media S:", rezultate$media_s, "\n")
cat("----------------\n")
note_p_curate <- elimina_aberante_si_grafic("P")
note_s_curate <- elimina_aberante_si_grafic("S")


