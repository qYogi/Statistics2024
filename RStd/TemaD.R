#TemaD
#D1
probabilitati <- read.csv("/Users/bogdan/Downloads/RStd/probabilitati.csv")

punctaje <- probabilitati$probabilitati

n <- length(punctaje)

sigma2 <- 92.16
sigma <- sqrt(sigma2)

media_esantion <- mean(punctaje)

niveluri_incredere <- c(0.95, 0.99)

for (nivel in niveluri_incredere) {

  eroare_standard <- sigma / sqrt(n)

  valoare_critica <- qnorm((1 + nivel) / 2)

  margine_eroare <- valoare_critica * eroare_standard

  limita_inferioara <- media_esantion - margine_eroare
  limita_superioara <- media_esantion + margine_eroare

  cat("Interval de încredere", nivel * 100, "%: [", limita_inferioara, ", ", limita_superioara, "]\n")
}

#D2

statistica <- read.csv("/Users/bogdan/Downloads/RStd/statistica.csv")$statistica

n <- length(statistica)

media_esantion <- mean(statistica)

s <- sd(statistica)

niveluri_incredere <- c(0.95, 0.99)

for (nivel in niveluri_incredere) {
  
  df <- n - 1

  t_critic <- qt((1 + nivel) / 2, df)

  eroare_standard <- s / sqrt(n)

  margine_eroare <- t_critic * eroare_standard

  limita_inferioara <- media_esantion - margine_eroare
  limita_superioara <- media_esantion + margine_eroare

  cat(sprintf("Interval de încredere %.0f%%: [%.2f, %.2f]\n", nivel * 100, limita_inferioara, limita_superioara))
}


