
# a
perm_random <- function(n) {
  U <- runif(n)  
  perm <- order(U)
  return(perm)
}

bitstrings_random <- function(n, k) {
  bitstrings <- replicate(n, sample(0:1, k, replace = TRUE), simplify = FALSE)
  return(bitstrings)
}

# b
compare <- function(Wi, Wj) {
  Lij <- min(length(Wi), length(Wj))
  
  for (h in 1:Lij) {
    if (Wi[h] < Wj[h]) {
      return(TRUE)  
    } else if (Wi[h] > Wj[h]) {
      return(FALSE)  
    }
  }
  
  while (length(Wi) != length(Wj)) {
    if (length(Wi) < length(Wj)) {
      Wi <- c(Wi, sample(0:1, 1, replace = TRUE))  
    } else {
      Wj <- c(Wj, sample(0:1, 1, replace = TRUE))
    }
    
    if (Wi[length(Wi)] < Wj[length(Wj)]) {
      return(TRUE)  
    } else if (Wi[length(Wi)] > Wj[length(Wj)]) {
      return(FALSE) 
    }
  }
  
  while (TRUE) {
    new_Wi <- sample(0:1, 1, replace = TRUE)
    new_Wj <- sample(0:1, 1, replace = TRUE)
    Wi <- c(Wi, new_Wi)
    Wj <- c(Wj, new_Wj)
    
    if (new_Wi < new_Wj) {
      return(TRUE)
    } else if (new_Wi > new_Wj) {
      return(FALSE)
    }
  }
}


# c
quicksort_random <- function(W, left, right) {
  if (left < right) {
    pivot_index <- sample(left:right, 1)
    pivot <- W[[pivot_index]]
    
    W[[pivot_index]] <- W[[right]]
    W[[right]] <- pivot
    
    cont_index <- left
    for (i in left:(right - 1)) {
      if (compare(W[[i]], pivot)) {
        aux <- W[[cont_index]]
        W[[cont_index]] <- W[[i]]
        W[[i]] <- aux
        cont_index <- cont_index + 1
      }
    }
    W[[right]] <- W[[cont_index]]
    W[[cont_index]] <- pivot
    
    W <- quicksort_random(W, left, cont_index - 1)
    W <- quicksort_random(W, cont_index + 1, right)
  }
  return(W)
}

# d
permutareqs_random <- function(n, k) {
  W <- vector("list", n)
  for (i in 1:n) {
    W[[i]] <- sample(0:1, k, replace = TRUE)
  }
  
  sorted_W <- quicksort_random(W, 1, length(W))
  
  perm <- match(W, sorted_W)
  
  return(perm)
}

# C2 a
max_cut <- function(V, E) {
  n <- floor(length(V) / 2)
  A <- sample(V, n)
  B <- setdiff(V, A)
  
  cut_edges <- 0
  for (edge in 1:nrow(E)) {
    u <- E[edge, 1]
    v <- E[edge, 2]
    if ((u %in% A && v %in% B) || (u %in% B && v %in% A)) {
      cut_edges <- cut_edges + 1
    }
  }
  
  return(cut_edges)
}

# Funcția pentru maximizarea tăieturii prin repetarea generării tăieturii
max_cut_repetari <- function(V, E, repeats) {
  max_cut_size <- 0
  for (i in 1:repeats) {
    cut_size <- max_cut(V, E)
    if (cut_size > max_cut_size) {
      max_cut_size <- cut_size
    }
  }
  return(max_cut_size)
}

set.seed(246)
n <- 5
k <- 3
G <- list(nodes=1:(2*n), edges=rbind(c(1,5), c(2,4), c(2,5), c(3,4), c(3,6), c(5,6)))

print(perm_random(n))
print(bitstrings_random(n, k))
W <- lapply(1:n, function(i) sample(0:1, k, replace=TRUE))
print(compare(W[[1]], W[[2]]))
print(quicksort_random(W, 1, length(W)))
print(permutareqs_random(n, k))
print(max_cut(G$nodes, G$edges))
print(max_cut_repetari(G$nodes, G$edges, 100))

