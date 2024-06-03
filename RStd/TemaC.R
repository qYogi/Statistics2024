#C1
#(a)
permutation = function(n){
  U = runif(n, 0, 1)
  perm = order(U)
  return (perm)
}

#(b)
comp = function(w1, w2){
  for (i in 1:min(length(w1), length(w2))){
    if (w1[i] < w2[i])
      return (TRUE)
    else if (w1[i] > w2[i])
      return (FALSE)
  }
  if (length(w1) < length(w2)){
    for (i in (length(w1) + 1):length(w2)){
      w1 <- append(w1, sample(0:1, 1))
      if (w1[i] < w2[i])
        return (TRUE)
      else if (w1[i] > w2[i])
        return (FALSE)
    }
  }
  else if (length(w1) > length(w2)){
    for (i in (length(w2) + 1):length(w1)){
      w2 <- append(w2, sample(0:1, 1))
      if (w1[i] < w2[i])
        return (TRUE)
      else if (w1[i] > w2[i])
        return (FALSE)
    }
  }
  if (length(w1) == length(w2)){
    i = length(w1) + 1
    while (TRUE){
      w1 <- append(w1, sample(0:1, 1))
      w2 <- append(w2, sample(0:1, 1))
      if (w1[i] < w2[i])
        return (TRUE)
      else if (w1[i] > w2[i])
        return (FALSE)
      i = i + 1
    }
  }
}

#(c)
RandQuickSort <- function(matrix) {
  if (nrow(matrix) <= 1) {
    return(matrix)
  }
  pivot_index <- sample(1:nrow(matrix), 1)
  pivot <- matrix[pivot_index, , drop = FALSE]
  S1 <- matrix[apply(matrix, 1, function(row) comp(row, pivot)), , drop = FALSE]
  S2 <- matrix[apply(matrix, 1, function(row) !comp(row, pivot)), , drop = FALSE]
  sorted_S1 <- RandQuickSort(S1)
  sorted_S2 <- RandQuickSort(S2)
  return(rbind(sorted_S1, pivot, sorted_S2))
}

#(d)
word_perm = function(n, k) {
  s <- matrix(0, nrow = n, ncol = k)
  for (i in 1:n){
    s[i, ] <- sample(0:1, k, replace = TRUE)
  }
  
  perm = RandQuickSort(s)
  return(perm)
}

k = 5
n = 20

word_perm(n, k)

#C2
#(a)
bipartite_cut = function(nodes, edges) {
  n = length(nodes)
  A <- sample(1:n, n/2)
  B <- setdiff(1:n, A)
  
  size = 0
  
  for (i in 1:n) {
    for (j in 1:n) {
      if (edges[i, j] == 1 && i!=j && i %in% A && j %in% B)
        size = size + 1
    }
  }
  
  return (size)
}

n = sample(10:30, 1)
nodes = c()
for (i in 1:n) {
  nodes <- append(nodes, i)
}

edges <- matrix(0, nrow = n, ncol = n)
for (i in 1:n) {
  edges[i, ] <- sample(0:1, n, replace = TRUE)
}

maxim = 0

for (i in 1:1000)
 maxim = max(bipartite_cut(nodes, edges), maxim)

print(maxim)

#(b)
#Pentru a crește șansele de a găsi o tăietură cât mai mare, putem să rulam
#algoritmul de mai multe ori și sa alegem cea mai mare tăietură găsită. Cu cât
#facem mai multe încercări, cu atât crește șansa de a găsi o tăietură mai mare.
