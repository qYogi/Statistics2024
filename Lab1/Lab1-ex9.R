densitate = function(p, n){
  v =  dgeom(0:(n-1), p)
  barplot(v, space = 0, main = "Ex 9", xlab = "x", ylab = "y")
  
}