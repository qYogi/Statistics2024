densitate2 = function(p, n){
  v =  dpois(0:(n-1), p)
  barplot(v, space = 1 , main = "Ex 10", xlab = "x", ylab = "y")
  
}