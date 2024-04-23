outliers_iqr <- function(x)
{
  m = mean(x)
  s = sd(x)
  outliers = vector()
  j = 0
  for (i in 1:length(x))
  {
    if (x[i] < m-2*s | x[i] > m+2*s)
    {
      j = j + 1
      outliers[j] = x[i]
    }
  }
  return(outliers)
}

x = c(1, 91, 38, 72, 13, 27, 11, 85, 5, 22, 20, 19, 8, 17, 11, 15, 13, 23, 14, 17)
outliers_iqr(x)