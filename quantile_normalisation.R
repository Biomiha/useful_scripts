quantile_fun <- function(x, low = 0.05, high = 0.95){
  quantiles <- quantile( x, c(low, high) )
  x[ x < quantiles[1] ] <- quantiles[1]
  x[ x > quantiles[2] ] <- quantiles[2]
  return(x)
}

range01 <- function(x){
  (x-min(x))/(max(x)-min(x))
}
