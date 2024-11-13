
#' Apply extrapolated RBO (Webber) on two ranked lists
#' 
#' @param x An ordered vector
#' @param y An ordered vector
#' @param p A number between 0 and 1
#' @param k A number between 1 and the min length betewen x and y
#' @returns The RBO similarity measssure between x and y


rbo <- function(x, y, p, k = min(length(x), length(y))) {
  
  if(p>1 | p<0){
    cli::cli_abort("p must be a value between 0 and 1")
  }
  
  if(k<1 | k>min(length(x), length(y))){
    cli::cli_abort("k must be a value between 0 and {min(length(x), length(y))}")
  }
  
  A_d <- sapply(1:k,
                function(i) {
                  (intersect(x[1:i], y[1:i]) |> 
                     length()) / i
                })
  
  p_d <- sapply(1:k,
                function(i){
                  p^(i-1)
                })
  
  (A_d*p_d*(1-p)) |> 
    sum() +
    length(intersect(x[1:k],y[1:k]))/k * p^k
  
}


#' Caluclate the weight of evaluation at depth d for parameter d during RBO
#' 
#' @param p Parameter p between 0 and 1
#' @param d Depth of analysis, a natural number
#' @returns The % of weight accumulated at depth d when applyting RBO(p)


rbo_weight <- function(p, d) {
  
  if(d<1){
    cli::cli_abort("d must be a value greater than 0")
  }
  else if(d == 1){
    i=1
  } else{
    i = 1:(d-1)
  }
  
  
  1 - p ^ (d - 1) + (1 - p) / p * d * (log(1 / (1 - p)) -  sum(p^i/i))
  
}
