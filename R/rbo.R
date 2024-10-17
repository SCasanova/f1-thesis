
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
