#' =============================@burdenstat

burdenP <- function(score, scores, covariance_matrix) {
  T_burden <- (sum(score))^2 / sum(covariance_matrix)
  T_burden_permuted <- sapply(scores, function(V) (sum(unlist(V)))^2 / sum(covariance_matrix))

  p_value <- mean(T_burden_permuted >= T_burden)

  return(p_value)
}
