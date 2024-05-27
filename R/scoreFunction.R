
#'============== @Score_function
#'
# Q: How did we get this score? does LMER provide this directly?
lmerScore <- function(lmerModel) {
                  X_mat     = getME(lmerModel, "X")  # Fixed-effects design matrix
                  residuals = resid(lmerModel)   # Residuals
                  
                  sigma2_e = sigma(lmerModel)^2  # Residual variance
                  score = t(X_mat) %*% (residuals / sigma2_e)
                  return(score)
}

