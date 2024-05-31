#' ============== @Score_function
#'
# Q: How did we get this score? does LMER provide this directly?
lmerScore <- function(lmerModel) {
  X_mat <- lme4::getME(lmerModel, "X") # Fixed-effects design matrix
  residuals <- resid(lmerModel) # Residuals

  score <- t(X_mat) %*% (residuals / sigma(lmerModel)^2)
  return(score)
}

allScores <- function(data, dependentVar, voxels, groupVar) {
  score <- sapply(voxels, function(voxel) {
    formula <- as.formula(paste(dependentVar, " ~", voxel, "+ (1 |", groupVar, ")-1"))
    model <- lme4::lmer(formula, data = data)
    score <- lmerScore(model)
    return(score)
  })
}
