#' ============== @Score_function
#'
# Q: How did we get this score? does LMER provide this directly?

allScores <- function(data, dependentVar, voxels, groupVar) {
  score <- sapply(voxels, function(voxel) {
    model1 <- Rfast::colrint.regbx(x = data[[voxel]], y = matrix(data[[dependentVar]]), id = data[[groupVar]])

    prediction <- model1$be[1] + model1$be[2] * data[[voxel]]
    # grab the random effect associated with the given group
    re_vec <- vapply(data[[groupVar]], function(group) model1$ranef[group], numeric(1))
    residuals <- data[[dependentVar]] - prediction - re_vec
    score <- t(data[[voxel]]) %*% (residuals / model1$info[[2]]^2)
    return(score)
  })
}
