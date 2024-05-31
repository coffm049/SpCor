#' =============== @permutesd_score_stats-with-parallelization
# Load the necessary package
# Load the necessary package
permute_and_fit <- function(data, dependentVar, voxels, groupVar) {
  data_permuted <- data |>
    dplyr::mutate(Y_permuted = sample({{ dependentVar }}), .by = {{ groupVar }})

  permutedScore <- allScores(data_permuted, "Y", fixed_effects, "Z1")
  return(permutedScore)
}
