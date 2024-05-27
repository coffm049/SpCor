#'=============== @permutesd_score_stats-with-parallelization
# Load the necessary package
# Load the necessary package
permute_and_fit <- function(j) {
  data_permuted <- data %>%
    group_by(Z1) %>%
    mutate(Y_permuted = sample(Y)) %>%
    ungroup()
  
  score_permuted  = sapply(1:n_vox,function(i){
    formula_permuted   =   as.formula(paste("Y_permuted ~", fixed_effects[[i]], "+ (1 | Z1)-1"))
    model_permuted     = lmer(formula_permuted, data = data_permuted)
    
    score_permuted = lmerScore(model_permuted)
    return(score_permuted)
  })
  return(score_permuted)
}