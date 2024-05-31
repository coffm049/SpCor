devtools::load_all()
args <- parseCLI()
# uncomment for debuggin
# args <- list("nsites" = 10, "nfamPerSite" = 10, "nmembersPerFamily" = 2, "nfeatures" = 5, "permutations" = 10, "cores" = 4, "beta" = 0, "scenario" = "test")

#' =========@data_generation
data <- data_generation(
  n_sites = args$nsites, n_fam_ps = args$nfamPerSite,
  n_ind_pf = args$nmembersPerFamily,
  n_vox = args$nfeatures,
  beta = args$beta
)

#' ============== @Score_function
fixed_effects <- paste0("X", 1:args$nfeatures)

# time1 <- Sys.time()
score <- allScores(data, "Y", fixed_effects, "Z1")
# time2 <- Sys.time()
# time2 - time1

#' =============== @permutesd_score_stats-with-parallelization
# time1 <- Sys.time()
scores <- parallel::mclapply(1:args$permutations, FUN = function(i) permute_and_fit(data = data, dependentVar = "Y", voxels = fixed_effects, groupVar = "Z1"), mc.cores = args$cores)
# time2 <- Sys.time()
# time2 - time1
scores_matrix <- t(do.call(cbind, scores))
scores_matrix <- as.matrix(apply(scores_matrix, 2, as.numeric))
# Compute the covariance matrix
covariance_matrix <- cov(scores_matrix)

#' =============================@burdenstat
p_value <- burdenP(score, scores, covariance_matrix)
write(p_value, file = paste0("simulations/", args$scenario, "/", args$scenario, ".pvalues"), append = TRUE)
print(p_value)
