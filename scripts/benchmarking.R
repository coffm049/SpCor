library(lme4)
library(devtools)
library(tidyverse)
library(parallel)
load_all()


args <- parseCLI()
n_sites <- args$nsites
n_fam_ps <- args$nfamPerSite
n_ind_pf <- args$nmembersPerFamily
n_vox <- args$nfeatures
P <- args$permutations
n_cores <- args$cores
# n_sites <- 50
# n_fam_ps <- 10
# n_ind_pf <- 2
# n_vox <- 5
# P <- 10
# n_cores <- 4
time1 <- Sys.time()
data <- data_generation(n_sites = n_sites, n_fam_ps = n_fam_ps, n_ind_pf = n_ind_pf, n_vox = n_vox)
time2 <- Sys.time()
dataGenTime <- time2 - time1


#' ============== @Score_function
#'
time1 <- Sys.time()
fixed_effects <- paste0("X", 1:n_vox)
score <- allScores(data, "Y", fixed_effects, "Z1")
time2 <- Sys.time()
scoreTime <- time2 - time1

#' =============== @permutesd_score_stats-with-parallelization
# Load the necessary package
# Load the necessary package

# Use mclapply to run the function in parallel
time1 <- Sys.time()
scores <- mclapply(1:P, FUN = function(i) permute_and_fit(data = data, dependentVar = "Y", voxels = fixed_effects, groupVar = "Z1"), mc.cores = num_cores)
time2 <- Sys.time()
permuteTime <- time2 - time1

scores_matrix <- t(do.call(cbind, scores))
scores_matrix <- as.matrix(apply(scores_matrix, 2, as.numeric))
# Compute the covariance matrix
covariance_matrix <- cov(scores_matrix)

# Print the covariance matrix
print(covariance_matrix)


p_value <- burdenP(score, scores, covariance_matrix)
print(p_value)
