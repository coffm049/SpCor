

#'=========@data_generation
#'=========@ Model Y.ij =X1.ij' beta1 +X2.ij' beta2 + Z1.i' alpha.i + error
#'
library(dplyr)
library(lme4)
library(numDeriv)
library(parallel)
library(devtools)
load_all()
n_sites = 50
n_fam_ps = 10
n_ind_pf = 2
n_vox = 5
P= 10
# Set the number of cores to use
num_cores <- 4 
data <- data_generation(n_sites = 50, n_fam_ps = 10, n_ind_pf = 2, n_vox = 5)

#'============== @Score_function
#'
fixed_effects = paste0("X", 1:n_vox)

score  =sapply(1:n_vox,function(i){
                  formula   =   as.formula(paste("Y ~", fixed_effects[[i]], "+ (1 | Z1)-1"))
                  model     = lmer(formula, data = data)
                  score <- lmerScore(model)
                  return(score)
    })

#'=============== @permutesd_score_stats-with-parallelization
# Load the necessary package
# Load the necessary package

# Use mclapply to run the function in parallel
time1=Sys.time()
scores <- mclapply(1:P, permute_and_fit, mc.cores = num_cores)
time2=Sys.time()
time2-time1
scores_matrix  = t(do.call(cbind, scores))
scores_matrix  = as.matrix(apply(scores_matrix, 2, as.numeric))
# Compute the covariance matrix
covariance_matrix <- cov(scores_matrix)

# Print the covariance matrix
print(covariance_matrix)


#'=============================@burdenstat


T_burden          = (sum(score))^2 /sum(covariance_matrix)
T_burden_permuted =  sapply(scores, function(V) (sum(unlist(V)))^2 /sum(covariance_matrix) )

p_value           =  mean(T_burden_permuted >= T_burden)

delta =   ifelse (p_value<0.05,1,0)
