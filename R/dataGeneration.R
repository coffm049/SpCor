#' =========@data_generation
#' =========@ Model Y.ij =X1.ij' beta1 +X2.ij' beta2 + Z1.i' alpha.i + error
#'
data_generation <- function(n_sites, n_fam_ps, n_ind_pf, n_vox, beta) {
  N <- n_sites * n_fam_ps * n_ind_pf

  X <- matrix(rnorm(N * n_vox, 0, 1), nrow = N)

  Z1 <- rep(seq(1:n_sites), each = (n_fam_ps * n_ind_pf))

  beta <- c(rep(beta, n_vox), 1)
  alpha <- rnorm(n_sites, 0, 1)

  R.i <- cbind(X, alpha[Z1])
  Y <- R.i %*% beta + rnorm(N)

  data <- data.frame(X, Z1, Y = Y) %>%
    dplyr::rename_with(~ paste0("X", 1:n_vox), dplyr::starts_with("X"))

  return(data)
}

# Usage example:
# data <- data_generation(n_sites = 50, n_fam_ps = 10, n_ind_pf = 2, n_vox = 5)
