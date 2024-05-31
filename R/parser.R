# make a command line argument parser with flags using argparse
# create a new argument parser
parseCLI <- function() {
  parser <- argparse::ArgumentParser()

  # add a flag for number of subjects to simulate
  parser$add_argument("--nsites", type = "integer", default = 10, help = "number of sites to simulate")
  parser$add_argument("--nfamPerSite", type = "integer", default = 10, help = "number of families per site")
  parser$add_argument("--nmembersPerFamily", type = "integer", default = 10, help = "number of members per family")
  parser$add_argument("--nfeatures", type = "integer", default = 10, help = "number of predictive features to simulate")
  parser$add_argument("--permutations", type = "integer", default = 10, help = "number of permutations to develop pvalue")
  parser$add_argument("--cores", type = "integer", default = 8, help = "number of cores to paralellize permutations")
  parser$add_argument("--scenario", type = "character", help = "filepath to write simulation output")
  parser$add_argument("--beta", type = "double", help = "simulated associate between predictors and outcome")

  # access the arguments of the parser
  args <- parser$parse_args()
  return(args)
}
