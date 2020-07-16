# extract convergence diagnostic autocorrelation from iteration object (e.g., chain means)
# requires packages 'purrr' and 'magrittr'/'dplyr', and the functions in 'SupplementAC.R'

# load supplementary file
source('Functions/SupplementAC.R')

# function for lag 1 AC for one or more imputation chains
ac_adapted <- function(x, ac_function = "ac") {
  # input: object with theta values (rows are iterations, columns are imputations),
  # and preferred way of calculating ac values
  # output: autocorrelations at each iteration in x, computed per imputation chain
  # and with max and mean across imputations
  
  # set function to apply
  if (ac_function == "acf") {
    function_to_apply <- acf_lag1
    min_it <- 2
  } else {
    function_to_apply <- ac_lag1
    min_it <- 3
  }
  
  # make input ready for mapping
  if (is.numeric(x)) {
    x <- as.data.frame(x)
  }
  it <- dim(x)[1] #nr of iterations
  M <- dim(x)[2] #nr of imputations
  
  # compute ac for each iteration (by definition only possible for t > 2)
  if (it < min_it) {
    ac <-
      matrix(NA, it, M + 2) %>% as.data.frame %>% set_names(paste0(ac_function, ".", c("mean", "max", names(x))))
  } else {
    ac <- map_dfr(min_it:it, function(i) {
      # compute ac
      x[1:i, ] %>% function_to_apply() %>% t() %>% data.frame(mean(.), max(.), .) %>% set_names(paste0(ac_function, ".", c("mean", "max", 1:M)))
    }) %>% rbind(matrix(NA, min_it-1, M+2, dimnames = list(NULL, names(.))), .) %>% cbind(iteration = 1:it, .)
  }
  
  # output
  return(ac)
}
