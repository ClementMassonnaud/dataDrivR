# date: Tue Dec 15 13:55:56 2020
# author: cmasso6
#------------------------------------

#' Make and check samples from the intial dataset
#'
#' @param dt the dataset
#' @param sizes a named numeric vector of the samples sizes
#' @param N an integer, the number of samples to make
#' @param cores integer, the number of cores to use for parallel computing
#' @param x
#' @param y
#'
#' @return a named list of samples s, with s$rows the rows to take from the initial dataset. s$use whether the sample dataset can be used. s$to_remove non-varying variables to remove.
#'
#' @export
make_samples = function(params, sizes, N, cores) {

  dt = params$dt
  x  = params$x
  y  = params$y
  type = params$type
  
  row_numbers = 1:nrow(dt)
  samples = lapply(sizes, function(n) {
    parallel::mclapply(mc.cores = cores,
                       1:N,
                       function(N) {
                         list(rows = sample(row_numbers, n, replace = T))
                       })
  })
  names(samples) = names(sizes)

  
  samples = lapply(samples, function(samples_list) {
    check_datasets(dt, samples_list, x, y, type = type)
  })
}
