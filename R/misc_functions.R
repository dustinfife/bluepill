#' Estimate standard deviation from mean, min, and max
#'
#' @param mean The mean
#' @param min The minimum
#' @param max The standard deviation
#' @param num_sds Number of standard deviations before reaching min/max
#'
#' @return
#' @export
#'
#' @examples
#' # estimate sd when a distribution has a mean of 10, min of 5, max of 10.
#' # this will use 3 stanard deivations to cover the range
#' estimate_sd(10, 5, 15)
#' # this will use 2 stanard deivations to cover the range
#' estimate_sd(10, 5, 15, 2)
estimate_sd = function(mean, min, max, num_sds = 3){
    mean_diff = mean(max-mean, mean-min)
    mean_diff/num_sds
}
