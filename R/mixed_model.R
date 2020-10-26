#' Generate simulated mixed model data
#'
#' @param fixed a vector equal to the number of variables + 1. Each entry indicates the
#' (standardized) value of the fixed effect. The first entry is for the intercept.
#' @param random a vector equal to the number of variables + 1. Each entry indicates the
#' (standardized) standard deviation for the random effects. The first entry is for the intercept.
#' @param sigma proportion of variance remaining unexplained at the residual level
#' @param clusters number of clusters
#' @param n_per a vector of length two, indicating the mean and standard deviation of
#' the number of observations within each cluster
#' @param vars a named list. The names correspond to the variable names (including the cluster names).
#' Each entry either contains a vector of three (indicating mean, standard deviation, and # of
#' digits to round), or a vector containing the unique values of categorical variables. Also, you must
#' include the name of the cluster. See examples.
#' @import flexplot purrr dplyr
#' @importFrom magrittr "%>%"
#'
#' @return
#' @export
#'
#' @examples
#' # simulate data where depression = stress + life_events + parental_depression + ses
#' fixed = c(0, .2, .5, .3, .2)
#' random = c(.1, .1, 0, .2, .1)
#' vars = list(
#'  depression = c(10, 3, 0),
#'  stress = c(22, 7, 0),
#'  life_events = c("no", "yes"),
#'  parental_depression = c("no", "mild", "moderate", "severe"),
#'  ses = c(55, 15, 0),
#'  therapist = paste0("Dr. ", LETTERS[1:15])
#' )
#' mixed_model(fixed, random, sigma = .3, clusters=15, n_per = c(11, 3), vars=vars)
mixed_model = function(fixed, random, sigma, clusters, n_per, vars){

    total.n = 1:clusters %>% purrr::map(function(x) round(rnorm(1, n_per[1], n_per[2])))
    N = sum(unlist(total.n))
    preds = 1:(length(fixed))
    predictors = preds[-length(preds)] %>% purrr::map_dfc(function(x) rnorm(N)) %>%
        dplyr::mutate(intercept = 1) %>%
        dplyr:::select(intercept, !starts_with("intercept"))
    coef_matrix = 1:length(total.n) %>%
        purrr::map(function(x) preds %>% purrr::map_dbl(function(y) rnorm(1, fixed[y], random[y])) %>%
                       purrr::set_names(c(paste0("b", 0:(length(preds)-1))))) %>%
        purrr::map2(unlist(total.n), function(x,y) matrix(x, nrow=y, ncol=length(preds), byrow=T)) %>%
        purrr::map(function(x) data.frame(x)) %>%
        purrr::map_dfr(function(x) x)
    y = preds %>%
        purrr::map(function(x) coef_matrix[x] * predictors[x]) %>%
        Reduce("+", .) %>%
        cbind(., predictors) %>%
        dplyr::select(-intercept)
    d = 1:ncol(y) %>% purrr::map_dfc(function(x) rescale_vars(vars[[x]],y[,x])) %>%
        purrr::set_names(names(vars)[-length(vars)]) %>%
        data.frame()
    d$person = 1:length(total.n) %>% purrr::map(function(x) rep(vars[[length(vars)]][x], times=total.n[x])) %>% unlist
    names(d)[ncol(d)] = names(vars)[length(vars)]
    return(d)
}

rescale_vars = function(dat, x) {
    names(x) = names(dat)
    dat = unlist(dat)
    if (!is.character(dat[1])) {
        x = as.numeric(flexplot:::rescale(x, dat[1], dat[2]) %>% round(digits=dat[3]))
        return(x)
    }
    x = cut(x, breaks = length(dat), labels=dat)
    x
}
