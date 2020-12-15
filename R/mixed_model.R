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
#' @import flexplot purrr dplyr magrittr
#'
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


    check_errors(fixed, random, vars, clusters)

    id_names = vars[length(vars)] %>% unlist
    total.n = 1:clusters %>%
        purrr::map(function(x) assign(as.character(id_names[x]), round(rnorm(1, n_per[1], n_per[2]))))
    names(total.n) = id_names
    N = sum(unlist(total.n))
    preds = 1:(length(fixed)); names(preds) = names(vars)[-length(names(vars))]; names(preds)[1] = "intercept"

    # this matrix will have the scores on the predictor variables
    # these are just rnorm, except that fixed effects are consistent within cluster
    predictor_matrix = preds %>% purrr::map_dfc(function(y) total.n %>%
        purrr::map(function(x) fixed_or_random_prediction(x,y,random) %>% unlist) %>%
        stack() %>% dplyr::select(-ind)) %>%
        purrr::set_names(names(preds)) %>%
        dplyr::mutate(intercept = 1)
    coef_matrix = preds %>% purrr::map_dfc(function(y) total.n %>%
        purrr::map(function(x) rep(rnorm(1, fixed[y], random[y]), x) %>% unlist) %>%
        stack() %>% dplyr::select(-ind)) %>%
        purrr::set_names(names(preds))

    res_cor = sigma*sqrt(1-sum(fixed[-1]^2)^2)
    y = preds %>%
        purrr::map(function(x) coef_matrix[x] * predictor_matrix[x] + rnorm(N, 0, res_cor)) %>%
        Reduce("+", .) %>%
        cbind(., predictor_matrix %>% dplyr::select(-intercept))
    d = 1:ncol(y) %>% purrr::map_dfc(function(x) rescale_vars(vars[[x]],y[,x])) %>%
       purrr::set_names(names(vars)[-length(vars)]) %>%
       data.frame()
    d$person = 1:length(total.n) %>% purrr::map(function(x) rep(vars[[length(vars)]][x], times=total.n[x])) %>% unlist
    names(d)[ncol(d)] = names(vars)[length(vars)]
    return(d)
}

fixed_or_random_prediction = function(n,pred,random){

    if (random[pred]==0) return(rep(rnorm(1, 0, 1), n))
    return(rnorm(n, 0, 1))
}

check_variances = function(fixed) {
    explained_var = sum(fixed[-1]^2)^2
    if (explained_var>1) stop("The sum of your standardized coefficients is greater than one.")
    return(NULL)
}

rescale = function(x, new.mean, new.sd){
    m2 = new.mean+(x-mean(x, na.rm=T))*(new.sd/sd(x, na.rm=T))
    m2
}
"%>%" <- NULL
#' @importFrom magrittr "%>%"
rescale_vars = function(dat, x) {
    names(x) = names(dat)
    dat = unlist(dat)
    if (!is.character(dat[1])) {
        x = as.numeric(rescale(x, dat[1], dat[2]) %>% round(digits=dat[3]))
        return(x)
    }
    x = cut(x, breaks = length(dat), labels=dat)
    x
}

#expect_error(check_id_length(3, list(id = 1:4)))
#expect_error(check_id_length(3, list(id = c(1,2,2,3))))
#expect_null(check_id_length(3, list(id = c(1,2,3))))
check_id_length = function(clusters, vars) {
    clustersl = vars[[length(vars)]]
    length_clusters = length(clustersl)
    if (length(unique(clustersl)) != length_clusters)
        stop("It looks like you're using non-unique IDs for your cluster variable.")
    if (length_clusters != clusters)
        stop(paste0("The number of clusters (", clusters, ") is not equal to the length of your ID variable (", length_clusters, ")"))
    return(NULL)
}

check_var_vs_fixed_vs_random = function(fixed, random, vars) {
    fixed_l = length(fixed)
    random_l = length(random)
    vars_l = length(vars)

    if (fixed_l != random_l) stop("Your fixed and random vectors need to be the same length.")
    if (fixed_l != (vars_l-1)) stop("Your vars list needs to be the same length as your fixed/random vectors + 1.")
    return(NULL)
}

check_errors = function(fixed, random, vars, clusters) {
    check_var_vs_fixed_vs_random(fixed, random, vars)
    check_id_length(clusters, vars)
    check_variances(fixed)
    return(NULL)
}





