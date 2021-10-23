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
#' # add a model with interactions/polynomials
#' mixed_model(fixed, random, sigma = .3, clusters=15, n_per = c(11, 3), vars=vars,
#'       interaction = list(from=c(1,2), to=c(2,3), coef=c(.2, .1)),
#'       polynomials = list(var = c(3,4), degree = c(2,2), coef = c(.2, .2))
mixed_model = function(fixed, random, sigma, clusters, n_per, vars, interactions = NULL, polynomials = NULL){


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

    # add interactions and polynomials here
    interaction_effects = add_interactions(predictor_matrix, coef_matrix, interactions)
        predictor_matrix = interaction_effects$predictor_matrix
        coef_matrix = interaction_effects$coef_matrix
        head(predictor_matrix)
    polynomial_effects = add_polynomials(predictor_matrix, coef_matrix, polynomials)
        predictor_matrix = polynomial_effects$predictor_matrix
        coef_matrix = polynomial_effects$coef_matrix

    res_cor = sigma*sqrt(1-sum(fixed[-1]^2)^2)
    y = preds %>%
        purrr::map(function(x) coef_matrix[x] * predictor_matrix[x] + rnorm(N, 0, res_cor)) %>%
        Reduce("+", .) %>%
        cbind(., predictor_matrix %>% dplyr::select(-intercept))
    d = 1:(length(vars)-1) %>% purrr::map_dfc(function(x) rescale_vars(vars[[x]],y[,x])) %>%
       purrr::set_names(names(vars)[-length(vars)]) %>%
       data.frame()
    d$person = 1:length(total.n) %>% purrr::map(function(x) rep(vars[[length(vars)]][x], times=total.n[x])) %>% unlist
    names(d)[ncol(d)] = names(vars)[length(vars)]
    return(d)
}

add_polynomials = function(predictor_matrix, coef_matrix, polynomials=NULL) {

    if (is.null(polynomials)) return(list(predictor_matrix=predictor_matrix, coef_matrix=coef_matrix))
    interaction_polynomial_checks(prediction_matrix, coef_matrix, polynomials)
    # reformat so I can pass it in to the add_interactions function
    # currently only supports squared terms
    polynomials[[2]] = polynomials[[1]]
    return(add_interactions(predictor_matrix, coef_matrix, polynomials))
}

interaction_polynomial_checks = function(prediction_matrix, coef_matrix, list){

    # check if list is length of 3
    if (length(list) != 3) stop("Your interactions and/or polynomial list must have exactly 3 elements.")

    # make sure each object inside list has the same length
    if (length(list[[1]]) != length(list[[2]]) &
        length(list[[2]]) != length(list[[3]])) stop("Your interaction and/or polynomial lists are not the same length.")
}

add_interactions = function(predictor_matrix, coef_matrix, interactions=NULL){

    if (is.null(interactions)) return(list(predictor_matrix=predictor_matrix, coef_matrix=coef_matrix))

    # check for errors and stuff
    interaction_polynomial_checks(prediction_matrix, coef_matrix, interactions)

    col_name = paste0(names(predictor_matrix)[interactions[[1]]+1], ":", names(predictor_matrix)[interactions[[2]]+1])
    col_values = (predictor_matrix[,interactions[[1]]+1 ] * predictor_matrix[,interactions[[2]]+1 ])
    predictor_matrix[,col_name] = col_values

    # for now, all interactions/polynomials are fixed effects
    coef_matrix[,col_name] = matrix(interactions[[3]], nrow=nrow(coef_matrix), ncol=length(col_name), byrow=T)
    return(list(predictor_matrix=predictor_matrix, coef_matrix=coef_matrix))
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





