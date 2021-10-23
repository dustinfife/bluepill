context("mixed_model")

test_that("add_polynomials works", {
    pm = data.frame(intercept = rnorm(10), a = rnorm(10), b=rnorm(10), c= rnorm(10))
    coef_matrix = data.frame(intercept = rep(c(1,2), times=5),
                                a = rep(c(.1,.2), times=5),
                                b = rep(c(.4,.5), times=5),
                                c = rep(c(.5,.2), times=5))
    res = add_polynomials(predictor_matrix=pm, coef_matrix, list(c(1,2), c(2,2), c(.3,.3)))
    expect_true("a:a" %in% names(res$predictor_matrix))
    expect_true(all(res$predictor_matrix$a*res$predictor_matrix$a == res$predictor_matrix$`a:a`))
    res = add_polynomials(predictor_matrix=pm, coef_matrix)
    expect_true(all(res$coef_matrix == coef_matrix))

})

test_that("add_interactions works", {
    pm = data.frame(intercept = rnorm(10), a = rnorm(10), b=rnorm(10), c= rnorm(10))
    coef_matrix = data.frame(intercept = rep(c(1,2), times=5),
                             a = rep(c(.1,.2), times=5),
                             b = rep(c(.4,.5), times=5),
                             c = rep(c(.5,.2), times=5))
    res = add_interactions(predictor_matrix=pm, coef_matrix, list(1, 2, .3))
    expect_true("a:b" %in% names(res$predictor_matrix))
    expect_true(all(res$predictor_matrix$a*res$predictor_matrix$b == res$predictor_matrix$`a:b`))
})


test_that("interaction_polynomial_checks", {
    pm = data.frame(intercept = rnorm(10), a = rnorm(10), b=rnorm(10), c= rnorm(10))
    coef_matrix = data.frame(intercept = rep(c(1,2), times=5),
                             a = rep(c(.1,.2), times=5),
                             b = rep(c(.4,.5), times=5),
                             c = rep(c(.5,.2), times=5))
    expect_error(interaction_polynomial_checks(pm, coef_matrix, list(1, c(1,2), .3)))
    expect_error(interaction_polynomial_checks(pm, coef_matrix, list(1, c(1))))
    expect_error(interaction_polynomial_checks(pm, coef_matrix, list(1, c(1,2), .3)))
})
test_that("mixed_model works", {
    fixed = c(0, .2, .3)
    random = c(.1, .1, .1)
    vars = list(
        y = c(10, 3, 0),
        x = c(22, 7, 0),
        z = c(15, 2, 0),
        person = 1:5)
    set.seed(2323)
    d = mixed_model(fixed, random, sigma = .3, clusters=5, n_per = c(10, 3), vars=vars)
    expect_true(names(d)[1]=="y")
    expect_true(length(unique(d$person))==5)
    expect_true(abs(mean(d$y)-10)<10)

    fixed = c(0, .2)
    random = c(.1, .1)
    vars = list(
        y = c(10, 3, 0),
        x = c("a", "b", "c"),
        person = 1:5)
    set.seed(2323)
    d = mixed_model(fixed, random, sigma = .3, clusters=5, n_per = c(10, 3), vars=vars)
    expect_true(length(unique(d$x)) == 3)

    fixed = c(0, .2)
    random = c(.1, .1)
    vars = list(
        y = c(10, 3, 0),
        x = c("a", "b", "c"),
        person = c("tom", "jill", "joe"))
    set.seed(2323)
    d = mixed_model(fixed, random, sigma = .3, clusters=3, n_per = c(10, 3), vars=vars)
    expect_true(d$person[1]=="tom")

})

test_that("check_id_length works", {
    expect_error(check_id_length(3, list(id = 1:4)))
    expect_error(check_id_length(3, list(id = c(1,2,2,3))))
    expect_null(check_id_length(3, list(id = c(1,2,3))))
})
