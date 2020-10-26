context("mixed_model")


test_that("mixed_model works", {
    fixed = c(0, .2)
    random = c(.1, .1)
    vars = list(
        y = c(10, 3, 0),
        x = c(22, 7, 0),
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
