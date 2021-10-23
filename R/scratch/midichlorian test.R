#devtools::install_github("dustinfife/bluepill")
require(bluepill)
require(tidyverse)

# darkness = anger + emotional_bonds + age_started + midichlorian
fixed = c(0, .4, .1, -.2, .1)
random = c(.1, .1, .3, 0, .5)
vars = list(
    darkness = c(5, 2, 1),
    anger = c(12, 3, 1),
    emotional_bonds = c(12, 3, 1),
    age_started = c(5, 3, 0),
    midichlorian = c(1200, 200, 0),
    jedi_id = paste0(paste0(1:95))
)
jedi_darkness = mixed_model(fixed, random, sigma = .3, clusters=95, n_per = c(75, 8), vars=vars,
                   interactions = list(c(1,2), c(2,3), c(.3, .1)),
                   polynomials  = list(c(3,4), c(2,2), c(.1, .1)))
usethis::use_data(jedi_darkness)
# require(lme4)
# mod = lmer(darkness~anger + emotional_bonds + age_started + midichlorian + anger:emotional_bonds + I(age_started^2) + (anger + emotional_bonds + midichlorian | jedi_id), data=data)
#
# mod = lmer(darkness~anger + emotional_bonds + anger:emotional_bonds + (anger + emotional_bonds | jedi_id), data=data)
#
# visualize(mod, plot="model")
# fixef(mod)
#
#
#
#
# require(tidyverse)
# fixed = c(0, .4, .1, -.2, .1)
# random = c(.1, .1, .3, 0, .5)
# vars = list(
#     darkness = c(0, 1, 3),
#     anger = c(0, 1, 3),
#     emotional_bonds = c(0, 1, 3),
#     age_started = c(0, 1, 3),
#     midichlorian = c(0, 1, 3),
#     jedi_id = paste0(paste0(1:995))
# )
# data = mixed_model(fixed, random, sigma = .3, clusters=995, n_per = c(65, 3), vars=vars)
# apply(data, 2, sd)
# require(lme4)
# mod = lmer(darkness~anger + emotional_bonds + age_started + midichlorian + (anger + emotional_bonds + midichlorian | jedi_id), data=data)
# fixef(mod) %>% round(digits=3)
# VarCorr(mod)
# summary(mod)
# flexplot::icc(mod)
#
#
# require(flexplot)
# require(lme4)
# head(math)
# mod = lmer(MathAch~SES + Sex + (SES | School), data=math)
# compare.fits(MathAch~SES | Sex, data=math, mod)
#
#
#
# require(bluepill)
# require(tidyverse)
#
# # darkness = anger + emotional_bonds + age_started + midichlorian
# fixed = c(0, .2, -.1, -.2, .1)
# random = c(.2, .6, .6, 0, .5)
# vars = list(
#     darkness = c(5, 2, 1),
#     anger = c(12, 3, 1),
#     emotional_bonds = c(12, 3, 1),
#     age_started = c(5, 3, 0),
#     midichlorian = c(1200, 200, 0),
#     jedi_id = paste0("jedi_", 1:95)
# )
# data = mixed_model(fixed, random, sigma = .3, clusters=95, n_per = c(15, 4), vars=vars)
# head(data)
