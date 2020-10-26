    #neg.affect, problem.focused, emotion.focused, social.anxiety, disorganized, cog.perceptual
# social anxiety = affect + problem focused + emotion focused + disorganized + cog perceptual + constricted.affect
mean = 16.07; min=8; max=29

fixed = c(0, .17, .07, .03, .3, .3, .5)
random = c(.2, .2, .3, .3, .2, .2, .1)
vars = list(
   social.anxiety = c(11.39, estimate_sd(11.39, 4, 20), 0),
   neg.affect = c(16, estimate_sd(16.89, 10, 47), 0),
   problem.focused = c(16, estimate_sd(16.07, 8, 29), 0),
   emotion.focused = c(21, estimate_sd(16.07, 17, 26), 0),
   disorganized = c(24.43, estimate_sd(24.43, 8, 40), 0),
   cog.perceptual = c(33.97, estimate_sd(33.97, 14, 66), 0),
   constricted.affect = c(14.14, estimate_sd(14.14, 6, 29), 0),
   person_id = 1:45
)
tom_data = mixed_model(fixed, random, .5, clusters=45, n_per = c(8, 2), vars)
write.csv(tom_data, file="inst/data/schizo.csv", row.names=F)
require(lme4)
require(flexplot)
mod = lmer(social.anxiety~neg.affect + disorganized + (1 + neg.affect | person_id), data=tom_data)
visualize(mod, formula = social.anxiety~neg.affect | person_id)
