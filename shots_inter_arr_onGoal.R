library("flexsurv")
library("MASS")

# Maaliakohden menneiden laukausten tapahtumavälien jakauma

#Without covariates

shots = read.csv("shots_data_onGoal.csv")
#shots = shots[0:3000,]
mean(shots$time_since_prev_shot)
median(shots$time_since_prev_shot)
var(shots$time_since_prev_shot)
max(shots$time_since_prev_shot)

exp_1 = flexsurvreg(formula = Surv(time_since_prev_shot) ~  1, data = shots,
                             dist = "exp")
exp_1


weib_1 = flexsurvreg(formula = Surv(time_since_prev_shot) ~  1, data = shots,
                             dist = "weibull")
weib_1

gamma_1 = flexsurvreg(formula = Surv(time_since_prev_shot) ~  1, data = shots,
                             dist = "gamma")
gamma_1


gengamma_1 = flexsurvreg(formula = Surv(time_since_prev_shot) ~  1, data = shots,
                             dist = "gengamma")
gengamma_1

coef(exp_1)
coef(weib_1)
coef(gamma_1)
coef(gengamma_1)
exp(coef(exp_1))
exp(coef(weib_1))
exp(coef(gamma_1))
exp(coef(gengamma_1))


AIC(exp_1, weib_1, gamma_1, gengamma_1)


x = (0:max(shots$time_since_prev_shot))
hist(shots$time_since_prev_shot, breaks = "FD", prob = TRUE, xlim = c(0,200), 
     main = "Distribution of inter-arrival times of shots",
     xlab = "Time since previous shot (seconds)")

exp_1
lines(x, dexp(x, exp(coef(exp_1))), col = "red", lwd = 2)

weib_1
lines(x, dweibull(x, shape =  exp(coef(weib_1)[1]), scale = exp(coef(weib_1)[2])), col = "blue", lwd = 2)

gamma_1
lines(x, dgamma(x, shape = exp(coef(gamma_1)[1]), rate = exp(coef(gamma_1)[2])), col = "darkgreen", lwd = 2)

gengamma_1
lines(x, dgengamma(x, mu = coef(gengamma_1)[1], sigma = exp(coef(gengamma_1)[2]), Q = coef(gengamma_1)[3]), col = "purple", lwd = 2)

legend("topright",
       legend = c("Exponential", "Weibull", "Gamma", "GenGamma"),
       col = c("red", "blue", "darkgreen", "purple"), lwd = 2)






#with covariates

exp_covs = flexsurvreg(formula = Surv(time_since_prev_shot) ~  home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = shots,
                                        dist = "exp")
exp_covs
exp(coef(exp_covs))


weib_covs = flexsurvreg(formula = Surv(time_since_prev_shot) ~ home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = shots,
                                        dist = "weibull")
weib_covs
coef(weib_covs)
exp(coef(weib_covs))

gamma_covs = flexsurvreg(formula = Surv(time_since_prev_shot) ~  home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = shots,
                                        dist = "gamma")
gamma_covs
coef(gamma_covs)
exp(coef(gamma_covs))

gengamma_covs = flexsurvreg(formula = Surv(time_since_prev_shot) ~  home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = shots,
                                        dist = "gengamma")
gengamma_covs
coef(gengamma_covs)
exp(coef(gengamma_covs)[2])






