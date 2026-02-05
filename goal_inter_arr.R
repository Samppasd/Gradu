library("flexsurv")
library("MASS")

# Maalien tapahtumaväli jakauma

# Without covariates

goals = read.csv("goals_data.csv")
#goals = goals[0:1000,]
mean(goals$time_since_prev_goal)
median(goals$time_since_prev_goal)



exp_1_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~  1, data = goals,
                             dist = "exp")
exp_1_g
coef(exp_1_g)

weib_1_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~  1, data = goals,
                             dist = "weibull")
weib_1_g
coef(weib_1_g)

gamma_1_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~  1, data = goals,
                             dist = "gamma")
gamma_1_g
coef(gamma_1_g)

gengamma_1_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~  1, data = goals,
                             dist = "gengamma")
gengamma_1_g
coef(gengamma_1_g)


AIC(exp_1_g, weib_1_g, gamma_1_g, gengamma_1_g)

x = (0:max(goals$time_since_prev_goal))
hist(goals$time_since_prev_goal, breaks = "FD", prob = TRUE,
     main = "Distribution of inter-arrival times of goals",
     xlab = "Time since previous goal (seconds)")

exp_1_g
lines(x, dexp(x, exp(coef(exp_1_g))), col = "red", lwd = 2)

weib_1_g
lines(x, dweibull(x, shape =  exp(coef(weib_1_g)[1]), scale = exp(coef(weib_1_g)[2])), col = "blue", lwd = 2)

gamma_1_g
lines(x, dgamma(x, shape = exp(coef(gamma_1_g)[1]), rate = exp(coef(gamma_1_g)[2])), col = "darkgreen", lwd = 2)

gengamma_1_g
lines(x, dgengamma(x, mu = coef(gengamma_1_g)[1], sigma = exp(coef(gengamma_1_g)[2]), Q = coef(gengamma_1_g)[3]), col = "purple", lwd = 2)

legend("topright",
       legend = c("Exponential", "Weibull", "Gamma", "GenGamma"),
       col = c("red", "blue", "darkgreen", "purple"), lwd = 2)


#with covariates

exp_covs_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~ time_since_prev_shot + time_since_prev_event + home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = goals,
                                        dist = "exp")
exp_covs_g
exp(coef(exp_covs_g))

weib_covs_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~ time_since_prev_shot + time_since_prev_event + home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = goals,
                                        dist = "weibull")
weib_covs_g
coef(weib_covs_g)
exp(coef(weib_covs_g))

gamma_covs_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~ time_since_prev_shot + time_since_prev_event + home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = goals,
                                        dist = "gamma")

gamma_covs_g
coef(gamma_covs_g)
exp(coef(gamma_covs_g))

gengamma_covs_g = flexsurvreg(formula = Surv(time_since_prev_goal) ~ time_since_prev_shot + time_since_prev_event + home_leads_1 +
                                        home_leads_2 + home_leads_3plus + away_leads_1 + away_leads_2 +
                                        away_leads_3plus + home_team_5vs4_pp + away_team_5vs4_pp +
                                        period_2 + period_3, data = goals,
                                        dist = "gengamma")
gengamma_covs_g
coef(gengamma_covs_g)
exp(coef(gengamma_covs_g)[2])



