library('Countr')
library('MASS')

df = read.csv('nhl_games_stats_24-25_with_xG.csv')
mean(df$away_goals)
var(df$away_goals)

#Vieras maalit Ilman selittäviä muuttujia

away_poiss_1 = glm(formula = away_goals ~ 1, data = df,
                   family = poisson)
summary(away_poiss_1)
exp(coef(away_poiss_1))

#sim_data = rpois(length(df$away_goals), lambda = exp(coef(away_poiss_1)))
#hist(df$away_goals, breaks = seq(-0.5, max(df$away_goals)+0.5, by=1), main = "Distribution of away Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dpois(0:8, lambda = mean(df$away_goals)), type = "b", col = "red")

away_g_nb = glm.nb(formula = away_goals ~ 1, data = df)
summary(away_g_nb)
exp(coef(away_g_nb))

away_weibull_1 = renewalCount(formula = away_goals ~ 1, data = df,
                              dist = "weibull", computeHessian = FALSE,
                              control = renewal.control(trace = 0))
summary(away_weibull_1)

away_gamma_1 = renewalCount(formula = away_goals ~ 1, data = df,
                            dist = "gamma", computeHessian = FALSE,
                            control = renewal.control(trace = 0))
summary(away_gamma_1)

away_gengamma_1 = renewalCount(formula = away_goals ~ 1, data = df,
                            dist = "gengamma", computeHessian = FALSE,
                            control = renewal.control(trace = 0))
summary(away_gengamma_1)

breaks = 0:max(df$away_goals)
pears = compareToGLM(poisson_model = away_poiss_1, breaks = breaks, nbinom_model = away_g_nb,
                     weibull = away_weibull_1, gamma = away_gamma_1, gengamma = away_gengamma_1)
library("dplyr")
frequency_plot(pears$Counts, pears$Actual,
               dplyr::select(pears, contains("_predicted")),
               colours = c("grey", "blue", "green", "lightblue", "lightgreen", "black"))

#lmtest::lrtest(away_poiss_1, away_g_nb)
#lmtest::lrtest(away_poiss_1, away_weibull_1)
#lmtest::lrtest(away_poiss_1, away_gamma_1)

coef(away_poiss_1)
coef(away_g_nb)
coef(away_weibull_1)
coef(away_gamma_1)
coef(away_gengamma_1)
exp(coef(away_poiss_1))
exp(coef(away_g_nb))
exp(coef(away_weibull_1))
exp(coef(away_gamma_1))
exp(coef(away_gengamma_1))

AIC(away_poiss_1, away_weibull_1, away_g_nb, away_gamma_1, away_gengamma_1)

#Countr::chiSq_gof(away_poiss_1, breaks = breaks)
#Countr::chiSq_gof(away_weibull_1, breaks = breaks)
#Countr::chiSq_gof(away_g_nb, breaks = breaks)
#Countr::chiSq_gof(away_gamma_1, breaks = breaks)
#Countr::chiSq_gof(away_gengamma_1, breaks = breaks)





#Vieras maalit Selittävien muuttujien kanssa:

away_poiss_2 = glm(formula = away_goals ~ away_shots_all 
                       + away_faceoff_wins + away_hits + away_takeaways
                       +  away_giveaways + away_blocks + away_penalties, data = df,
                       family = poisson)
summary(away_poiss_2)
coef(away_poiss_2)
exp(coef(away_poiss_2))

#hist(df$away_poiss_2, breaks = seq(-0.5, max(df$away_poiss_2)+0.5, by=1), main = "Distribution of away Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dpois(0:8, lambda = mean(df$away_poiss_2)), type = "b", col = "red")


away_nb_2 = MASS::glm.nb(formula = away_goals ~ away_shots_all 
                             + away_faceoff_wins + away_hits + away_takeaways + away_giveaways
                             + away_blocks + away_penalties, data = df)
coef(away_nb_2)
exp(coef(away_nb_2))
summary(away_nb_2)

#hist(df$away_goals, breaks = seq(-0.5, max(df$away_goals)+0.5, by=1), main = "Distribution of away Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dnbinom(0:8, size = away_nb_2$theta, mu = mean(df$away_goals)), type = "b", col = "blue")


away_weibull_2 = renewalCount(formula = away_goals ~ away_shots_all 
                                  + away_faceoff_wins + away_hits + away_takeaways + away_giveaways
                                  + away_blocks + away_penalties, data = df,
                                  dist = "weibull", computeHessian = FALSE,
                                  control = renewal.control(trace = 1))
#summary(away_weibull_2)
coef(away_weibull_2)
exp(coef(away_weibull_2))

away_gamma_2 = renewalCount(formula = away_goals ~ away_shots_all + away_faceoff_wins 
                                + away_hits + away_takeaways + away_giveaways
                                + away_blocks + away_penalties, data = df,
                                dist = "gamma", computeHessian = FALSE,
                                control = renewal.control(trace = 1))

away_gengamma_2 = renewalCount(formula = away_goals ~ away_shots_all + away_faceoff_wins 
                                + away_hits + away_takeaways + away_giveaways
                                + away_blocks + away_penalties, data = df,
                                dist = "gengamma", computeHessian = FALSE,
                                control = renewal.control(trace = 1))

breaks = 0:max(df$away_goals)

pears = compareToGLM(poisson_model = away_poiss_2, breaks = breaks, nbinom_model = away_nb_2,
                     weibull = away_weibull_2, gamma = away_gamma_2, gengamma = away_gengamma_2)
library("dplyr")
frequency_plot(pears$Counts, pears$Actual,
               dplyr::select(pears, contains("_predicted")),
               colours = c("grey", "blue", "green","lightblue", "lightgreen", "black"))

lmtest::lrtest(away_poiss_2, away_nb_2)
lmtest::lrtest(away_poiss_2, away_weibull_2)
lmtest::lrtest(away_poiss_2, away_gamma_2)

coef(away_poiss_2)
coef(away_nb_2)
coef(away_weibull_2)
coef(away_gamma_2)
AIC(away_poiss_2, away_weibull_2, away_nb_2, away_gamma_2, away_gengamma_2)







