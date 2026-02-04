library('Countr')
library('MASS')

df = read.csv('nhl_games_stats_24-25_with_xG.csv')
mean(df$home_goals)
var(df$home_goals)


#Kotijoukkueen maalit Ilman selittäviä muuttujia

home_poiss_1 = glm(formula = home_goals ~ 1, data = df,
                      family = poisson)
summary(home_poiss_1)
exp(coef(home_poiss_1))

#sim_data = rpois(length(df$home_goals), lambda = exp(coef(home_poiss_1)))
#hist(df$home_goals, breaks = seq(-0.5, max(df$home_goals)+0.5, by=1), main = "Distribution of Home Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dpois(0:8, lambda = mean(df$home_goals)), type = "b", col = "red")

home_nb_1 = glm.nb(formula = home_goals ~ 1, data = df)
summary(home_nb_1)
exp(coef(home_nb_1))

home_weibull_1 = renewalCount(formula = home_goals ~ 1, data = df,
                                       dist = "weibull", computeHessian = FALSE,
                                       control = renewal.control(trace = 0))
summary(home_weibull_1)

home_gamma_1 = renewalCount(formula = home_goals ~ 1, data = df,
                            dist = "gamma", computeHessian = FALSE,
                            control = renewal.control(trace = 0))
summary(home_gamma_1)

home_gengamma_1 = renewalCount(formula = home_goals ~ 1, data = df,
                            dist = "gengamma", computeHessian = FALSE,
                            control = renewal.control(trace = 0))
summary(home_gengamma_1)


breaks = 0:max(df$home_goals)
pears = compareToGLM(poisson_model = home_poiss_1, breaks = breaks, nbinom_model = home_nb_1,
                     weibull = home_weibull_1, gamma = home_gamma_1, gengamma = home_gengamma_1)
library("dplyr")
frequency_plot(pears$Counts, pears$Actual,
               dplyr::select(pears, contains("_predicted")),
               colours = c("grey", "blue", "green", "lightblue","lightgreen", "black"))

#lmtest::lrtest(home_poiss_1, home_nb_1)
#lmtest::lrtest(home_poiss_1, home_weibull_1)
#lmtest::lrtest(home_poiss_1, home_gamma_1)

coef(home_poiss_1)
coef(home_nb_1)
coef(home_weibull_1)
coef(home_gamma_1)
AIC(home_poiss_1, home_weibull_1, home_nb_1, home_gamma_1, home_gengamma_1)

#Countr::chiSq_gof(home_poiss_1, breaks = breaks)
#Countr::chiSq_gof(home_weibull_1, breaks = breaks)
#Countr::chiSq_gof(home_nb_1, breaks = breaks)
#Countr::chiSq_gof(home_gamma_1, breaks = breaks)




#Kotijoukkuen maalit Selittävien (pelkästään kotijoukkueen) muuttujien kanssa:

home_poiss_2 = glm(formula = home_goals ~ home_shots_all 
                 + home_faceoff_wins + home_hits + home_takeaways
                 +  home_giveaways + home_blocks + home_penalties, data = df,
                 family = poisson)
summary(home_poiss_2)
coef(home_poiss_2)
exp(coef(home_poiss_2))

#hist(df$home_poiss_2, breaks = seq(-0.5, max(df$home_poiss_2)+0.5, by=1), main = "Distribution of Home Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dpois(0:8, lambda = mean(df$home_poiss_2)), type = "b", col = "red")


home_nb_2 = MASS::glm.nb(formula = home_goals ~ home_shots_all 
                             + home_faceoff_wins + home_hits + home_takeaways + home_giveaways
                             + home_blocks + home_penalties, data = df)
coef(home_nb_2)
exp(coef(home_nb_2))
summary(home_nb_2)

#hist(df$home_goals, breaks = seq(-0.5, max(df$home_goals)+0.5, by=1), main = "Distribution of Home Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dnbinom(0:8, size = home_nb_2$theta, mu = mean(df$home_goals)), type = "b", col = "blue")


home_weibull_2 = renewalCount(formula = home_goals ~ home_shots_all 
                               + home_faceoff_wins + home_hits + home_takeaways + home_giveaways
                               + home_blocks + home_penalties, data = df,
                               dist = "weibull", computeHessian = FALSE,
                               control = renewal.control(trace = 0))
#summary(home_weibull_2)
coef(home_weibull_2)
exp(coef(home_weibull_2))

home_gamma_2 = renewalCount(formula = home_goals ~ home_shots_all + home_faceoff_wins 
                                          + home_hits + home_takeaways + home_giveaways
                                          + home_blocks + home_penalties, data = df,
                                            dist = "gamma", computeHessian = FALSE,
                                            control = renewal.control(trace = 0))

home_gengamma_2 = renewalCount(formula = home_goals ~ home_shots_all + home_faceoff_wins 
                      + home_hits + home_takeaways + home_giveaways
                      + home_blocks + home_penalties, data = df,
                      dist = "gengamma", computeHessian = FALSE,
                      control = renewal.control(trace = 0))
#summary(home_gamma_2)
coef(home_gamma_2)
exp(coef(home_gamma_2))

breaks = 0:max(df$home_goals)

pears = compareToGLM(poisson_model = home_poiss_2, breaks = breaks, nbinom_model = home_nb_2,
                     weibull = home_weibull_2, gamma = home_gamma_2, gengamma = home_gengamma_2)
library("dplyr")
frequency_plot(pears$Counts, pears$Actual,
               dplyr::select(pears, contains("_predicted")),
               colours = c("grey", "blue", "green","lightblue", "lightgreen", "black"))

coef(home_poiss_2)
coef(home_nb_2)
coef(home_weibull_2)
coef(home_gamma_2)
AIC(home_poiss_2, home_weibull_2, home_nb_2, home_gamma_2, home_gengamma_2)






# Kotijoukkueen maalit selittävien muuttujien (molemmat joukkueet) kanssa

home_poiss_3 = glm(formula = home_goals ~ away_goals + home_shots_all + away_shots_all
                       + home_faceoff_wins + away_faceoff_wins + home_hits + away_hits + home_takeaways + away_takeaways
                       +  home_giveaways + away_giveaways + home_blocks + away_blocks + home_penalties + away_penalties
                      , data = df, family = poisson)

summary(home_poiss_3)
coef(home_poiss_3)
exp(coef(home_poiss_3))

#hist(df$home_poiss_3, breaks = seq(-0.5, max(df$home_poiss_3)+0.5, by=1), main = "Distribution of Home Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dpois(0:8, lambda = mean(df$home_poiss_3)), type = "b", col = "red")


home_nb_3 = MASS::glm.nb(formula = home_goals ~ away_goals + home_shots_all + away_shots_all
                          + home_faceoff_wins + away_faceoff_wins + home_hits + away_hits + home_takeaways + away_takeaways
                          +  home_giveaways + away_giveaways + home_blocks + away_blocks + home_penalties + away_penalties, data = df)
coef(home_nb_3)
exp(coef(home_nb_3))
summary(home_nb_3)

#hist(df$home_goals, breaks = seq(-0.5, max(df$home_goals)+0.5, by=1), main = "Distribution of Home Goals", xlab = "Number of Goals", ylab = "Frequency", freq = FALSE)
#lines(x = 0:8, y = dnbinom(0:8, size = home_nb_3$theta, mu = mean(df$home_goals)), type = "b", col = "blue")


home_weibull_3 = renewalCount(formula = home_goals ~ away_goals + home_shots_all + away_shots_all
                               + home_faceoff_wins + away_faceoff_wins + home_hits + away_hits + home_takeaways + away_takeaways
                               +  home_giveaways + away_giveaways + home_blocks + away_blocks + home_penalties + away_penalties, data = df,
                                  dist = "weibull", computeHessian = FALSE,
                                  control = renewal.control(trace = 0))
#summary(home_weibull_3)
coef(home_weibull_3)
exp(coef(home_weibull_3))

home_gamma_3 = renewalCount(formula = home_goals ~ away_goals + home_shots_all + away_shots_all
                             + home_faceoff_wins + away_faceoff_wins + home_hits + away_hits + home_takeaways + away_takeaways
                             +  home_giveaways + away_giveaways + home_blocks + away_blocks + home_penalties + away_penalties, data = df,
                                dist = "gamma", computeHessian = FALSE,
                                control = renewal.control(trace = 0))
#summary(home_gamma_3)
coef(home_gamma_3)
exp(coef(home_gamma_3))

home_gengamma_3 = renewalCount(formula = home_goals ~ away_goals + home_shots_all + away_shots_all
                               + home_faceoff_wins + away_faceoff_wins + home_hits + away_hits + home_takeaways + away_takeaways
                               +  home_giveaways + away_giveaways + home_blocks + away_blocks + home_penalties + away_penalties, data = df,
                                   dist = "gengamma", computeHessian = FALSE,
                                   control = renewal.control(trace = 0))

#summary(home_gengamma_3)
coef(home_gengamma_3)
exp(coef(home_gengamma_3))

breaks = 0:max(df$home_goals)

pears = compareToGLM(poisson_model = home_poiss_3, breaks = breaks, nbinom_model = home_nb_3,
                     weibull = home_weibull_3, gamma = home_gamma_3, gengamma = home_gengamma_3)
library("dplyr")
frequency_plot(pears$Counts, pears$Actual,
               dplyr::select(pears, contains("_predicted")),
               colours = c("grey", "blue", "green","lightblue", "lightgreen", "black"))

coef(home_poiss_3)
coef(home_nb_3)
coef(home_weibull_3)
coef(home_gamma_3)
AIC(home_poiss_3, home_weibull_3, home_nb_3, home_gamma_3)













