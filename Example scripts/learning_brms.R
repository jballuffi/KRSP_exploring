#learning brms

library(brms)

#data 
head(epilepsy)

fit1 <- brm(count ~ zAge + zBase*Trt + (1|patient),
            data = epilepsy, family = poisson())

summary(fit1)

plot(fit1, variable = c("b_Trt1", "b_zBase"))

plot(conditional_effects(fit1, effects = "zBase:Trt"))


#predict responses (i.e. seizure counts) of a person in the treatment group (Trt = 1) 
# and in the control group (Trt = 0) with average age and average number of previous seizures

#build new data frame using averages
newdata <- data.frame(Trt = c(0, 1), zAge = round(mean(epilepsy$zAge)), zBase = round(mean(epilepsy$zBase)))

#predict off new data frame. We need to set re_formula = NA in order not to condition of the group-level effects.
predict(fit1, newdata = newdata, re_formula = NA)

#While the predict method returns predictions of the responses, the fitted method returns predictions
# of the regression line.
fitted(fit1, newdata = newdata, re_formula = NA)
#If we want to predict values of the original data, we can just leave the new data argument empty.


#Suppose, we want to investigate whether there is over dispersion in the model, that is residual 
#variation not accounted for by the response distribution. 
#For this purpose, we include a second group-level intercept that captures possible over dispersion.
fit2 <- brm(count ~ zAge + zBase * Trt + (1|patient) + (1|obs),
            data = epilepsy, family = poisson())

summary(fit2)

#leave out one cross validation
loo(fit1, fit2)





