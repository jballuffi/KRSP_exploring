
library(devtools)
library(dplyr)
library(tidyverse)
require(lubridate)
library(ggpubr)
library(MCMCglmm)

load(file = "Wrangled data_revist.RData")

rm(mean_yearly_cones)
#load("BLUP_models_for_trivariate_paper.RData")

# Filter to years that all females that reproduced that year had fully expressed
#   lifespans

temp <- analyze_density_pop %>%
  group_by(year, deathyear) %>%
  summarize(freq=n())

# filter the dataset to 1989-2020; All females that reproduced in 2020 had fully
#    expressed lifespans

analyze_density_pop <- as.data.frame(analyze_density_pop) %>%
  filter(ln==1,
         year<2021)%>%
  filter(!(is.na(age)))%>%
  filter(!(is.na(growth_rate)))%>%
  filter(!(is.na(litter_size)))%>%
  mutate(year=as.character(year))%>%
  mutate(survive_to_tp1=as.numeric(survive_to_tp1))%>%
  mutate(litter_id=as.character(litter_id))%>%
  mutate(squirrel_id=as.character(squirrel_id))%>%
  filter(!(is.na(survive_to_tp1)))

#calculate sample sizes
length(unique(analyze_density_pop$squirrel_id))
length(unique(analyze_density_pop$litter_id))
length(unique(analyze_density_pop$year))



analyze_density_pop_mast <- as.data.frame(analyze_density_pop) %>%
  filter(mast=="y")%>%
  mutate(year=as.character(year))%>%
  mutate(litter_id=as.character(litter_id))%>%
  mutate(squirrel_id=as.character(squirrel_id))

analyze_density_pop_nonmast <- as.data.frame(analyze_density_pop) %>%
  filter(mast=="n")%>%
  mutate(year=as.character(year))%>%
  mutate(litter_id=as.character(litter_id))%>%
  mutate(squirrel_id=as.character(squirrel_id))


analyze_density_pop_mast_tm1 <- as.data.frame(analyze_density_pop) %>%
  filter(mast_tm1=="y")%>%
  mutate(year=as.character(year))%>%
  mutate(litter_id=as.character(litter_id))%>%
  mutate(squirrel_id=as.character(squirrel_id))

analyze_density_pop_nonmast_tm1 <- as.data.frame(analyze_density_pop) %>%
  filter(mast_tm1=="n")%>%
  mutate(year=as.character(year))%>%
  mutate(litter_id=as.character(litter_id))%>%
  mutate(squirrel_id=as.character(squirrel_id))

#analyze_individual<- analyze_density_pop %>%
#  filter(!(is.na(age)))%>%
#  filter(!(is.na(growth_rate)))%>%
#  filter(!(is.na(litter_size)))%>%
#  filter(log_cache_size_available > -1) %>%
#  mutate(year=as.character(year))

#analyze_individual<- as.data.frame(analyze_individual)


#Set chain values #trying the first set for the paper analyses
nitt <- 450000
thin <- 400
burnin <- 50000

# nitt <- 2200000
# thin <- 400
# burnin <- 200000
# 
# nitt <- 65000
# thin <- 400
# burnin <- 3000
# 
# nitt <- 1000
# thin <- 50
# burnin <- 250
# 
# #Test out with these for intermediate try model fitting
# nitt <- 202000
# thin <- 200
# burnin <- 2000





#### Run models with all 3 random effects
prior.bi3G = list(R = list(V = diag(c(1,0.0001),2,2), nu = 0.5, fix = 2),
                  G = list(G1 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2),
                           G2 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2),
                           G3 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2)))


#Base Model
m0.6 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + 
                   trait:scale(age) + 
                   trait:scale(age2) + 
                   trait:grid, 
                 random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                 rcov=~idh(trait):units,
                 data = analyze_density_pop,
                 nitt = nitt, thin = thin, burnin = burnin,
                 prior = prior.bi3G, 
                 family = c("gaussian", "gaussian"),
                 verbose = FALSE,
                 pr=TRUE)
summary(m0.6)
plot(m0.6$VCV)
plot(m0.6$Sol)




#Subset models
m0mast <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait-1 + 
                     trait:scale(age) + 
                     trait:scale(age2) + 
                     trait:grid, 
                   random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                   rcov=~idh(trait):units,
                   data = analyze_density_pop_mast,
                   nitt = nitt, thin = thin, burnin = burnin,
                   prior = prior.bi3G, 
                   family = c("gaussian", "gaussian"),
                   verbose = FALSE,
                   pr=TRUE)
summary(m0mast)
plot(m0mast$VCV)
plot(m0mast$Sol)

mean(m0nonmast$VCV[,"traitgrowth_rate:traitgrowth_rate.squirrel_id"])
median(m0nonmast$VCV[,"traitgrowth_rate:traitgrowth_rate.squirrel_id"]) 
HPDinterval(m0nonmast$VCV[,"traitgrowth_rate:traitgrowth_rate.squirrel_id"])

m0nonmast <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait-1 + 
                        trait:scale(age) + 
                        trait:scale(age2) + 
                        trait:grid, 
                      random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                      rcov=~idh(trait):units,
                      data = analyze_density_pop_nonmast,
                      nitt = nitt, thin = thin, burnin = burnin,
                      prior = prior.bi3G, 
                      family = c("gaussian", "gaussian"),
                      verbose = FALSE,
                      pr=TRUE)
summary(m0nonmast)
plot(m0nonmast$VCV)
plot(m0nonmast$Sol)







#Base mast_tm1 models
m0mast_tm1 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait-1 + 
                         trait:scale(age) + 
                         trait:scale(age2) + 
                         trait:grid, 
                       random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                       rcov=~idh(trait):units,
                       data = analyze_density_pop_mast_tm1,
                       nitt = nitt, thin = thin, burnin = burnin,
                       prior = prior.bi3G, 
                       family = c("gaussian", "gaussian"),
                       verbose = FALSE,
                       pr=TRUE)
summary(m0mast_tm1)
plot(m0mast_tm1$VCV)
plot(m0mast_tm1$Sol)

m0nonmast_tm1 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait-1 + 
                            trait:scale(age) + 
                            trait:scale(age2) + 
                            trait:grid, 
                          random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                          rcov=~idh(trait):units,
                          data = analyze_density_pop_nonmast_tm1,
                          nitt = nitt, thin = thin, burnin = burnin,
                          prior = prior.bi3G, 
                          family = c("gaussian", "gaussian"),
                          verbose = FALSE,
                          pr=TRUE)
summary(m0nonmast_tm1)
plot(m0nonmast_tm1$VCV)




#No interactions
m2 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait-1 + 
                 trait:scale(age) + 
                 trait:scale(age2) + 
                 trait:grid +
                 trait:mast + 
                 trait:scale(cone_index_tm1), 
               random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
               rcov=~idh(trait):units,
               data = analyze_density_pop,
               nitt = nitt, thin = thin, burnin = burnin,
               prior = prior.bi3G, 
               family = c("gaussian", "gaussian"),
               verbose = FALSE,
               pr=TRUE)
summary(m2)
plot(m2$VCV)
plot(m2$Sol)





# 3 response variables
prior.bi3G3D = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 0.002, fix = 2),
                    G = list(G1 = list(V = diag(3)/3, nu=0.002, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
                             G2 = list(V = diag(3)/3, nu=0.002, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
                             G3 = list(V = diag(3)/3, nu=0.002, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))

#prior.bi3G3D = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 1, fix = 2),
#                    G = list(G1 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
#                             G2 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
#                             G3 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))

#Good model for paper
trivariate_m <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), scale(residual_life)) ~ trait -1 + 
                           trait:scale(age) + 
                           trait:scale(age2) + 
                           trait:grid + 
                           trait:mast + 
                           trait:scale(cone_index_tm1), 
                      random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                      rcov=~idh(trait):units,
                      data = analyze_density_pop,
                      nitt = nitt, thin = thin, burnin = burnin,
                      prior = prior.bi3G3D, 
                      family = c("gaussian", "gaussian", "gaussian"),
                      verbose = FALSE,
                      pr=TRUE)
summary(trivariate_m)
plot(trivariate_m$VCV)
#plot(trivariate_m$Sol)


#mean-centered but not scaled
trivariate_m2 <- MCMCglmm(cbind(scale(growth_rate, scale=FALSE), scale(litter_size, scale=FALSE), scale(residual_life, scale=FALSE)) ~ trait -1 + 
                           trait:scale(age, scale=FALSE) + 
                           trait:scale(age2, scale=FALSE) + 
                           trait:grid + 
                           trait:mast + 
                           trait:scale(cone_index_tm1), 
                         random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                         rcov=~idh(trait):units,
                         data = analyze_density_pop,
                         nitt = nitt, thin = thin, burnin = burnin,
                         prior = prior.bi3G3D, 
                         family = c("gaussian", "gaussian", "gaussian"),
                         verbose = FALSE,
                         pr=TRUE)
summary(trivariate_m2)
plot(trivariate_m2$VCV)
#plot(trivariate_m2$Sol)



#Run models for Fig. 3
#Separate among-litter effects into mast vs nonmast and masttm1 vs nonmasttm1
analyze_density_pop <- analyze_density_pop %>%
  mutate(mast=as.factor(mast),
         mast_tm1=as.factor(mast_tm1))

prior = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 0.002, fix=2),
             G = list(G1 = list(V = diag(c(1,1,1),3,3,3), nu = 0.002),
                      G2 = list(V = diag(c(1,1,1),3,3,3), nu = 0.002),
                      G3 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))

trivariate_mastvnonmast <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), scale(residual_life)) ~ trait -1 + 
                           trait:scale(age) + 
                           trait:scale(age2) + 
                           trait:grid,
                           #trait:mast + 
                           #trait:scale(cone_index_tm1), 
                         random = ~us(trait:at.level(mast,1)):litter_id + us(trait:at.level(mast,2)):litter_id +
                                   us(trait):squirrel_id,
                         rcov=~idh(trait):units,
                         data = analyze_density_pop,
                         nitt = nitt, thin = thin, burnin = burnin,
                         prior = prior, 
                         family = c("gaussian", "gaussian", "gaussian"),
                         verbose = FALSE,
                         pr=TRUE)
summary(trivariate_mastvnonmast)
plot(trivariate_mastvnonmast$VCV)


trivariate_mast_tm1vnonmast_tm1 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), scale(residual_life)) ~ trait -1 + 
                           trait:scale(age) + 
                           trait:scale(age2) + 
                           trait:grid, 
                           #trait:mast + 
                           #trait:scale(cone_index_tm1), 
                         random = ~us(trait:at.level(mast_tm1,1)):litter_id + us(trait:at.level(mast_tm1,2)):litter_id +
                           us(trait):squirrel_id,
                         rcov=~idh(trait):units,
                         data = analyze_density_pop,
                         nitt = nitt, thin = thin, burnin = burnin,
                         prior = prior, 
                         family = c("gaussian", "gaussian", "gaussian"),
                         verbose = FALSE,
                         pr=TRUE)
summary(trivariate_mast_tm1vnonmast_tm1)
plot(trivariate_mast_tm1vnonmast_tm1$VCV)













##### #Make a dataset at the litter level and then try to run binary survival trivariate-response models
litter_level_df <- analyze_density_pop %>%
  group_by(litter_id, squirrel_id, mast, mast_tm1, year, survive_to_tp1, ln, grid, age, age2, cone_index_tm1)%>%
  summarize(growth_rate=mean(growth_rate),
            litter_size=mean(litter_size),
            residual_life=mean(residual_life))%>%
  mutate(mast=as.factor(mast),
         mast_tm1=as.factor(mast_tm1),
         year=as.factor(year),
         grid=as.factor(grid),
         survive_to_tp1=as.factor(survive_to_tp1),
         litter_id=as.factor(litter_id),
         squirrel_id=as.factor(squirrel_id),
         age=as.numeric(age),
         age2=as.numeric(age2),
         cone_index_tm1=as.numeric(cone_index_tm1),
         residual_life=as.numeric(residual_life))
litter_level_df<-as.data.frame(litter_level_df)

prior = list(R = list(V = diag(c(1,1,1),3,3,3), nu = 0.002, fix = 3),
                    G = list(G1 = list(V = diag(3), alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
                             G2 = list(V = diag(3), alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))

trivariate_binary_ll <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), survive_to_tp1) ~ trait -1 + 
                                trait:scale(age) + 
                                trait:scale(age2) + 
                                trait:grid + 
                                trait:mast + 
                                trait:scale(cone_index_tm1), 
                              random = ~us(trait):squirrel_id + us(trait):year,
                              rcov=~us(trait):units,
                              data = litter_level_df,
                              nitt = nitt, thin = thin, burnin = burnin,
                              prior = prior, 
                              family = c("gaussian", "gaussian", "categorical"),
                              verbose = FALSE,
                              pr=TRUE)
summary(trivariate_binary)
plot(trivariate_binary$VCV)








#####






# trivariate model with binomial survival
prior.bi3G3D = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 1, fix = 2),
                    G = list(G1 = list(V = diag(c(0.333,0.333,1),3,3,3), nu = 1, fix = 3),
                             G2 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
                             G3 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))
#Posterior distributions on this one are okay, but no significance within G covariances

prior.bi3G3D = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 0.002, fix = 2),
                    G = list(G1 = list(V = diag(c(1,1,1),3,3,3), nu = 0.002, fix = 3),
                             G2 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
                             G3 = list(V = diag(3)/3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))

#try fixing the G1 variance of survive_to_tp1 to 1.
prior.bi3G3D = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 1, fix = 2),
                    G = list(G1 = list(V = diag(c(1,1,1),3,3,3), nu = 1, fix = 3),
                             G2 = list(V = diag(3), nu=3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3),
                             G3 = list(V = diag(3), nu=3, alpha.mu=c(0,0,0), alpha.V=diag(3)*1000, n = 3)))
#Seems to mix well, but no significant G covariances

analyze_density_pop <- analyze_density_pop %>%
  mutate(survive_to_tp1=as.factor(survive_to_tp1))
#Does it matter if survive to tp1 is coded as numeric vs factor?
trivariate_binary <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), survive_to_tp1) ~ trait -1 + 
                           trait:scale(age) + 
                           trait:scale(age2) + 
                           trait:grid + 
                           trait:mast + 
                           trait:scale(cone_index_tm1), 
                         random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                         rcov=~idh(trait):units,
                         data = analyze_density_pop,
                         nitt = nitt, thin = thin, burnin = burnin,
                         prior = prior.bi3G3D, 
                         family = c("gaussian", "gaussian", "categorical"),
                         verbose = FALSE,
                         pr=TRUE)
summary(trivariate_binary)
plot(trivariate_binary$VCV)
#plot(trivariate_binary$Sol)




#Separate random effects into mast and nonmast
# 3 response variables

analyze_density_pop <- analyze_density_pop %>%
  mutate(mast=as.factor(mast),
         mast_tm1=as.factor(mast_tm1))

prior6G = list(R = list(V = diag(c(1,0.0001,0.0001),3,3,3), nu = 1, fix = 2),
                    G = list(G1 = list(V = diag(c(1,1,1),3,3,3), nu = 1, fix = 3),
                             G2 = list(V = diag(c(1,1,1),3,3,3), nu = 1, fix = 3),
                             G3 = list(V = diag(3), nu=1),
                             G4 = list(V = diag(3), nu=1)))

prior6G = list(R = list(V = diag(c(0.9998,0.0001,0.0001),3,3,3), nu = 0.002, fix = 2),
               G = list(G1 = list(V = diag(c(0.5,0.5,1),3,3,3), nu = 0.002, fix = 3),
                        G2 = list(V = diag(c(0.5,0.5,1),3,3,3), nu = 0.002, fix = 3),
                        G3 = list(V = diag(3)/3, nu=0.002),
                        G4 = list(V = diag(3)/3, nu=0.002)))

trivariate_by_mast_binary <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), survive_to_tp1) ~ trait -1 + 
                           trait:scale(age) + 
                           trait:scale(age2) + 
                           trait:grid + 
                           trait:mast + 
                           trait:scale(cone_index_tm1), 
                         random = ~us(trait:at.level(mast,1)):litter_id + us(trait:at.level(mast,2)):litter_id + 
                                   us(trait):squirrel_id + 
                                   us(trait):year,
                         rcov=~idh(trait):units,
                         data = analyze_density_pop,
                         nitt = nitt, thin = thin, burnin = burnin,
                         prior = prior6G, 
                         family = c("gaussian", "gaussian", "categorical"),
                         verbose = FALSE,
                         pr=TRUE)
summary(trivariate_by_mast_binary)
plot(trivariate_by_mast_binary$VCV)


#nonmast years
cov_xy <- trivariate_mastvnonmast$VCV[, "traitlitter_size:at.level(mast, 1):traitgrowth_rate:at.level(mast, 1).units"]
var_y <- trivariate_mastvnonmast$VCV[, "traitgrowth_rate:at.level(mast, 1):traitgrowth_rate:at.level(mast, 1).units"]
var_x <- trivariate_mastvnonmast$VCV[, "traitlitter_size:at.level(mast, 1):traitlitter_size:at.level(mast, 1).units"]

geom_mean_slope_nonmast_binary <- sign(cov_xy) * sqrt((cov_xy/var_x) * ((cov_xy/var_y)^-1)) 
OLS_slope = (cov_xy/var_x) 
plot(geom_mean_slope_nonmast_binary)
mean(geom_mean_slope_nonmast_binary)
mean(OLS_slope)
HPDinterval(geom_mean_slope_nonmast_binary, 0.95)




#mast years
cov_xy <- trivariate_mastvnonmast$VCV[, "traitlitter_size:at.level(mast, 2):traitgrowth_rate:at.level(mast, 2).units"]
var_y <- trivariate_mastvnonmast$VCV[, "traitgrowth_rate:at.level(mast, 2):traitgrowth_rate:at.level(mast, 2).units"]
var_x <- trivariate_mastvnonmast$VCV[, "traitlitter_size:at.level(mast, 2):traitlitter_size:at.level(mast, 2).units"]

geom_mean_slope <- sign(cov_xy) * sqrt((cov_xy/var_x) * ((cov_xy/var_y)^-1)) 
OLS_slope = (cov_xy/var_x) 
plot(geom_mean_slope)
mean(geom_mean_slope)
mean(OLS_slope)
HPDinterval(geom_mean_slope, 0.95)


geom_mean_slope_nonmast
geom_mean_slope_mast
#Make a plot with both geom mean slopes plotted

analyze_density_pop %>% 
  ggplot(aes(litter_size, growth_rate)) + 
  geom_point(alpha=0.2, show.legend=FALSE, size=0.75)+
  #geom_smooth(size = 1, method="lm", se=TRUE, show.legend=FALSE) +
  #xlab("Life-history trait 1")+
 #ylab("Life-history trait 2")+
  geom_abline(intercept = 3,
              alpha=0.05,
              slope = geom_mean_slope_nonmast_binary,
              colour = "navy",
              linetype = "dashed",
              size=1.0)+
  geom_abline(intercept = 2,
              alpha=0.02,
              slope = geom_mean_slope_mast_binary,
              colour = "navy",
              linetype = "solid",
              size=1.0)+
  #xlim(0.5,3.5)+
  #ylim(0.5,1.5)+
  theme_bw()



trivariate_by_mast_tm1_binary <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), survive_to_tp1) ~ trait -1 + 
                                 trait:scale(age) + 
                                 trait:scale(age2) + 
                                 trait:grid + 
                                 trait:mast + 
                                 trait:scale(cone_index_tm1), 
                               random = ~us(trait:at.level(mast_tm1,1)):litter_id + us(trait:at.level(mast_tm1,2)):litter_id + 
                                 us(trait):squirrel_id + 
                                 us(trait):year,
                               rcov=~idh(trait):units,
                               data = analyze_density_pop,
                               nitt = nitt, thin = thin, burnin = burnin,
                               prior = prior6G, 
                               family = c("gaussian", "gaussian", "categorical"),
                               verbose = FALSE,
                               pr=TRUE)
summary(trivariate_by_mast_tm1_binary)
plot(trivariate_by_mast_tm1_binary$VCV)

#save.image(file = "BLUP_models_for_trivariate_paper_fig3_models.RData")


length(unique(analyze_density_pop_mast_tm1$squirrel_id))


#save output
#save(m0mast, m0nonmast, m1, m1.2, file = "MCMCglmm_output.RData")
# to open - load("MCMCglmm_output.RData")
#saveRDS(m0mast, file = "m0mast.2.RDS")
#saveRDS(m0nonmast, file = "m0nonmast.2.RDS")
#saveRDS(m1, file = "m1")
#saveRDS(m1.2, file = "m1.2")

#save(m2, file = "MCMCglmm_output.2.RData")
#saveRDS(m2, file = "m2.RDS")
#save(survivem1, survivem2, file = "Survival bivariate output.RData")
#load("m0mast.2.RDS")
#load("MCMCglmm_output.RData")













#####Run bivariate residual lifespan models
prior.bi3G = list(R = list(V = diag(c(1,0.0001),2,2), nu = 0.002, fix = 2),
                  G = list(G1 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2),
                           G2 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2),
                           G3 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2)))

survivem1 <- MCMCglmm(cbind(scale(growth_rate), scale(residual_life)) ~ trait - 1 + trait:scale(age) + trait:scale(age2) + trait:grid 
                      + trait:mast + trait:scale(cone_index_tm1), 
                      random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                      rcov=~idh(trait):units,
                      data = analyze_density_pop,
                      nitt = nitt, thin = thin, burnin = burnin,
                      prior = prior.bi3G, 
                      family = c("gaussian", "gaussian"),
                      verbose = FALSE)
summary(survivem1)
plot(survivem1$VCV)
#plot(survivem1$Sol)


### convert residual lifespan to survive to next year or no.
analyze_reduced <- as.data.frame(analyze_density_pop) %>%
  filter(!(is.na(age)))%>%
  filter(!(is.na(growth_rate)))%>%
  filter(!(is.na(litter_size)))%>%
  filter(!(is.na(survive_to_tp1)))%>%
  group_by(litter_id, squirrel_id, year, grid, mast, survive_to_tp1)%>%
  summarise(litter_size=mean(litter_size),
            mean_growth=mean(growth_rate),
            residual_life=mean(residual_life),
            age=mean(age),
            age2=mean(age2),
            cone_index_t=mean(cone_index_t),
            cone_index_tm1=mean(cone_index_tm1),
            spr_density=mean(spr_density))%>%
  ungroup()%>%
  mutate(year=as.character(year))%>%
  mutate(litter_id=as.character(litter_id))%>%
  mutate(squirrel_id=as.character(squirrel_id))%>%
  mutate(survive_to_tp1=as.numeric(survive_to_tp1))%>%
  mutate()
analyze_reduced <- as.data.frame(analyze_reduced)

prior.bi2G.ls = list(R = list(V = diag(c(1,1),2,2), nu = 0.002),
                     G = list(G1 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2),
                              G2 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2)))

survivem2 <- MCMCglmm(cbind(scale(litter_size), scale(residual_life)) ~ trait - 1 + trait:scale(age) + trait:scale(age2) + trait:grid 
                      + trait:mast + trait:scale(cone_index_tm1), 
                      random = ~us(trait):squirrel_id + us(trait):year,
                      rcov=~idh(trait):units,
                      data = analyze_reduced,
                      nitt = nitt, thin = thin, burnin = burnin,
                      prior = prior.bi2G.ls, 
                      family = c("gaussian", "gaussian"),
                      verbose = FALSE)
summary(survivem2)
plot(survivem1$VCV)
#plot(survivem1$Sol)








prior.bi2G = list(R = list(V = diag(c(1,0.0001),2,2), nu = 0.002, fix = 2),
                  G = list(G1 = list(V = diag(2)/3, alpha.mu=c(0,0), alpha.V=diag(2)*1000, n = 2)))

graph_data <- as.data.frame(analyze_density_pop) %>%
  filter(year==2012)%>%
  filter(grid=="SU")%>%
  mutate(year=as.character(year))

mgraph <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1, 
                   random = ~us(trait):litter_id,
                   rcov=~idh(trait):units,
                   data = graph_data,
                   nitt = nitt, thin = thin, burnin = burnin,
                   prior = prior.bi2G, 
                   family = c("gaussian", "gaussian"),
                   verbose = FALSE)
summary(mgraph)
plot(mgraph$VCV)
plot(mgraph$Sol)








cor(analyze_individual$log_cache_size_available, analyze_individual$cone_index_tm1)


#trait:scale(cone_index_tm1) +

#Assessment of van Noordwijk and de Jong - It doesn't seem individual resources affect gr or ls.
m4 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait-1 + trait:scale(age) + trait:scale(age2) + trait:grid +
                 trait:scale(log_cache_size_available), 
               random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
               rcov=~idh(trait):units,
               data = analyze_individual,
               nitt = nitt, thin = thin, burnin = burnin,
               prior = prior.bi3G, 
               family = c("gaussian", "gaussian"),
               verbose = FALSE)
summary(m4)
plot(m4$VCV)
plot(m4$Sol)
















prior.univariate <- list(R = list(V = diag(1), n = 0.2),
                         G = list(G1 = list(V = diag(1), alpha.mu=0, alpha.V=diag(1)*1000, n = 1),
                                  G2 = list(V = diag(1), alpha.mu=0, alpha.V=diag(1)*1000, n = 1),
                                  G3 = list(V = diag(1), alpha.mu=0, alpha.V=diag(1)*1000, n = 1)))        














testd <- analyze_density_pop %>%
  filter(!is.na(age))%>%
  filter(!is.na(spr_density))%>%
  filter(!is.na(cone_index_t))%>%
  filter(!is.na(cone_index_tm1))%>%
  filter(!is.na(growth_rate))%>%
  filter(!is.na(litter_size))



m10 <- MCMCglmm(cbind(growth_rate, litter_size) ~ trait:age + trait:age2 + trait:grid + trait:spr_density -1, 
                random = ~us(trait):litter_id + us(trait):squirrel_id,
                rcov=~idh(trait):units,
                data = analyze_density,
                nitt = nitt, thin = thin, burnin = burnin,
                prior = prior.bi2G, 
                family = c("gaussian", "gaussian"),
                verbose = FALSE)
summary(m10)
plot(m10$VCV)
plot(m10$Sol)



#
 





#Make models for LTREB proposal BLUP Figs

#Base Model
pop_null_model <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid, 
                 random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                 rcov=~idh(trait):units,
                 data = analyze_density_pop,
                 nitt = nitt, thin = thin, burnin = burnin,
                 prior = prior.bi3G, 
                 family = c("gaussian", "gaussian"),
                 verbose = FALSE,
                 pr=TRUE)
summary(pop_null_model)
plot(pop_null_model$VCV)
plot(pop_null_model$Sol)


indiv_null_model <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid, 
                           random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                           rcov=~idh(trait):units,
                           data = analyze_individual,
                           nitt = nitt, thin = thin, burnin = burnin,
                           prior = prior.bi3G, 
                           family = c("gaussian", "gaussian"),
                           verbose = FALSE,
                           pr=TRUE)
summary(indiv_null_model)
plot(indiv_null_model$VCV)
plot(indiv_null_model$Sol)


pop_cone_index_model <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid + trait:scale(cone_index_tm1), 
                           random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                           rcov=~idh(trait):units,
                           data = analyze_density_pop,
                           nitt = nitt, thin = thin, burnin = burnin,
                           prior = prior.bi3G, 
                           family = c("gaussian", "gaussian"),
                           verbose = FALSE,
                           pr=TRUE)
summary(pop_cone_index_model)
plot(pop_cone_index_model$VCV)
plot(pop_cone_index_model$Sol)


indiv_cone_index_model <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid + trait:scale(cone_index_tm1), 
                             random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                             rcov=~idh(trait):units,
                             data = analyze_individual,
                             nitt = nitt, thin = thin, burnin = burnin,
                             prior = prior.bi3G, 
                             family = c("gaussian", "gaussian"),
                             verbose = FALSE,
                             pr=TRUE)
summary(indiv_cone_index_model)
plot(indiv_cone_index_model$VCV)
plot(indiv_cone_index_model$Sol)



indiv_cache_size_model <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid + trait:scale(log_cache_size_available), 
                                   random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                                   rcov=~idh(trait):units,
                                   data = analyze_individual,
                                   nitt = nitt, thin = thin, burnin = burnin,
                                   prior = prior.bi3G, 
                                   family = c("gaussian", "gaussian"),
                                   verbose = FALSE,
                                   pr=TRUE)
summary(indiv_cache_size_model)
plot(indiv_cache_size_model$VCV)
plot(indiv_cache_size_model$Sol)


indiv_consumption_model <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid + trait:scale(log_cone_consumption), 
                                   random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                                   rcov=~idh(trait):units,
                                   data = analyze_individual,
                                   nitt = nitt, thin = thin, burnin = burnin,
                                   prior = prior.bi3G, 
                                   family = c("gaussian", "gaussian"),
                                   verbose = FALSE,
                                   pr=TRUE)
summary(indiv_consumption_model)
plot(indiv_consumption_model$VCV)
plot(indiv_consumption_model$Sol)






#LTREB model - ARS, Survival, Cache Size
Trivariate_ARS_Survival_CacheSize <- MCMCglmm(cbind(scale(ARS), scale(survive_to_tp1), scale(log_cache_size_available)) ~ trait -1 + trait:scale(age) + trait:scale(age2), 
                              random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                              rcov=~idh(trait):units,
                              data = analyze_individual,
                              nitt = nitt, thin = thin, burnin = burnin,
                              prior = prior.bi3G, 
                              family = c("poisson","gaussian", "gaussian"),
                              verbose = FALSE,
                              pr=TRUE)
summary(indiv_consumption_model)
plot(indiv_consumption_model$VCV)
plot(indiv_consumption_model$Sol)



#LTREB model - ARS, GR, LS
Trivariate_ARS_GR_LS_null <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), scale(ARS)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid, 
                                    random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                                    rcov=~idh(trait):units,
                                    data = analyze_individual,
                                    nitt = nitt, thin = thin, burnin = burnin,
                                    prior = prior.bi3G, 
                                    family = c("gaussian", "gaussian", "poisson"),
                                    verbose = FALSE,
                                    pr=TRUE)
summary(indiv_consumption_model)
plot(indiv_consumption_model$VCV)
plot(indiv_consumption_model$Sol)
Trivariate_ARS_GR_LS_2 <- MCMCglmm(cbind(scale(growth_rate), scale(litter_size), scale(ARS)) ~ trait -1 + trait:scale(age) + trait:scale(age2) + trait:grid + trait:scale(log_cache_size_available), 
                                      random = ~us(trait):litter_id + us(trait):squirrel_id + us(trait):year,
                                      rcov=~idh(trait):units,
                                      data = analyze_individual,
                                      nitt = nitt, thin = thin, burnin = burnin,
                                      prior = prior.bi3G, 
                                      family = c("gaussian", "gaussian", "poisson"),
                                      verbose = FALSE,
                                      pr=TRUE)
summary(indiv_consumption_model)
plot(indiv_consumption_model$VCV)
plot(indiv_consumption_model$Sol)

