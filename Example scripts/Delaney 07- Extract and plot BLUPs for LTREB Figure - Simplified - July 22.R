
library(devtools)
library(dplyr)
library(tidyverse)
require(lubridate)
library(ggpubr)
#theme_set(theme_bw())

load(file = "BLUP_models_for_LTREB_Figs_Simplified_July22.RData")

rawP <- analyze_individual %>% 
  ggplot(aes(litter_size, growth_rate)) +
  geom_jitter(alpha =.25, width=0.2, height=0, show.legend=FALSE) +
  geom_line(aes(), stat="smooth", alpha=1, size =1.2, method = "lm", se = TRUE) +
  xlab("Litter size")+
  ylab("Pup growth rate")
rawP

summary(indiv_cone_index_model_2$VCV)
summary(indiv_null_model$VCV)

#Extract and plot BLUPs
#Code adapted from Tom Houslay's tutorial (#2)
#Code to extract mean BLUPs across all iterations

#Code to extract mean BLUPs across all iterations
BLUPData <- tibble(Trait = attr(colMeans(indiv_null_model$Sol), "names"),
                   Value = colMeans(indiv_null_model$Sol)) %>%
  separate(Trait, c("Trait","random_effect", "random_effect_value"), sep = "\\.", fill = "left") 

#filter to BLUPs for the random effect of year
BLUPData_year <- BLUPData %>%
  filter(random_effect == "year") %>%
  filter(Trait %in% c("traitgrowth_rate",
                      "traitlitter_size")) %>%
  select(random_effect_value, Trait, Value) %>%
  spread(Trait, Value)

#filter to BLUPs for the random effect of litter
BLUPData_litter <- BLUPData %>%
  filter(random_effect == "litter_id") %>%
  filter(Trait %in% c("traitgrowth_rate",
                      "traitlitter_size")) %>%
  select(random_effect_value, Trait, Value) %>%
  spread(Trait, Value)

#Next plot BLUP data
BLUP1 <- BLUPData_year %>% 
  mutate(mast = ifelse(random_effect_value=="2010", "y", "n"),
         mast = ifelse(random_effect_value=="2014", "y", mast),
         mast = ifelse(random_effect_value=="2019", "y", mast))%>%
  ggplot(aes(traitlitter_size, traitgrowth_rate)) +
  geom_point(aes(color=mast),alpha =0.4, show.legend=FALSE) +
  scale_color_manual(values = c("navy", "goldenrod"), labels = c("Non-mast ", "Mast"))+
  geom_line(aes(), stat="smooth", alpha=1, size =1.2, method = "lm", se = TRUE) +
  xlab("Litter size")+
  ylab("=")+
  xlim(-1.75,1.5)+
  ylim(-2.0, 2.4)+
  theme(axis.title.y = element_text(angle = 180, vjust=0.5, size=18))
 
BLUP1
BLUP2 <- BLUPData_litter %>% 
  ggplot(aes(traitlitter_size, traitgrowth_rate)) +
  geom_point(alpha =.4, show.legend=FALSE) +
  geom_line(aes(), stat="smooth", alpha=1, size =1.2, method = "lm", se = TRUE) +
  xlab("Litter size")+
  ylab("+")+ 
  xlim(-1.75,1.5)+
  ylim(-2.0, 2.4)+
  theme(axis.title.y = element_text(size=18))
BLUP2

BLUPfig <- ggarrange(rawP, BLUP1, BLUP2,
                     labels= c("A", "B", "C"), 
                     ncol=3, nrow = 1)
#7x3
BLUPfig
annotate_figure(BLUPfig,
                top = text_grob("  Raw data                                     Hierarchically partitioned data", 
                                color="black", face = "bold"))
                


BLUPData <- tibble(Trait = attr(colMeans(indiv_cone_index_model_2$Sol), "names"),
                   Value = colMeans(indiv_cone_index_model_2$Sol)) %>%
  separate(Trait, c("Trait","random_effect", "random_effect_value"), sep = "\\.", fill = "left") 

#filter to BLUPs for the random effect of year
BLUPData_year <- BLUPData %>%
  filter(random_effect == "year") %>%
  filter(Trait %in% c("traitgrowth_rate",
                      "traitlitter_size")) %>%
  select(random_effect_value, Trait, Value) %>%
  spread(Trait, Value)

#filter to BLUPs for the random effect of litter
BLUPData_litter <- BLUPData %>%
  filter(random_effect == "litter_id") %>%
  filter(Trait %in% c("traitgrowth_rate",
                      "traitlitter_size")) %>%
  select(random_effect_value, Trait, Value) %>%
  spread(Trait, Value)

#Next plot BLUP data
BLUP3 <- BLUPData_year %>% 
  ggplot(aes(traitlitter_size, traitgrowth_rate)) +
  geom_point(alpha =1, show.legend=FALSE) +
  geom_line(aes(), stat="smooth", alpha=1, size =1.2, method = "lm", se = TRUE) +
  
  xlab("Litter size")+
  ylab("Pup growth rate (g/d)")+
 ylim(-0.6,0.4)+
  xlim(-1, 1)
BLUP3

BLUP4 <- BLUPData_litter %>% 
  ggplot(aes(traitlitter_size, traitgrowth_rate)) +
  geom_point(alpha =.25, show.legend=FALSE) +
  geom_line(aes(), stat="smooth", alpha=1, size =1.2, method = "lm", se = TRUE) +
  xlab("Litter size")+
  ylab("Pup growth rate (g/d)")+
  xlim(-2.1,2.75)+
  ylim(-3, 2.25)
BLUP4

#5x9
BLUPfig <- ggarrange(BLUP1, BLUP3, BLUP2, BLUP4,
                     labels= c("", "", "", ""), 
                     ncol=2, nrow = 2)

annotate_figure(BLUPfig,
                top = text_grob("        Null model                        Accounting for resources", 
                                 color="black", face = "bold"),
                left = text_grob("Among litter                         Among year", 
                                 color="black", face = "bold", rot = 90))

