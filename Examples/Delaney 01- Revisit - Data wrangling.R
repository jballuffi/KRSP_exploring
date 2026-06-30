
#############

library(devtools)
library(dplyr)
library(RMySQL)
library(krsp)
library(tidyverse)
require(lubridate)
library(ggpubr)
theme_set(theme_bw())

#install.packages("devtools")
#devtools::install_github("KluaneRedSquirrelProject/krsp", force = TRUE)
#library(krsp)


#install.packages("remotes")
#library(remotes)
#remotes::install_github("KluaneRedSquirrelProject/krsp")
#library(krsp)

#####
#
# Things I need to do:
#    1) Bring in full dataset through 2025
#    2) Calculat part dates according to Andrew's update
#    3) Figure out new truncation year 
#
#####


#Connect to database
con <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                    dbname ="krsp",
                    user = Sys.getenv("krsp_user"),
                    password = Sys.getenv("krsp_password"))

#Import some of the tables
litter <- tbl(con, "litter")%>%
  select(id, food, fieldBDate, date1, grid, tagDt, squirrel_id, ln, yr)%>%
  collect() 

squirrel <- tbl(con, "squirrel")%>%
  select(id, census_date, gr, sex, trap_date)%>%
  collect()

juvenile <- tbl(con, "juvenile") %>%
  select(id, litter_id, sex, squirrel_id, weight, tagWT)%>%
  collect()

flastall2 <- tbl(con, "flastall2")%>%
  select(squirrel_id, byear, datee, bcert)%>%
  mutate(deathyear = YEAR(datee))%>%
  mutate(deathyear = ifelse(deathyear<2025,deathyear,NA_real_))%>%
  collect() 

hist(as.numeric(flastall2$deathyear), breaks=40)

# what was the last year that the entire cohort of moms lived full lives? 2018
temp <- flastall2 %>%
  group_by(byear, deathyear) %>%
  summarize(freq = n()) 


#make a connection to the supplementary database
con2 <- krsp_connect(host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp_suppl",
                     user = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password"))

#Import the midden table
suppl_tbl <- tbl(con2, "midden_cones")%>%
  select(id, year, grid, midden, locx, locy, sex, squirrel_id, Date, width, length, no_quad, total_new, total_old, total_cones, area_quad, total_newopen, total_newclosed)%>%
  collect()


#Join litter and juvenile tables
litter <- litter %>%
  rename(litter_id = id)

juvenile <- juvenile %>%
  select(id, litter_id, sex, weight, tagWT)

ls <- left_join(juvenile,litter, by = "litter_id")#squirrel_id = mother

#calculate litter size
ls1 <- ls %>%
  group_by(litter_id, grid) %>% 
  summarize(litter_size = n()) 

#Calculate juvenile growth rate
gr <- ls %>%
  mutate(nest_days = as.numeric(difftime(tagDt, date1, units = "days")),
         growth_rate = (tagWT - weight) / nest_days,
         birthdate = yday(fieldBDate)) %>%
  mutate(
    growth_rate = if_else(is.na(weight) | !between(weight, 1, 50),
                          NA_real_, growth_rate),
    growth_rate = if_else(is.na(tagWT) | !between(tagWT, 1, 100),
                          NA_real_, growth_rate),
    growth_rate = if_else(nest_days < 5, NA_real_, growth_rate),
    # exclude food manipulation experiments - want to keep these for now
    growth_rate = if_else(food %in% c(1, 4, 5, 6, 7, 9, 10, 13),
                          NA_real_, growth_rate),
    growth_rate = if_else(tagWT-weight==0,
                          NA_real_, growth_rate)) %>%
  select(-nest_days)

#Join gr and ls1 so that each juvenile has a row with its growth_rate and litter_size
gr <- gr %>%
  select(litter_id, fieldBDate, birthdate, squirrel_id, yr, weight, tagWT, growth_rate, ln)

ls2 <- left_join(gr, ls1, by = "litter_id") #squirrel_id = mother

#Calculate cones per cache
suppl_tbl2 <- suppl_tbl %>%
  mutate(total_new_2019 = total_newclosed + total_newopen,
         total_new = coalesce(total_new, total_new_2019),
         total_cones = total_old+total_new,
         total_cones2 = total_old+total_newclosed,
         cache_size_total = (total_cones/no_quad) * ((pi * (width/2) * (length/2))/area_quad),
         cache_size_total2 = (total_cones2/no_quad) * ((pi * (width/2) * (length/2))/area_quad),
         cache_size_new = (total_new/no_quad) * ((pi * (width/2) * (length/2))/area_quad),
         cache_size_new_closed = (total_newclosed/no_quad) * ((pi * (width/2) * (length/2))/area_quad),
         cache_size_new_open = (total_newopen/no_quad) * ((pi * (width/2) * (length/2))/area_quad),
         cache_size_old = (total_old/no_quad) * ((pi * (width/2) * (length/2))/area_quad),
         squirrel_id = as.numeric(as.character(squirrel_id))) %>%  #Needed to match variable types.  The as.character is needed otherwise the squirrel_ids get recoded as 1...n
  select(year, grid, midden, squirrel_id, cache_size_total, cache_size_old, cache_size_new)

#Calculate cone consumption and cache size available to each squirrel for reproduction
suppl_tbl3 <- suppl_tbl2 %>%  
  filter(!is.na(cache_size_total)) %>%
  group_by(squirrel_id, midden)  %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(time_since_last_sample = year - lag(year)) %>% #lag() returns the value of the preceeding row within group
  mutate(cone_consumption = ifelse(time_since_last_sample == 1, (lag(cache_size_total)-cache_size_old), NA))%>%
  mutate(cache_size_available = ifelse(time_since_last_sample == 1, lag(cache_size_total), NA)) %>%
  ungroup() 
  
# #Filter suppl_tbl2.5 to verify that cone_consumption is being calculated correctly
# test_df <-suppl_tbl2.5 %>%
#   #filter(!is.na(cache_size_available)) %>%
#   group_by(squirrel_id, midden) %>%
#   arrange(year, .by_group = TRUE)
# 
# #Accounts for squirrels with more than 1 midden
# suppl_tbl3 <- suppl_tbl2.5 %>%
#   group_by(squirrel_id, year, grid) %>% 
#   mutate (max_cone_consumption = max(cone_consumption)) %>%
#   mutate (max_cache_size_available = max(cache_size_available)) %>%
#   summarize(cache_size_available = sum(cache_size_available),
#             max_cache_size_available = max_cache_size_available,
#             cone_consumption = sum(cone_consumption),
#             max_cone_consumption = max_cone_consumption) %>%
#   ungroup() %>%
#   mutate(log_cone_consumption = ifelse(cone_consumption < 0, 0, log(cone_consumption+1)))%>%
#   mutate(log_cache_size_available = log(cache_size_available+1)) %>%
#   select(-grid)


#This is different from Andrew's 2020 squirrel meeting figure because cache_size_available is the availability in the previous year (when it would matter for that year's reproduction)
suppl_tbl3 %>% 
  ggplot(aes(as.character(year), log_cache_size_available)) +      
  geom_boxplot()+
  xlab("Year")+
  ylab("Log Cache size total")


#Join ls2 with midden2 to test how cone consumption relates to reproductive output
ls2 <- ls2 %>%
  rename(year = yr)

#ls2_suppl3 <- left_join(ls2, suppl_tbl3, by = c("squirrel_id", "year")) 


#Calculate age of mothers
age <- flastall2 %>%
  rename(birthyear = byear)

#Join to bigger dataset
ls2_suppl3_age <- left_join(ls2, age, by = "squirrel_id")%>%
  mutate(age = year - birthyear)%>%
  mutate(residual_life = deathyear - year)%>%
  mutate(lifespan= deathyear-birthyear)%>%
  mutate(survive_to_tp1=ifelse(lifespan>age,"1","0"))%>%
  group_by(squirrel_id)  %>%
  arrange(year, .by_group = TRUE)%>%
  ungroup()

#Needs some work to calculate primaparity and join. 
#calc_prim <- ls2_suppl3_age%>%
#  group_by(squirrel_id)  %>%
#  summarise(age_at_primaparity=ifelse(bcert=="Y", min(age), NA_real_))

#ls2_suppl3_age <- left_join(ls2_suppl3_age, calc_prim, by = "squirrel_id")



#Calculate population cone index for KL and SU
cone_counts_klsu<-tbl(con, "cones") %>%
  filter(Grid %in% c("SU", "KL"), Year>=1988, !is.na(NumNew)) %>%
  collect() %>%
  mutate(Year = as.numeric(Year), LocX = as.numeric(LocX), LocY = as.numeric(LocY), DBH = as.numeric(DBH), Per = as.numeric(Per), NumNew = as.numeric(NumNew)) %>% 
  mutate(total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)))

yearly_cones_klsu <- group_by(cone_counts_klsu, Year, Grid) %>% 
  summarize(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(log(NumNew + 1), na.rm = TRUE),
            total_cones = mean(total_cones, na.rm = TRUE))

yearly_cones_klsu <- yearly_cones_klsu %>% 
  mutate(cone_index_t = ifelse(is.finite(cone_index), cone_index, NA)) %>% 
  mutate(Year_tp1 = Year+1)

yearly_klsu_cone_temp<-yearly_cones_klsu %>% 
  select(Year, Grid, Year_tp1, cone_index_tm1=cone_index_t)

yearly_cones_klsu<-left_join(yearly_cones_klsu, yearly_klsu_cone_temp, by=c("Year" = "Year_tp1", "Grid")) %>% 
  select(Year, Grid, cone_index_t, cone_index_tm1)%>%
  mutate (mast = "n") %>%  
  mutate (mast = ifelse(Year==1993, "y", mast),
          mast = ifelse(Year==1998, "y", mast),
          mast = ifelse(Year==2005, "y", mast),
          mast = ifelse(Year==2010, "y", mast),
          mast = ifelse(Year==2014, "y", mast),
          mast = ifelse(Year==2019, "y", mast),
          mast = ifelse(Year==2022, "y", mast))%>%  
  mutate (mast_tm1 = "n") %>%  
  mutate (mast_tm1 = ifelse(Year==1994, "y", mast_tm1),
          mast_tm1 = ifelse(Year==1999, "y", mast_tm1),
          mast_tm1 = ifelse(Year==2006, "y", mast_tm1),
          mast_tm1 = ifelse(Year==2011, "y", mast_tm1),
          mast_tm1 = ifelse(Year==2015, "y", mast_tm1),
          mast_tm1 = ifelse(Year==2020, "y", mast_tm1),
          mast_tm1 = ifelse(Year==2023, "y", mast_tm1))

mean_yearly_cones <- yearly_cones_klsu %>%
  group_by(Year)%>%
  summarise(mean_cone_index_tm1 = mean(cone_index_tm1),
            mean_cone_index_t = mean(cone_index_t))%>%
  ungroup()




rm(yearly_klsu_cone_temp)


#Join to bigger dataset and filter to just KL & SU
ls2_suppl3_age_cones <- left_join(ls2_suppl3_age, yearly_cones_klsu, by = c("year" = "Year", "grid" = "Grid")) %>%
  filter(grid %in% c("SU", "KL"))



analyze_pop_level <- ls2_suppl3_age_cones %>%
  filter(!(is.na(litter_size)))%>%
  filter(!(is.na(growth_rate)))%>%
  filter(!(growth_rate<0))%>%
  mutate(age2 = age*age)%>%
  filter(year>1988)%>%
  mutate(year = as.character(year))
  
#testD<- analyze_pop_level %>%
#  filter(growth_rate<0)




#Get squirrel density data
connection_1 <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                              dbname ="krsp",
                              username = Sys.getenv("krsp_user"),
                              password = Sys.getenv("krsp_password")
)



#Importing midden census data
census_1<-tbl(connection_1, "dbamidden") %>% 
  select(reflo, squirrel_id, locX, locY, grid, date, Sex) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locX = loc_to_numeric(locX))

#Importing squirrel census data
census_2 <- tbl(connection_1, "census") %>%
  # be careful with case, database uses locX in census, but locX in dbaMidden
  # sql doesn't care since it's not case sensitive, but R does!
  select(reflo, squirrel_id, locx, locy, gr, census_date, sq_fate, sex) %>%
  filter(sq_fate != 7) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locx = loc_to_numeric(locx))

census_2 <- select (census_2, -sq_fate) %>% 
  rename (locX = locx,
          locY = locy,
          grid = gr,
          date = census_date,
          Sex = sex)

census_all<-bind_rows(census_1, census_2)%>% 
  mutate(grid = factor(grid),
         year = year(ymd(date)),
         month = month(ymd(date)),
         locY=as.numeric(locY),
         Sex=factor(Sex))

selected_grids <- c("KL", "SU")
census_all <- filter(census_all, grid %in% selected_grids, year>=1989) %>% 
  mutate(grid=factor(grid, levels = c("KL", "SU")))

selected_grids2 <- c("KL", "SU")

suchjokl_core_may_census<-filter(census_all, month==5, 
                                 grid %in% selected_grids2, 
                                 locX>=-0.2, locX<=20.8, 
                                 locY>=-0.2, locY<=20.8)#39.69ha

ag_core_may_census<-filter(census_all, month==5, 
                           grid =="AG", 
                           locX>=-0.2, locX<=20.8, 
                           locY>=-0.2, locY<=23.8) #45.36ha

ll2_core_may_census<-filter(census_all, month==5, 
                            grid =="LL", 
                            year>2005, 
                            locX>=-10.2, locX<=22.8, 
                            locY>=-0.2, locY<=8.8) #26.73ha

core_may_census_all<-bind_rows(suchjokl_core_may_census, ag_core_may_census, ll2_core_may_census)

grids_density <- group_by(core_may_census_all, year, grid) %>% 
  filter(!is.na(squirrel_id)) %>%
  summarise(spr_number = n_distinct (squirrel_id)) %>% 
  select (year, grid, spr_number)

# Summary of Spring Density
grid<-c("KL", "SU")
area<-c(39.69, 39.69)
area<-data.frame(grid, area)

grids_density <- left_join(grids_density, area, by = "grid") %>% 
  mutate(spr_density = spr_number/area)

mean_density <- grids_density %>%
  group_by(year)%>%
  summarize(mean_density= mean(spr_density))

rm(ag_core_may_census, area, census_1, census_2, census_all, connection_1, core_may_census_all, ll2_core_may_census, suchjokl_core_may_census, grid, selected_grids, selected_grids2)



#join analyze dataset with density dataset - be careful year gets joined correctly.
grids_density <- grids_density %>%
  mutate(year = as.factor(year))

analyze_density_pop <- left_join(analyze_pop_level, grids_density, by = c("year", "grid"))


rm(age, analyze_pop_level, con, con2, gr, grids_density, flastall2, juvenile, litter, ls, ls1, ls2, ls2_suppl3)
rm(ls2_suppl3_age, ls2_suppl3_age_cones, squirrel, suppl_tbl2, suppl_tbl3, suppl_tbl, yearly_cones_klsu, cone_counts_klsu)
rm(test_df, suppl_tbl2.5, mean_density)

save.image(file = "Wrangled data_revist.RData")
