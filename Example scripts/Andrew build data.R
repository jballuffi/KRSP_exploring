# Build data for Red Squirrel Experiment paper
# Sept 2026

# install.packages("devtools")
# devtools::install_github("KluaneRedSquirrelProject/krsp")

#library (plyr)  #Causes conflicts with dplyr
library(tidyverse)
library (krsp)
library (lubridate)
library (MCMCglmm)

# This connection requires that the user's username and password are stored in their system settings.
# See KRSP GitHub site for instructions

#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

selected_grids <- c("AG", "CH", "JO", "KL", "LL", "SU")

#########
# Cones #
#########
cone_counts<-tbl(con, "cones") %>%
  filter(Grid %in% selected_grids, Year>=1988) %>%
  collect()  %>%
  mutate(Year = as.numeric(Year), 
         NumNew = as.numeric(NumNew),
         cone_index = log(NumNew + 1),
         total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)) # according to Krebs et al. 2012
  )

cones_grids_years <- cone_counts %>% 
  group_by(Year, Grid) %>% 
  summarise(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(cone_index, na.rm = TRUE)) %>% 
  mutate(Year_tp1 = Year+1,
         cone_index_t = ifelse(is.finite(cone_index), cone_index, NA))

#link in cones from the previous year
cone_temp<-cones_grids_years %>% 
  select(Grid, Year, Year_tp1, cone_index_tm1=cone_index_t)

cones_grids_years<-left_join(cones_grids_years, cone_temp, by=c("Grid", "Year" = "Year_tp1")) %>% 
  select(-Year_tp1, -Year.y)

# Manually code mast years
cones_grids_years<-cones_grids_years %>%
  mutate (mast = "n") %>% 
  mutate (mast = ifelse(Grid=="KL"&Year==1993, "y", mast),
          mast = ifelse(Grid=="LL"&Year==1993, "y", mast),
          mast = ifelse(Grid=="SU"&Year==1993, "y", mast),
          mast = ifelse(Grid=="KL"&Year==1998, "y", mast),
          mast = ifelse(Grid=="LL"&Year==1998, "y", mast),
          mast = ifelse(Grid=="SU"&Year==1998, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2005, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2005, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2005, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2010, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2010, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2010, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2010, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2010, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2010, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2014, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2014, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2014, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2014, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2014, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2014, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2019, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2019, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2019, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2019, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2019, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2022, "y", mast),
          mast = ifelse(Grid=="KL"&Year==2022, "y", mast),
          mast = ifelse(Grid=="LL"&Year==2022, "y", mast),
          mast = ifelse(Grid=="SU"&Year==2022, "y", mast),
          mast = ifelse(Grid=="CH"&Year==2022, "y", mast),
          mast = ifelse(Grid=="JO"&Year==2022, "y", mast),
          mast = ifelse(Grid=="AG"&Year==2022, "y", mast)) %>% 
  mutate (Exp = "c") %>% 
  mutate (Exp = ifelse(Grid=="AG"&Year>2004&Year<2018, "f", Exp),
          Exp = ifelse(Grid=="JO"&Year>2006&Year<2013, "f", Exp),
          Exp = ifelse(Grid=="LL"&Year>2005&Year<2013, "f", Exp)) %>% 
  mutate (EXP_label = 1) %>% 
  mutate (EXP_label = ifelse(Exp=="f", 19, EXP_label))

grids_cone_data <- cones_grids_years %>%
  ungroup() %>% 
  mutate(Year=as.integer(Year)) %>% 
  mutate(cones_t = cone_index_t) %>% 
  mutate(cones_tm1 = cone_index_tm1) %>% 
  mutate (cones_tm1 = ifelse(Year == 2005 & Grid == "AG", 1.045008523, cones_tm1)) %>% 
  #No cone data available for AG in 2004 so this needs to be added - assumed to be equal to cone index on LL
  select (Year, Grid, cones_t, cones_tm1, mast, Exp, EXP_label)


cone_counts_klsu<-tbl(con, "cones") %>%
  filter(Grid %in% c("SU", "KL"), Year>=1988, !is.na(NumNew)) %>%
  collect() %>%
  mutate(Year = as.numeric(Year),
         NumNew = as.numeric(NumNew)) %>% 
  mutate(total_cones = 1.11568 * exp(0.1681 + 1.1891 * log(NumNew + 0.01)))

yearly_cones_klsu <- group_by(cone_counts_klsu, Year) %>% 
  summarize(num_trees = sum(!is.na(NumNew)),
            cone_counts = mean(NumNew, na.rm = TRUE),
            cone_index = mean(log(NumNew + 1), na.rm = TRUE),
            total_cones = mean(total_cones, na.rm = TRUE))

yearly_cones_klsu <- yearly_cones_klsu %>% 
  mutate(cone_index_t = ifelse(is.finite(cone_index), cone_index, NA)) %>% 
  mutate(Year_tp1 = Year+1)

yearly_klsu_cone_temp<-yearly_cones_klsu %>% 
  select(Year, Year_tp1, cone_index_tm1=cone_index_t)

yearly_cones_klsu<-left_join(yearly_cones_klsu, yearly_klsu_cone_temp, by=c("Year" = "Year_tp1")) %>% 
  select(Year, num_trees, cone_counts, cone_index_t, cone_index_tm1, total_cones)%>% 
  mutate(mast=ifelse(Year %in% c(1993, 1998, 2005, 2010, 2014, 2019, 2022), "y", "n"))

KLSU_cone_data <- yearly_cones_klsu %>%
  select (year=Year, cones_t=cone_index_t, cones_tm1=cone_index_tm1)

rm(yearly_klsu_cone_temp, cone_temp, cone_counts)

###########
# Density #
###########
#Importing midden census data
census_1<-tbl(con, "dbamidden") %>% 
  select(reflo, squirrel_id, locX, locY, grid, date, Sex) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locX = loc_to_numeric(locX))

#Importing squirrel census data
census_2 <- tbl(con, "census") %>%
  # be careful with case, database uses locX in census, but locX in dbaMidden
  # sql doesn't care since it's not case sensitive, but R does!
  select(reflo, squirrel_id, locx, locy, gr, census_date, sq_fate, sex) %>%
  filter(sq_fate != 7) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locx = loc_to_numeric(locx))

census_2 <- census_2 %>% 
  select (-sq_fate) %>% 
  dplyr::rename (locX = locx,
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

census_all <- filter(census_all, grid %in% selected_grids, year>=1989) %>% 
  mutate(grid=factor(grid, levels = c("AG", "CH", "JO", "KL", "LL", "SU")))

selected_grids2 <- c("CH", "JO", "KL", "SU")

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
  dplyr::summarise(spr_number = n_distinct (squirrel_id)) %>% 
  dplyr::select (year, grid, spr_number)

# Summary of Spring Density
grid<-c("KL", "SU", "CH", "JO", "LL", "AG")
area<-c(39.69, 39.69, 39.69, 39.69, 26.73, 45.36)
area<-data.frame(grid, area)

grids_density <- left_join(grids_density, area, by = "grid") %>% 
  mutate(spr_density = spr_number/area)

density <- grids_density %>% 
  select(year, grid, spr_density)


# Annual density measures for KL and SU only
SUKL_yearly_density<-core_may_census_all %>% 
  filter(!is.na(squirrel_id), grid %in% c("KL", "SU")) %>%
  group_by(year) %>% 
  dplyr::summarise(spr_number = n_distinct (squirrel_id)) %>% 
  arrange(year) %>% 
  mutate(spr_density = spr_number/(39.69+39.69))

KLSU_density <- SUKL_yearly_density %>%
  select (year, spr_density)

rm(ag_core_may_census, area, census_1, census_2, census_all, core_may_census_all, ll2_core_may_census, suchjokl_core_may_census, selected_grids)




#################
# Other data #
#################

# Generations in the experiment
fla<-read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/generations3.csv", header=T, stringsAsFactors = FALSE) %>% 
  select (squirrel_id, generation, maternal_generation) %>% 
  mutate(squirrel_id=as.integer(squirrel_id)) 

# Whether the squirrel owned a bucket
food_add <- read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/fed_squirrels.csv") %>% 
  filter(!is.na(squirrel_id_spring)) %>% 
  mutate(bucket = "Y") %>% 
  select(squirrel_id = squirrel_id_spring, year, bucket)

part_dates<-read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/Parturition-Dates/refs/heads/main/part_dates.csv") %>% 
  select(litter_id, part_date)


#################################
# Juvenile Data - results table #
#################################

flastall <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL", "CH", "AG", "LL", "JO")) %>% 
  select(squirrel_id, gr, byear=byear, dam_id, sire_id, bcert=bcert) %>% 
  collect()

juvenile <- tbl(con, "juvenile") %>% 
  select(squirrel_id, litter_id, sex, weight, tagwt = tagWT) %>% 
  collect() %>% 
  mutate (litter_id = as.integer(litter_id))

##################
## Known errors ##
##################
#These squirrels were listed as litter_id=5032 but this is incorrect. They are actually from a second litter that I will label as 99999
juvenile<-juvenile %>%
  mutate (litter_id = ifelse(squirrel_id==13172, 99999, litter_id),
          litter_id = ifelse(squirrel_id==13173, 99999, litter_id),
          litter_id = ifelse(squirrel_id==13174, 99999, litter_id),
          litter_id = ifelse(squirrel_id==13175, 99999, litter_id))
# A litter record for 9999 added below.  Not so relevant here because this would then be a second litter
#################################################################################################################

# litter level information
litter <- tbl(con, "litter") %>% 
  collect() %>% 
  # exclusions
  filter(yr >= 1989,
         !grid %in% c("EN", "FL", "SX"),
         !(grid == "AG" & yr < 2004),
         !(grid == "LL" & yr >= 1997 & yr <= 2003),
         !(grid == "LL" & yr >= 2012),
         !(grid == "JO" & yr >= 2012),
         !(grid == "CH" & yr >= 2012)) %>% 
  select(litter_id = id,
         year = yr, field_bdate = fieldBDate, n1_date = date1, tag_date = tagDt,
         mother_id = squirrel_id, grid, locx, locy, ln, food)

###########################
## Known errors as above ##
###########################
temp_litter<-data.frame(litter_id=99999, year = 2011, field_bdate = NA, n1_date = "2011-05-22", tag_date = "2011-06-17", mother_id = 11486, grid = "CH", locx = NA, locy = NA, ln = 2, food = 0)
litter<-bind_rows(litter, temp_litter) %>% 
  mutate (litter_id = as.integer(litter_id))

rm(temp_litter)
##############################################################################################################################


results <- inner_join(juvenile, litter, by = "litter_id")
results <- collect(results)
rm(litter, juvenile)

# flastall - juvenile
flastall_juv <- tbl(con, "flastall2") %>% 
  select(squirrel_id, date_end = datee, fate_end = f2) %>% 
  collect()
results <- left_join(results, flastall_juv, by = "squirrel_id")
rm(flastall_juv)

# flastall - mother
flastall_mom <- tbl(con, "flastall2") %>% 
  select(mother_id = squirrel_id, mother_byear = byear, mother_bcert = bcert) %>% 
  collect()
results <- left_join(results, flastall_mom, by = "mother_id")
rm(flastall_mom)


# clean up variables
results <- results %>% 
  # ids imported as numeric, convert back to integer
  mutate(mother_id = as.integer(mother_id),
         squirrel_id = as.integer(squirrel_id),
         litter_id = as.integer(litter_id)) %>% 
  # convert locs to numeric values
  mutate(locx = loc_to_numeric(locx),
         locy = loc_to_numeric(locy)) %>% 
  # convert dates from character to date class
  mutate(field_bdate = ymd(field_bdate),
         n1_date = ymd(n1_date),
         tag_date = ymd(tag_date),
         date_end = ymd(date_end)) %>% 
  #calculates mom age
  mutate(dam_age = ifelse(mother_bcert=="Y", year - mother_byear, NA_real_))


results <- results %>%
  left_join(part_dates, by = "litter_id") %>% 
  rename(pd_bdate = part_date)

# calculate litter size
results <- results %>% 
  group_by(litter_id) %>% 
  dplyr::summarize(litter_size = n()) %>% 
  inner_join(results, ., by = "litter_id")
# add additional fields
results <- results %>% 
  mutate(
    # growth rate
    nest_days = as.numeric(difftime(tag_date, n1_date, units = "days")),
    growth = (tagwt - weight) / nest_days,
    # calculate birth date as julian day
    bdate_temp = yday(field_bdate),
    # calculate age as of last sighting
    age_last = as.integer(difftime(date_end, field_bdate, units = "days")))
# there are various exclusions for which growth rate shouldn't be calculated

results <- results %>% 
  mutate(bdate = coalesce(pd_bdate, bdate_temp))


results <- results %>% 
  mutate(
    # n1 weight between 0 and 50 g
    growth = if_else(is.na(weight) | !between(weight, 1, 50),
                     NA_real_, growth),
    # tag weight between 0 and 100 g
    growth = if_else(is.na(tagwt) | !between(tagwt, 1, 100),
                     NA_real_, growth),
    # tag date at least 5 days after n1 date
    growth = if_else(nest_days < 5, NA_real_, growth),
    # exclude food manipulation experiments - want to keep these for now
    #growth = if_else(food %in% c(1, 4, 5, 6, 7, 9, 10, 13),
    #                 NA_real_, growth),
    growth = if_else(tagwt-weight==0,
                     NA_real_, growth)) %>% 
  select(-nest_days)


results <- results %>% 
  mutate(
    # did the squirrel survive to 200 days?
    survived_200d = !(age_last < 200))

# remove fitness for LL 2012 and CH 2011
results <- results %>% 
  mutate(survived_200d = ifelse(grid == "LL" & year == 2012, NA, survived_200d),
         survived_200d = ifelse(grid == "CH" & year == 2011, NA, survived_200d),
         survived_200d = ifelse(year == max(year), NA, survived_200d))

# output
# convert T/F to 1/0
results<-results %>% 
  mutate(survived_200d = as.integer(survived_200d)) 

#  Add in cones and density
results<-results %>% 
  left_join(density, by=c("year", "grid")) %>% 
  left_join(grids_cone_data, by=c("year"="Year", "grid"="Grid")) 

temp_cones<-grids_cone_data %>% 
  select(year = Year, grid = Grid, mom_mast=mast, mom_cones_t = cones_t)

results<- results%>% 
  left_join(temp_cones, by = c("mother_byear"="year", "grid")) 

results<-results %>% 
  # left join will give NAs when squirrel/year isn't in food_add
  left_join(distinct(food_add), by = c("mother_id"="squirrel_id", "year")) %>% 
  #distinct(squirrel_id, .keep_all = TRUE) %>% 
  #This is needed because otherwise juvenile rows will be duplicated for each match in the join.  This is a potentially big problem!
  # fill NAs with "0"
  mutate(dam_bucket = coalesce(bucket, "0")) %>% 
  mutate(dam_bucket2 = if_else(year %in% c(2011, 2015)&grid %in% c("AG", "JO", "LL"), "1", dam_bucket)) %>% 
  # 2011 and 2015 have dam_bucket==0 and dam_bucket2==1
  mutate(dam_bucket = if_else(dam_bucket=="Y", "1", dam_bucket)) %>% 
  mutate (dam_bucket = as.numeric(dam_bucket)) %>% 
  mutate(dam_bucket2 = if_else(dam_bucket2=="Y", "1", dam_bucket2)) %>% 
  mutate (dam_bucket2 = as.numeric(dam_bucket2))

###############
# Litter Data #
###############
litter <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         ln==1,
         !(grid == "AG" & yr < 2004),
         !(grid == "LL" & yr >= 1997 & yr <= 2003),
         !(grid == "LL" & yr >= 2012),
         !(grid == "JO" & yr >= 2012),
         !(grid == "CH" & yr >= 2012),
         !(grid == "AG" & yr >= 2018)) %>% 
  select(litter_id = id,
         year = yr, field_bdate = fieldBDate, n1_date = date1, tag_date = tagDt,squirrel_id, grid, locx, locy, ln, food)

litter<-collect(litter)


dam_fit <- results %>% 
  group_by(litter_id) %>% 
  dplyr::summarize(litter_fit=sum(survived_200d), mean_growth=mean(growth, na.rm=T), ls = mean(litter_size, na.rm=T)) %>% 
  mutate(litter_id = as.integer(litter_id))

dam_fit2<-results %>% #Sum of all offspring from that year regardless of litter number
  group_by(mother_id, year) %>% 
  dplyr::summarize(all_litters_fit=sum(survived_200d))

litter<- litter %>% 
  mutate(grid=factor(grid, levels = c("AG", "CH", "JO", "KL", "LL", "SU"))) %>% 
  mutate(squirrel_id = as.integer(squirrel_id),
         litter_id = as.integer(litter_id)) %>% 
  mutate(field_bdate = ymd(field_bdate),
         n1_date = ymd(n1_date),
         tag_date = ymd(tag_date)) %>% 
  mutate(yrf=factor(year)) 

temp_cones2<-grids_cone_data %>% 
  select(year = Year, grid = Grid, by_mast=mast, by_cones_t = cones_t)

litter<-litter %>% 
  left_join(grids_cone_data, by =c("year" = "Year", "grid" = "Grid")) %>% 
  select(litter_id, food, grid, ln, squirrel_id, year, yrf, field_bdate, cones_tm1, Exp, EXP_label, cones_t, mast) %>% 
  left_join(flastall, by="squirrel_id") %>% 
  mutate(age=year-byear) %>% 
  left_join(temp_cones2, by =c("byear" = "year", "grid")) %>%    
  left_join(dam_fit, by="litter_id") %>%
  left_join(dam_fit2, by= c("squirrel_id" = "mother_id", "year"))  %>% 
  left_join(fla, by="squirrel_id") %>%
  left_join(density, by=c("year", "grid")) %>%  
  select(litter_id, food, grid, ln, squirrel_id, year, yrf, field_bdate, mean_growth, ls, cones_t, cones_tm1, mast, spr_density, by_mast, by_cones_t, Exp, EXP_label, byear, bcert, age, litter_fit, all_litters_fit, sire_id, dam_id, maternal_generation, generation)

rm(dam_fit, temp_cones2)  

litter<-litter %>% 
  mutate(Julian=yday(field_bdate),
         grid=factor(grid),
         yrf=factor(yrf)) %>% 
  collect()

litter<-litter %>% 
  left_join(part_dates, by="litter_id") %>% 
  mutate (Julian=part_date)

litter <- litter %>% 
  # left join will give NAs when squirrel/year isn't in food_add
  left_join(distinct(food_add), by = c("squirrel_id", "year")) %>% 
  #distinct(litter_id, .keep_all = TRUE) %>% 
  # fill NAs with "0"
  mutate(bucket = coalesce(bucket, "0")) %>% 
  mutate(bucket2 = if_else(year %in% c(2011, 2015)&grid %in% c("AG", "JO", "LL"), "1", bucket)) %>% 
  # 2011 and 2015 have bucket==0, and bucket2==1
  mutate(bucket = if_else(bucket=="Y", "1", bucket)) %>% 
  mutate (bucket = as.numeric(bucket)) %>% 
  mutate(bucket2 = if_else(bucket2=="Y", "1", bucket2)) %>% 
  mutate (bucket2 = as.numeric(bucket2))

# Standardize litter table within grid-years
litter<-litter %>% 
  group_by(yrf, grid) %>% 
  mutate(std_Growt = as.numeric(scale(mean_growth)),
         std_Julian = as.numeric(scale(Julian)),
         std_LS = as.numeric(scale(ls)),
         w_litter_fit = litter_fit/mean(litter_fit, na.rm=T)) %>% 
  ungroup()

#Check that this worked
# with(litter, tapply(std_Julian, list(yrf, grid), mean, na.rm=T))
# with(litter, tapply(std_Julian, list(yrf, grid), sd, na.rm=T))
# with(litter, tapply(w_litter_fit, list(yrf, grid), mean, na.rm=T))
# with(litter, tapply(w_litter_fit, list(yrf, grid), var, na.rm=T))


##########################
# Generate lifetime data #
##########################

lifetime<-tbl(con, "flastall2") %>% 
  filter(byear >= 1989,
         gr %in% c("AG", "CH", "JO", "KL", "LL", "SU"),
         bcert=="Y",
         !(f2 %in% c("4", "5", "11", "12", "22"))) %>% 
  select(squirrel_id, grid=gr, sex, dates, f1, byear=byear, litter_id, dam_id, sire_id, bcert=bcert, new_grid=newgr, new_sex=newsex, datee, f2, locX, locY) %>% 
  collect()

##  Note there are many behavioural records for squirrel_id=6230 that dont seem like they should be for her.  dateE should be 1999-08-06
## There seems to be a dead squirrel (squirrel_id = 7581) that showed up in the census when dead
## There are two litters for squirrel_id = 11486 (F5874/F7334; C1.) that are squished together as the same litter_id = 5032.  This is the correct id for some of the juves but the rest are from a second litter.  Need to get proper litter info for this litter and assign a new litter id.

################################################################
# Note that the above issue with 11486 still needs to be fixed #
################################################################


#fix known errors
lifetime<-lifetime %>% 
  mutate (datee = ifelse(squirrel_id==6230, "1999-08-06", datee)) %>% 
  mutate (datee = ifelse(squirrel_id==7581, "1998-06-26", datee))

#Link in juvenile data
results_temp<-results %>% 
  select(squirrel_id, field_bdate, bdate, age_last, birth_litter_size=litter_size, growth, birth_locx=locx, birth_locy=locy, dam_age, mother_byear)

lifetime<-lifetime %>% 
  left_join(results_temp, by="squirrel_id") %>% 
  #left_join(food_coding, by = c("grid", "byear"="yr")) %>% 
  left_join(grids_cone_data, by = c("grid"="Grid", "byear"="Year")) %>% 
  left_join(density, by = c("grid", "byear"="year")) %>% 
  left_join(fla, by="squirrel_id")

rm(results_temp)


lifetime<- lifetime%>% 
  left_join(temp_cones, by = c("mother_byear"="year", "grid")) 

rm(temp_cones)

lifetime<-lifetime %>% 
  mutate(dateE=ymd(datee),
         dateS=ymd(dates),
         field_bdate=ymd(field_bdate),
         longevity = as.integer(difftime(datee, field_bdate, units = "days")),
         grid=as.factor(grid),
         sex=as.factor(sex),
         bcert=as.factor(bcert))

## Calculate LBS
lifetime <- lifetime %>% 
  group_by(dam_id) %>% 
  summarize(dam_lbs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "dam_id"))

lifetime <- lifetime %>% 
  group_by(sire_id) %>% 
  summarize(sire_lbs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "sire_id"))

lifetime<-lifetime %>% 
  mutate(dam_lbs = ifelse(is.na(dam_lbs)&sex=="F", 0, dam_lbs),
         sire_lbs = ifelse(is.na(sire_lbs)&sex=="M", 0, sire_lbs),
         sire_lbs = ifelse(byear>2001, sire_lbs, NA_real_)) 


## Calculate LRS
lifetime <- lifetime %>% 
  filter(longevity > 199) %>% 
  group_by(dam_id) %>% 
  summarize(dam_lrs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "dam_id"))

lifetime <- lifetime %>% 
  filter(longevity > 199) %>% 
  group_by(sire_id) %>% 
  summarize(sire_lrs = n()) %>% 
  left_join(lifetime, ., by = c("squirrel_id"= "sire_id"))

lifetime<-lifetime %>% 
  mutate(dam_lrs = ifelse(is.na(dam_lrs)&sex=="F", 0, dam_lrs),
         sire_lrs = ifelse(is.na(sire_lrs)&sex=="M", 0, sire_lrs),
         sire_lrs = ifelse(byear>2001, sire_lrs, NA_real_))


lifetime<-lifetime %>% 
  mutate(LBS = coalesce(lifetime$dam_lbs, lifetime$sire_lbs),
         LRS = coalesce(lifetime$dam_lrs, lifetime$sire_lrs))

# LRS not defined for squirrels that did not survive to 200 days of age.
lifetime<-lifetime %>% 
  mutate(LRS = ifelse(longevity > 199, LRS, NA_real_))


lifetime<-lifetime %>% 
  group_by (byear, grid) %>% 
  mutate(std_bdate = as.numeric(scale(bdate)),
         std_growth = as.numeric(scale(growth)),
         std_birth_litter_size = as.numeric(scale(birth_litter_size)),
         w_dam_lrs = dam_lrs/mean(dam_lrs, na.rm=T),
         w_sire_lrs = sire_lrs/mean(sire_lrs, na.rm=T)) %>% 
  ungroup()


# library (plyr)
# lifetime<-ddply(lifetime, c("byear", "grid"), transform, std_bdate = scale(bdate))
# lifetime<-ddply(lifetime, c("byear", "grid"), transform, std_growth = scale(growth))
# lifetime<-ddply(lifetime, c("byear", "grid"), transform, std_birth_litter_size = scale(birth_litter_size))
# lifetime<-ddply(lifetime, c("byear", "grid"), transform, w_dam_lrs = dam_lrs/mean(dam_lrs, na.rm=T))
# lifetime<-ddply(lifetime, c("byear", "grid"), transform, w_sire_lrs = sire_lrs/mean(sire_lrs, na.rm=T))
# detach (package:plyr)


#Link some adult traits
first_litter_means<-litter %>% 
  filter (ln==1) %>% 
  group_by(squirrel_id) %>%
  dplyr::summarize(mean_std_julian = mean(std_Julian, na.rm=T),
                   mean_std_LS = mean(std_LS, na.rm=T), 
                   mean_std_growth = mean(std_Growt, na.rm=T))

lifetime<-lifetime %>% 
  left_join(first_litter_means, by = "squirrel_id")

rm(first_litter_means)

#Substitute zeros for missing adult traits (i.e. did not survive or was a male!)
lifetime<-lifetime %>% 
  mutate(mean_std_julian = ifelse(is.na(mean_std_julian), 0, mean_std_julian),
         mean_std_LS = ifelse(is.na(mean_std_LS), 0, mean_std_LS),
         mean_std_growth = ifelse(is.na(mean_std_growth), 0, mean_std_growth))


#Substitute zeros for missing juvenile traits (i.e. did not survive)
lifetime<-lifetime %>% 
  mutate(std_bdate = ifelse(is.na(std_bdate), 0, std_bdate),
         std_birth_litter_size = ifelse(is.na(std_birth_litter_size), 0, std_birth_litter_size),
         std_growth = ifelse(is.na(std_growth), 0, std_growth))


lifetime<-results %>%
  select(squirrel_id, ln) %>% 
  right_join (lifetime, by = "squirrel_id") %>% 
  mutate(ln=as.factor(ln))


#  Update litter table with some information from lifetime
litter<-lifetime %>%
  select(squirrel_id, std_bdate) %>% 
  right_join(litter, by="squirrel_id") %>% 
  mutate(std_mom_bdate = std_bdate) %>% 
  select (-std_bdate)

##  Any Missing Data in Lifetime?
good_litters<-c("0", "1", "2", "4", "6", "7")
missed_litters<-c("3", "5")

good_litter_records <- tbl(con, "litter") %>% 
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         br %in% good_litters) %>% 
  select(litter_id = id, squirrel_id) %>% 
  group_by(squirrel_id) %>% 
  dplyr::summarize(num_good_litters = n()) %>%
  collect()


bad_litter_records <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         br %in% missed_litters) %>% 
  select(litter_id = id, squirrel_id) %>% 
  group_by(squirrel_id) %>% 
  dplyr::summarize(num_missed_litters = n()) %>%
  collect()

manipulation_codes<-c("1", "4", "5", "9", "10", "13", "14", "15", "16", "17")

manipulated_litter_records <- tbl(con, "litter") %>% 
  # exclusions
  filter(yr >= 1989,
         grid %in% c("KL", "SU", "CH", "AG", "LL", "JO"), 
         food %in% manipulation_codes) %>% 
  select(litter_id = id, squirrel_id) %>% 
  group_by(squirrel_id) %>% 
  dplyr::summarize(num_manipulated_litters = n()) %>%
  collect()

afr <- tbl(con, "litter") %>% 
  collect()

afr<-afr %>% 
  left_join(flastall, by="squirrel_id") %>% 
  mutate(age=yr-byear) %>%
  filter (br != 0) %>% 
  select(squirrel_id, age) %>%
  group_by(squirrel_id) %>% 
  dplyr::summarize(afr = min(age),
                   alr = max(age))


lifetime<-lifetime %>% 
  left_join(good_litter_records, by="squirrel_id") %>% 
  left_join(bad_litter_records, by="squirrel_id") %>% 
  left_join(manipulated_litter_records, by="squirrel_id") %>% 
  left_join(afr, by="squirrel_id") %>% 
  left_join(food_add, by=c("dam_id" = "squirrel_id", "byear" = "year")) %>% 
  distinct(squirrel_id, .keep_all = TRUE) %>% 
  mutate(bucket = coalesce(bucket, "0")) %>% 
  mutate(bucket2 = if_else(byear %in% c(2011, 2015)&grid %in% c("AG", "JO", "LL"), "1", bucket)) %>% 
  # 2011 and 2015 have bucket==0, bucket2==1
  mutate(bucket = if_else(bucket=="Y", "1", bucket)) %>% 
  mutate (bucket = as.numeric(bucket)) %>% 
  mutate(bucket2 = if_else(bucket2=="Y", "1", bucket2)) %>% 
  mutate (bucket2 = as.numeric(bucket2)) %>% 
  mutate(num_missed_litters = ifelse(is.na(num_missed_litters), 0, num_missed_litters))

# Food add as adult.  Codes bucket.as.adult based on whether the squirrel owned a bucket as an adult.
food_add_temp<-food_add %>% 
  select(squirrel_id, bucket.as.adult=bucket)

lifetime<-lifetime %>% 
  left_join(food_add_temp, by = c("squirrel_id")) %>% 
  distinct(squirrel_id, .keep_all = TRUE) %>% 
  # fill NAs with "0"
  mutate(bucket.as.adult = coalesce(bucket.as.adult, "0")) %>% 
  mutate(bucket.as.adult = if_else(bucket.as.adult=="Y", "1", bucket.as.adult)) %>% 
  mutate (bucket.as.adult = as.numeric(bucket.as.adult))

rm(food_add_temp)  

rm(afr, bad_litter_records, good_litter_records, good_litters, manipulated_litter_records, manipulation_codes, missed_litters) 

# Lifetime Clean
lifetime_clean<-lifetime %>%
  filter (byear<2012, num_missed_litters<1, 
          is.na(alr)|is.na(afr)|num_good_litters - (alr-afr) >0)


# Add female survival following breeding
datee_temp <- tbl(con, "flastall2") %>% 
  #flastall2 contains juveniles that were not tagged
  # exclusions
  filter(gr %in% c("SU", "KL")) %>% 
  collect() %>% 
  mutate(datee = ymd(datee)) %>% 
  select(squirrel_id, datee)

litter<-litter %>%
  left_join(datee_temp, by="squirrel_id") %>%
  mutate(dam_surv = ifelse(datee-field_bdate>200, 1, 0))

####################
# Complete Cohorts #
####################
# So as of 2023, all cohorts were complete through the 2016 cohort.  
# There was still 1 squirrel alive from the 2017 cohort at the end of 2023.

############################
# Global Data Restrictions #
############################

# Exclude all data from experimental areas outside of the experiment (i.e. in control years prior to manipulation)
# Note that the end of the experiment is already filtered in the lifetime code

results<-results %>%
  filter (year>1988, 
          year<2025, 
          ln==1,
          !(grid == "AG" & year < 2005),
          !(grid == "LL" & year < 2006),
          !(grid == "JO" & year < 2007))

litter<-litter %>%
  filter (year>1988, 
          year<2025, 
          ln==1,
          !(grid == "AG" & year < 2005),
          !(grid == "LL" & year < 2006),
          !(grid == "JO" & year < 2007))

# Substitute zero for missing standardized growth
litter<-litter %>% 
  mutate (std_Growt = ifelse(is.na(std_Growt), 0, std_Growt))

# remove fitness for LL 2012 and CH 2011
results <- results %>% 
  mutate(survived_200d = ifelse(grid == "LL" & year == 2012, NA, survived_200d),
         survived_200d = ifelse(grid == "CH" & year == 2011, NA, survived_200d))

#################
# Pedigree Data #
#################

# Pedigree
library (RCurl)
script <- getURL("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/create_pedigree.R", ssl.verifypeer = FALSE)
eval(parse(text = script))

ped<-krsp_pedigree


#Generation in Experiment (or control)
gens<-read.csv("https://raw.githubusercontent.com/KluaneRedSquirrelProject/krsp-functions/master/generations3.csv", header=T, stringsAsFactors = FALSE) 


## Generate files for analysis
#########
#  Exp  #
#########

litter.EXP<-litter %>%
  filter(ln ==1, 
         year >= 1989,
         grid %in% c("AG", "LL", "JO"), 
         !(grid == "AG" & year < 2005),
         !(grid == "LL" & year < 2006),
         !(grid == "LL" & year > 2012),
         !(grid == "JO" & year > 2012),
         !(grid == "JO" & year < 2007),
         year > 2004, 
         year < 2018, 
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_tm1, cones_t, litter_fit) %>%
  left_join(gens, by="squirrel_id") %>% # NOTE that generation here is from generations3 and not generations2 from Lifetime
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year))) 

ped.EXP<-prunePed(ped, keep=litter.EXP$animal, make.base=FALSE)

litter.EXP=as.data.frame(litter.EXP[litter.EXP$animal %in% ped.EXP$id,])


litter.EXP.no.na.gen<-litter.EXP %>%
  filter(!is.na(generation))

###################
##  AG  ##
###################

litter.AG <- litter %>%
  filter(ln ==1, 
         grid == "AG", 
         year > 2004, 
         year < 2018,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.AG<-prunePed(ped, keep=litter.AG$animal, make.base=FALSE)

litter.AG=as.data.frame(litter.AG[litter.AG$animal %in% ped.AG$id,])

###################
##  SU  ##
###################

litter.SU <- litter %>%
  filter(ln ==1, 
         grid == "SU", 
         year > 2004, 
         year < 2018,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.SU<-prunePed(ped, keep=litter.SU$animal, make.base=FALSE)

litter.SU=as.data.frame(litter.SU[litter.SU$animal %in% ped.SU$id,])

###################
##  CH  ##
###################

litter.CH <- litter %>%
  filter(ln ==1, 
         grid == "CH", 
         year > 2004, 
         year < 2018,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.CH<-prunePed(ped, keep=litter.CH$animal, make.base=FALSE)

litter.CH=as.data.frame(litter.CH[litter.CH$animal %in% ped.CH$id,])

###################
##  LL  ##
###################

litter.LL <- litter %>%
  filter(ln ==1, 
         grid == "LL", 
         year > 2004, 
         year < 2018,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.LL<-prunePed(ped, keep=litter.LL$animal, make.base=FALSE)

litter.LL=as.data.frame(litter.LL[litter.LL$animal %in% ped.LL$id,])

###################
##  JO  ##
###################

litter.JO <- litter %>%
  filter(ln ==1, 
         grid == "JO", 
         year > 2004, 
         year < 2018,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.JO<-prunePed(ped, keep=litter.JO$animal, make.base=FALSE)

litter.JO=as.data.frame(litter.JO[litter.JO$animal %in% ped.JO$id,])

###################
##  KL  ##
###################

litter.KL <- litter %>%
  filter(ln ==1, 
         grid == "KL", 
         year > 2004, 
         year < 2018,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.KL<-prunePed(ped, keep=litter.KL$animal, make.base=FALSE)

litter.KL=as.data.frame(litter.KL[litter.KL$animal %in% ped.KL$id,])



###################
##  AG2  ##
###################

litter.AG2 <- litter %>%
  filter(ln ==1, 
         grid == "AG", 
         year > 2004, 
         year < 2018,
         year!=2011,
         year!=2015,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.AG2<-prunePed(ped, keep=litter.AG2$animal, make.base=FALSE)

litter.AG2=as.data.frame(litter.AG2[litter.AG2$animal %in% ped.AG2$id,])



##########################
#  All Controls
##########################

litter.all.control<-litter %>%
  filter(ln ==1, 
         year >= 1989,
         grid %in% c("KL", "SU", "CH"), 
         year < 2018, 
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_tm1, cones_t, litter_fit) %>%
  left_join(gens, by="squirrel_id") %>% # NOTE that generation here is from generations3 and not generations2 from Lifetime
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.all.control<-prunePed(ped, keep=litter.all.control$animal, make.base=FALSE)

litter.all.control=as.data.frame(litter.all.control[litter.all.control$animal %in% ped.all.control$id,])


##################
# Control KL, SU #
##################
litter.control <- litter %>%
  filter(ln ==1, 
         grid %in% c("SU", "KL"), 
         year > 1988, 
         year < 2026, 
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.control<-prunePed(ped, keep=litter.control$animal, make.base=FALSE)

litter.control=as.data.frame(litter.control[litter.control$animal %in% ped.control$id,])

################################
## Shortened control pedigree ##
################################
litter.control.short <- litter %>%
  filter(ln ==1, 
         grid %in% c("SU", "KL"), 
         year > 2004, 
         year < 2018, 
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, dam_id, cones_t, cones_tm1, litter_fit, std_Julian) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.control.short<-prunePed(ped, keep=litter.control.short$animal, make.base=FALSE)

litter.control.short=as.data.frame(litter.control.short[litter.control.short$animal %in% ped.control.short$id,])


##################################
## Shortened control pedigree 2 ##
##################################
litter.control.short2 <- litter %>%
  filter(ln ==1, 
         grid %in% c("SU", "KL"), 
         year > 2004, 
         year < 2018, 
         year!=2011,
         year!=2015,
         !is.na(age),
         !is.na(dam_id)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, dam_id, cones_t, cones_tm1, litter_fit, std_Julian) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.control.short2<-prunePed(ped, keep=litter.control.short2$animal, make.base=FALSE)

litter.control.short2=as.data.frame(litter.control.short2[litter.control.short2$animal %in% ped.control.short2$id,])


#######################################################################
# Build files for animal models including squirrels with missing dams #
#######################################################################
# This code is the same as above, but it no longer requires that the dam_id be known

##########
##  AG  ##
##########

litter.AG.v2 <- litter %>%
  filter(ln ==1, 
         grid == "AG", 
         year > 2004, 
         year < 2018,
         !is.na(age)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.AG.v2<-prunePed(ped, keep=litter.AG.v2$animal, make.base=FALSE)

litter.AG.v2=as.data.frame(litter.AG.v2[litter.AG.v2$animal %in% ped.AG.v2$id,])

###########
##  AG2  ##
###########

litter.AG2.v2 <- litter %>%
  filter(ln ==1, 
         grid == "AG", 
         year > 2004, 
         year < 2018,
         year!=2011,
         year!=2015,
         !is.na(age)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, bucket, bucket2, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year))


ped.AG2.v2<-prunePed(ped, keep=litter.AG2.v2$animal, make.base=FALSE)

litter.AG2.v2=as.data.frame(litter.AG2.v2[litter.AG2.v2$animal %in% ped.AG2.v2$id,])


##################
# Control KL, SU #
##################
litter.control.v2 <- litter %>%
  filter(ln ==1, 
         grid %in% c("SU", "KL"), 
         year > 1988, 
         year < 2026, 
         !is.na(age)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, dam_id, w=w_litter_fit, std_Julian, cones_t, cones_tm1, litter_fit) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.control.v2<-prunePed(ped, keep=litter.control.v2$animal, make.base=FALSE)

litter.control.v2=as.data.frame(litter.control.v2[litter.control.v2$animal %in% ped.control.v2$id,])

################################
## Shortened control pedigree ##
################################
litter.control.short.v2 <- litter %>%
  filter(ln ==1, 
         grid %in% c("SU", "KL"), 
         year > 2004, 
         year < 2018, 
         !is.na(age)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, dam_id, cones_t, cones_tm1, litter_fit, std_Julian) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.control.short.v2<-prunePed(ped, keep=litter.control.short.v2$animal, make.base=FALSE)

litter.control.short.v2=as.data.frame(litter.control.short.v2[litter.control.short.v2$animal %in% ped.control.short.v2$id,])


##################################
## Shortened control pedigree 2 ##
##################################
litter.control.short2.v2 <- litter %>%
  filter(ln ==1, 
         grid %in% c("SU", "KL"), 
         year > 2004, 
         year < 2018, 
         year!=2011,
         year!=2015,
         !is.na(age)) %>%
  select(squirrel_id, year, Julian, grid, age, generation, dam_id, cones_t, cones_tm1, litter_fit, std_Julian) %>%
  mutate(squirrel_id=factor(squirrel_id),
         animal=factor(squirrel_id),
         year.f=factor(year),
         grid_year=factor(paste(grid, year)))

ped.control.short2.v2<-prunePed(ped, keep=litter.control.short2.v2$animal, make.base=FALSE)

litter.control.short2.v2=as.data.frame(litter.control.short2.v2[litter.control.short2.v2$animal %in% ped.control.short2.v2$id,])


#############
# Save Data #
#############

save.image("Exp_Data_Sept_2026.RData")

