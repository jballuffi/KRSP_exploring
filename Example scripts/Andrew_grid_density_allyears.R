#Measures spring density for each grid in each year

# Creates two tables.  One for spring densities for each grid in each year (grids_density) and one table for yearly spring densities for KL and SU combined (SUKL_yearly_density)

library(tidyverse)
library(krsp)
library (lubridate)

connection_1 <- krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                              dbname ="krsp",
                              username = Sys.getenv("krsp_user"),
                              password = Sys.getenv("krsp_password")
)



#Importing midden census data
census_1 <- tbl(connection_1, "dbamidden") %>% 
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

selected_grids <- c("AG", "CH", "JO", "KL", "LL", "SU")
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
  summarise(spr_number = n_distinct (squirrel_id)) %>% 
  select (year, grid, spr_number)

# Summary of Spring Density
grid<-c("KL", "SU", "CH", "JO", "LL", "AG")
area<-c(39.69, 39.69, 39.69, 39.69, 26.73, 45.36)
area<-data.frame(grid, area)

grids_density <- left_join(grids_density, area, by = "grid") %>% 
  mutate(spr_density = spr_number/area)


# Annual density measures for KL and SU only

SUKL_yearly_density<-core_may_census_all %>% 
  filter(!is.na(squirrel_id), grid %in% c("KL", "SU")) %>%
  group_by(year) %>% 
  summarise(spr_number = n_distinct (squirrel_id)) %>% 
  arrange(year) %>% 
  mutate(spr_density = spr_number/(39.69+39.69))


rm(ag_core_may_census, area, census_1, census_2, census_all, connection_1, core_may_census_all, ll2_core_may_census, suchjokl_core_may_census, grid, selected_grids, selected_grids2)
