
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#add in this
library(tidyverse)


# Import Data ---------------------------------------

#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

#Importing midden census data
census_1 <- tbl(con, "dbamidden") %>% 
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
  filter(sq_fate!= 7) %>%
  # use collect to execute the sql query before using any R specific functions
  # such as loc_to_numeric
  collect %>%
  mutate(locx = loc_to_numeric(locx))

#import extra data for rattle experiments in 2010
RRcensus <- fread("Input/2010_RR_Midden_Census.csv")
BTcensus <- fread("Input/2010_BT_Midden_Census.csv")
AGcensus <- fread("Input/2010_AG_AO_Midden_Census.csv")



# prep and merge database data -----------------------------------------------------

#change col names to match
census_2 <- select (census_2, -sq_fate) %>% 
  rename (locX = locx,
          locY = locy,
          grid = gr,
          date = census_date,
          Sex = sex)

#bind both census data together
census <- bind_rows(census_1, census_2) %>% 
  mutate(grid = factor(grid),
         date = ymd(date),
         year = year(ymd(date)),
         month = month(ymd(date)),
         locY = as.numeric(locY),
         Sex = factor(Sex))

#make a data.table
census <- data.table::as.data.table(census)

#remove any lines where there is no location
census <- census[!is.na(locX) & !is.na(reflo)]



# prep and merge 2010 data ------------------------------------------------

#fix date formats
AGcensus[, Date := dmy(Date)]
BTcensus[, Date := mdy(Date)]
RRcensus[, Date := mdy(Date)]

#rbind all the 2010 data sets
extra <- rbindlist(list(AGcensus, BTcensus, RRcensus), fill = TRUE)

#take only grids from the list of grids
extra <- extra[Grid %in% grids]

census2010 <- extra[, .(grid = factor(Grid),
                        reflo = MidReflo, 
                        squirrel_id = paste0(Taglft, "-", Tagrt),
                        date = Date,
                        locx = as.numeric(LocX),
                        locy = as.numeric(LocY),
                        date = Date,
                        Sex,
                        year = year(Date),
                        month = month(Date))]



# Save --------------------------------------------------------------------

#save as RDS
saveRDS(census, "Output/Data/census_full.rds")


