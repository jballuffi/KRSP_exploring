
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
  select(reflo, squirrel_id, locX, locY, grid, date, Sex, fate) %>%
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

#import trapping data for any missing midden locations
trapping <- as.data.table(tbl(con, "trapping") %>% collect())

#import extra data from rattle experiments in 2010 that arent in data base.
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

#reclassify SX or SUX to just be SU
census[grid == "SX" | grid == "SUX", grid := "SU"]

#clean out unwanted grids and any rows without squirrel ID
census <- census[grid %in% grids & !is.na(squirrel_id)]

#start a file with all squirrels censused every year and grid
c1 <- census[, .(squirrel_id = unique(squirrel_id)), by = .(grid, year)]

#take only spring census (April and May) and average locations
censusspring <- census[month == 5 | month == 4, 
                       .(locX = round(mean(locX, na.rm = TRUE), 1), 
                        locY = round(mean(locY, na.rm = TRUE), 1)),
                       by = .(squirrel_id, year, grid)]

#prep trapping data
trap <- trapping[, .(grid = factor(gr),
                     year = year(ymd(date)),
                     month = month(ymd(date)),
                     squirrel_id,
                     locX = loc_to_numeric(locx),
                     locY = as.numeric(locy)
                     )]

#take only April - June for trapping
trapspring <- trap[month == 4 | month == 5 | month == 6]

#average spring trapping locations for each squirrel each grid-year
traplocs <- trapspring[, .(trapX = round(mean(locX, na.rm = TRUE), 1), 
                     trapY = round(mean(locY, na.rm = TRUE), 1)),
                by = .(squirrel_id, year, grid)]


#merge starting list with census data
#any NA locations are squirrels that were censused that year but not in april or may
c1 <- merge(c1, censusspring, by = c("squirrel_id", "grid", "year"), all.x = TRUE)

#merge census list with trap locations
c1 <- merge(c1, traplocs, by = c("squirrel_id", "year", "grid"), all.x = TRUE)

#if there is a missing census location, will in with average trap location
c1[is.na(locX), locX := trapX][is.na(locY), locY := trapY]

#remove trap cols
c1[, trapX := NULL][, trapY := NULL]



# prep 2010 data ------------------------------------------------

#fix date formats because they are all DIFFERENT
AGcensus[, Date := dmy(Date)]
BTcensus[, Date := mdy(Date)]
RRcensus[, Date := mdy(Date)]

#take only the back of AG
AGcensus <- AGcensus[LocY > 24]

#rbind all the 2010 data sets
extra <- rbindlist(list(AGcensus, BTcensus, RRcensus), fill = TRUE)

#make squirrel ID from ear tag
extra[, squirrel_id := paste0(Taglft, "-", Tagrt)]

#take only cases where ID is known
extra <- extra[!squirrel_id == "-"]

#take only cols that are in  census
extra <- extra[, .(grid = factor(Grid),
                        squirrel_id,
                        year = year(Date),
                        locX = as.numeric(LocX),
                        locY = as.numeric(LocY))]

#average census locations 
census2010 <- extra[, .(locX = round(mean(locX, na.rm = TRUE), 1), 
                        locY = round(mean(locY, na.rm = TRUE), 1)),
                       by = .(squirrel_id, year, grid)]



# merge database and 2010 rattle data  ------------------------------------

#rbind 2010 data with census
fullcensus <- rbind(census2010, c1)

#lastly, remove anyone who still does not have a location for the spring of that year
fullcensus <- fullcensus[!is.na(locX) | !is.na(locY)]



# Save --------------------------------------------------------------------

#save as RDS
saveRDS(fullcensus, "Output/Data/census_full.rds")


