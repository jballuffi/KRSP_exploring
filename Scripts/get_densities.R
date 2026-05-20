
library(krsp)
library(ggplot2)
library(data.table)
library(lubridate)
library(ggforce)

#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

#set distance for local densities
distance <- 130



# Import and bind all census data (Andrew's code) -----------------------------------------

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
         year = year(ymd(date)),
         month = month(ymd(date)),
         locY = as.numeric(locY),
         Sex = factor(Sex))




# my inquiries into the data ----------------------------------------------

#make a data.table
census <- as.data.table(census)

#take only data when there is a squirrel ID and from May census
census <- census[!is.na(squirrel_id) & month == 5]

#not sure what to do about replicate squirrel IDs

#sample size of squirrels by year
census[, length(unique(squirrel_id)), by = .(year)]
census[, .N, by = year]

#convert spatial locations into meters
census[, locXm := locX*30][, locYm := locY*30]

#test with a subset of data
test <- census[grid == "SU" & year == 2002]

#plot the location of all squirrels for that test data
ggplot(test)+
  geom_circle(aes(x0 = locXm, y0 = locYm, r = distance), color = "blue")+
  geom_point(aes(x = locXm, y = locYm))+
  coord_fixed()+
  theme_minimal()

#take average location for each squirrel ID
test2 <- test[, .(locXm = mean(locXm), locYm = mean(locYm)), squirrel_id]

ggplot(test2)+
  geom_circle(aes(x0 = locXm, y0 = locYm, r = distance), color = "blue")+
  geom_point(aes(x = locXm, y = locYm))+
  coord_fixed()+
  theme_minimal()

#create coordinates of a test location
testX = 250
testY = 250

#from Erin's code:
#(30*data$locX[i] - 30*census$locX)^2 + (30*data$locY[i] - 30*census$locY)^2 <= (distance)^2)

test2[(testX - locXm)^2 + (testY - locYm)^2 <= (distance)^2, .N]


getdensity <- function(x, y){
  
  #tally up how many locations in the test folder are within the radius
  test2[(x - locXm)^2 + (y - locYm)^2 <= (distance)^2, .N]
  
}

test2[, .(density = getdensity(x = locXm, y = locYm))]


test2[, x := locXm][, y := locYm]

test2[(x - locXm)^2 + (y - locYm)^2 <= (distance)^2, .N, squirrel_id]




# Erin's code to get local densities --------------------------------------


#create an empty data frame to store the iterations of your loop in
neighbours.all <- data.frame()

#subset the census data
data <- census %>% 
  filter(year == "2002",
         !is.na(squirrel_id),
         !duplicated(squirrel_id)) %>%
  mutate(local.density = "") #this creates a new column in which to store your calculated local density

n <- length(data$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through.

for (i in 1:n) {
  print(i)
  neighbours <- subset(census, census$grid == data$grid[i] 
                       & census$year == data$year[i] 
                       & census$month == data$month[i] 
                       & (30*data$locX[i] - 30*census$locX)^2 + (30*data$locY[i] - 30*census$locY)^2 <= (distance)^2) #This selects neighbours in the same grid, year, month and within 130 m of your focal individual.
  neighbours$Focal_ID <- data$squirrel_id[i] #this creates a column in your new 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for. 
  neighbours <- subset(neighbours, !neighbours$Focal_ID == neighbours$squirrel_id) #this makes sure your focal squirrel is not included in your neighbours.
  
  #Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
  num.indiv <-  length(unique(neighbours$squirrel_id))
  density <- num.indiv/((pi*distance^2)/10000)
  
  #Put it all together
  data[i,"local.density"] <- density
  
}




















# get all squirrel data ---------------------------------------------------------

#import data
squirrel <- as.data.table(tbl(con, "squirrel") %>% collect())

#take only cols of interest
squirrel <- squirrel[, .(squirrel_id = id, tdate = ymd(trap_date), grid = gr)]



# calculate grid densities  ----------------------------------------

#get year and month
squirrel[, yr := year(tdate)]

#get total number of individual per year and grid
density <- squirrel[, .(density = length(unique(squirrel_id))), by = .(yr, grid)]

#remove the missing grid
density <- density[!is.na(grid)]



# save densities ----------------------------------------------------------

saveRDS(density, "Output/Data/grid_densities.rds")

