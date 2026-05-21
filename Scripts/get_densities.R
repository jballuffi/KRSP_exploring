
library(ggplot2)
library(data.table)
library(lubridate)
library(ggforce)
library(dplyr)


#read in prepped census data
census <- readRDS("Output/Data/census_full.rds")

#set distance for local densities
radius <- 130



# Erin's code -------------------------------------------------------------

#this initial subsetting removes any duplicated IDs across all years. This is a mistake
data <- census %>% 
  filter(month == 5,
         !is.na(squirrel_id),
         !duplicated(squirrel_id)) %>%
  mutate(local.density = "") #this creates a new column in which to store your calculated local density

n <- length(data$squirrel_id) #This is the length of your data (i.e. the squirrels that you want to create neighbourhoods and calculate density for). You can substitute squirrel_id for any column. This is the data the code below will loop through.

for (i in 1:n) {
  print(i)
  neighbours <- subset(census, census$grid==data$grid[i] & census$year==data$year[i] & census$month == data$month[i] & (30*data$locX[i]-30*census$locX)^2+(30*data$locY[i]-30*census$locY)^2<=(distance)^2) #This selects neighbours in the same grid, year, month and within 130 m of your focal individual.
  neighbours$Focal_ID <- data$squirrel_id[i] #this creates a column in your new 'neighbours' dataframe so that you can identify which focal individual you just created a neighbourhood for. 
  neighbours <- subset(neighbours, !neighbours$Focal_ID==neighbours$squirrel_id) #this makes sure your focal squirrel is not included in your neighbours.
  
  #Calculate Density (squirrels per hectare: 53,092 m2 = 5.3 hectares)
  num.indiv <-  length(unique(neighbours$squirrel_id))
  density <- num.indiv/((pi*distance^2)/10000)
  
  #Put it all together
  data[i,"local.density"] <- density
  
}



# Clean data -----------------------------------------------------------------

#subset census data
data2 <- census[!is.na(squirrel_id) & month == 5]

#remove date col
data2[, date := NULL]

#make a squirrel_year col
data2[, squirrel_yr := paste0(squirrel_id, "_", year)]

#make a squirrel_year_grid col
data2[, squirrel_yr_gr := paste0(squirrel_id, "_", year, "_", grid)]

#remove any cases of duplicate squirrel years
data2 <- data2[!duplicated(squirrel_yr)]

#convert spatial locations into meters
data2[, locX := locX*30][, locY := locY*30]



# test on one individual on one grid in one year -----------------------------------------------------------

#step 1 get the squirrel's location
#step 2 get the number of locations for that squirrel
#step 3 take all locations that are within the radius
#step 4 tally that number of locations
#step 5 subtract the locations that are the same squirrel
#step 6 calculate density


#duplicate test
test <- data2[grid == "SU" & year == 2023]

#randomly pick an individual
testid <- sample(test$squirrel_id, 1)

#make a col to highlight that individual
test[squirrel_id == testid, testsubject := "yes"]
test[is.na(testsubject), testsubject := "no"]

#make colors for test subject
testcols <- c("yes" = "red", "no" = "grey")

#plot the location of all squirrels for that test data with the test individual highlighted
ggplot(test)+
  geom_circle(aes(x0 = locX, y0 = locY, r = radius, color = testsubject))+
  geom_point(aes(x = locX, y = locY, color = testsubject), size = 3)+
  scale_color_manual(guide = NULL, values = testcols)+
  coord_fixed()+
  theme_minimal()

#get the x and y locations for that test individual
testx <- test[squirrel_id == testid, locX]
testy <- test[squirrel_id == testid, locY]

#count the number of other individuals within the radius (minus 1 to remove the test ind's location)
test[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2]
count <- test[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2, .N] - 1
count #squirrels

#calculate the density based on that count
density <- count/((pi*radius^2)/10000)
density #squirrels per hectare

# ^^^ THIS WORKS FOR INDIVIDUALS




# create function for one grid and one year ---------------------------------------------------------

#test function
fun1 <- function(id){
  #get the x and y locations for that test individual
  testx <- test[squirrel_id == id, locX]
  testy <- test[squirrel_id == id, locY]
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- test[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2, .N] - 1
  density <- count/((pi*radius^2)/10000)
  return(density) #squirrels per hectare
  }

fun1(id = testid)

#test function on the entire list of squirrels
test$density <- lapply(test$squirrel_id, fun1)



# create function for one grid but multi years ----------------------------

#test function
fun2 <- function(id){
  #get the x and y locations for that test individual
  testx <- test2[squirrel_yr == id, locX]
  testy <- test2[squirrel_yr == id, locY]
  testyear <- test2[squirrel_yr == id, year]
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- test2[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2 & year %in% testyear, .N] - 1
  density <- count/((pi*radius^2)/10000)
  return(density) #squirrels per hectare
}

#duplicate test
test2 <- data2[grid == "SU" & year > 2020]

testidyr <- paste0(testid, "_", 2023)

fun2(id = testidyr)

test2$density <- lapply(test2$squirrel_yr, fun2)

test2[squirrel_yr == testidyr]




# create function for multi grid and multi years ----------------------------

#test function
fun3 <- function(id){
  #get the x and y locations for that test individual
  testx <- test3[squirrel_yr_gr == id, locX]
  testy <- test3[squirrel_yr_gr == id, locY]
  testyear <- test3[squirrel_yr_gr == id, year]
  testgrid <- test3[squirrel_yr_gr == id, grid]
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- test3[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2 & year %in% testyear & grid %in% testgrid, .N] - 1
  density <- count/((pi*radius^2)/10000)
  return(count) #squirrels per hectare
}

#duplicate test
test3 <- data2[grid == "SU" | grid == "KL" & year > 2020]

testid <- sample(test3$squirrel_yr_gr)

fun3(id = testid)

test3$density <- lapply(test3$squirrel_yr_gr, fun3)
min(test3$density)

#a few with neg densities but likely because of the minus 1
