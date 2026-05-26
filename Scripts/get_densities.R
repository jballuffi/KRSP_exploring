
library(ggplot2)
library(data.table)
library(lubridate)
library(ggforce)
library(dplyr)


#read in prepped census data
census <- readRDS("Output/Data/census_full.rds")

#set distance for local densities
radius <- 130



# Clean data -----------------------------------------------------------------

#subset census data
#remove any unknown squirrels and only take from month of may
data <- census[!is.na(squirrel_id) & month == 5]

#remove date col
data[, date := NULL]

#make a squirrel_year col
data[, squirrel_yr := paste0(squirrel_id, "_", year)]

#make a squirrel_year_grid col
data[, squirrel_yr_gr := paste0(squirrel_id, "_", year, "_", grid)]

#remove any cases of duplicate squirrel year/grid
data <- data[!duplicated(squirrel_yr_gr)]

#convert spatial locations into meters
data[, locX := locX*30][, locY := locY*30]



# test on one individual on one grid in one year -----------------------------------------------------------

#step 1 get the squirrel's location
#step 2 get the number of locations for that squirrel
#step 3 take all locations that are within the radius
#step 4 tally that number of locations
#step 5 subtract the locations that are the same squirrel
#step 6 calculate density

#subset the dataset to test on
test <- data[grid == "SU" & year == 2023]

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
  labs(x = "X coordinate (m)", y = "Y coordinate (m)")+
  coord_fixed()+
  theme_minimal()

#get the x and y locations for that test individual
testx <- test[squirrel_id == testid, locX]
testy <- test[squirrel_id == testid, locY]

#count the number of other individuals within the radius (minus 1 to remove the test ind's location)
count <- test[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2, .N] - 1
count #squirrels

#calculate the density based on that count
density <- count/((pi*radius^2)/10000)
density #squirrels per hectare



# create function for multi grid and multi years ----------------------------

#build function for full dataset
getneighbors <- function(id){
  #get the x and y locations for that test individual
  testx <- test3[squirrel_yr_gr == id, locX]
  testy <- test3[squirrel_yr_gr == id, locY]
  testyear <- test3[squirrel_yr_gr == id, year]
  testgrid <- test3[squirrel_yr_gr == id, grid]
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- test3[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2 & year %in% testyear & grid %in% testgrid, .N] - 1
  return(count) #squirrels per hectare
}

#duplicate test
test3 <- data[grid == "SU" | grid == "KL" & year > 2020]

testid2 <- sample(test3$squirrel_yr_gr)

getneighbors(id = testid2)

test3$count <- lapply(test3$squirrel_yr_gr, getneighbors)
test3$count <- unlist(test3$density)



# run function on all data  ------------------------------------------------

#run function on the entire list of squirrel yr grids
data$neighbors <- lapply(data$squirrel_yr_gr, getneighbors)
data$neighbors <- unlist(data$neighbors)

#fix neg 1's (these were cases where there we no neighbors)
data[neighbors < 0, neighbors := 0]

#convert to densities (squirrels/ha)
data[, density := neighbors/((pi*radius^2)/10000)]



# subset and save  --------------------------------------------------------

#subset to cols of interest
densities <- data[, .(squirrel_id, grid, year, reflo, neighbors, density)]

#save
saveRDS(densities, "Output/Data/local_densities.rds")




