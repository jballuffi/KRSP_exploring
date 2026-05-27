
library(ggplot2)
library(data.table)
library(lubridate)
library(ggforce)
library(dplyr)


#read in prepped census data
census <- readRDS("Output/Data/census_full.rds")

#read in PB bucket data
buckets <- fread("Input/fed_squirrels.csv")

#set distance for local densities
radius <- 130



# Clean data -----------------------------------------------------------------

#subset census data
#remove any unknown squirrels and only take from month of may
data <- census[!is.na(squirrel_id) & month == 5] #correct to remove NAs

#remove date col
data[, date := NULL]

#make a squirrel_year col
data[, squirrel_yr := paste0(squirrel_id, "_", year)]

#make a squirrel_year_grid col
data[, squirrel_yr_gr := paste0(squirrel_id, "_", year, "_", grid)]

#remove any cases of duplicate squirrel year/grid
#most likely from someone censused twice
data <- data[!duplicated(squirrel_yr_gr)]

#convert spatial locations into meters
data[, locX := locX*30][, locY := locY*30]

#remove one data point with ?? grid
data <- data[!grid == "??"]



# merge with bucket info --------------------------------------------------

#remove missing squirrel IDs
buckets <- buckets[!is.na(squirrel_id_spring)]

#create a col called bucket and remove 
buckets[, bucket := 1][, bucket_reflo := NULL]

#rename id col
setnames(buckets, "squirrel_id_spring", "squirrel_id")

#merge with census data
data2 <- merge(data, buckets, by = c("grid", "year", "squirrel_id"), all.x = TRUE)

#any bucket NAs become zero
data2[is.na(bucket), bucket := 0]

#make bucket factor
data2[, bucket := as.factor(bucket)]



# test on one individual on bucket# test on one individual on one grid in one year -----------------------------------------------------------

#step 1 get the squirrel's location
#step 2 get the number of locations for that squirrel
#step 3 take all locations that are within the radius
#step 4 tally that number of locations
#step 5 subtract the locations that are the same squirrel
#step 6 calculate density

#subset the data set to test on
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



# create function to get the number of neighbors ----------------------------

#build function for full data set
getneighbors <- function(id){
  #get the x and y locations for that test individual
  testx <- data2[squirrel_yr_gr == id, locX]
  testy <- data2[squirrel_yr_gr == id, locY]
  testyear <- data2[squirrel_yr_gr == id, year]
  testgrid <- data2[squirrel_yr_gr == id, grid]
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- data2[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2 & year %in% testyear & grid %in% testgrid, .N] - 1
  return(count) #squirrels per hectare
}



# run function on all data  ------------------------------------------------

#run function on the entire list of squirrel yr grids
data2$neighbors <- lapply(data2$squirrel_yr_gr, getneighbors)
data2$neighbors <- unlist(data2$neighbors)

#fix neg 1's (these were cases where there we no neighbors)
data2[neighbors < 0, neighbors := 0]

#convert to densities (squirrels/ha)
data2[, density := neighbors/((pi*radius^2)/10000)]

#subset to cols of interest
densities <- data2[, .(mother_id = squirrel_id, grid, year, reflo, bucket, neighbors, density)]



# plot --------------------------------------------------------------------

means <- densities[, .(density = mean(density)), year]
setorder(means, year)

ggplot(means)+
  geom_path(aes(x = year, y = density, group = 1))

ggplot(densities)+
  geom_boxplot(aes(x = grid, y = density))



# save  --------------------------------------------------------

#save prepped data
saveRDS(densities, "Output/Data/local_densities.rds")




