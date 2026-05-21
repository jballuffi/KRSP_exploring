
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

#remove any cases of duplicate squirrel years
data2 <- data2[!duplicated(squirrel_yr)]

#convert spatial locations into meters
data2[, locX := locX*30][, locY := locY*30]




# build function for density -----------------------------------------------------------

#test with a subset of data, Sulphur in 2002
test <- data2[grid == "SU" & year == 2002]
testfun <- test #the test for the function building

#pick an individual
testid <- "5475"

#make a col to highlight that individual
test[squirrel_id == testid, idtest := "yes"]

#plot the location of all squirrels for that test data with the test individual highlighted
ggplot(test)+
  geom_circle(aes(x0 = locX, y0 = locY, r = radius, color = idtest), linewidth = .75)+
  geom_point(aes(x = locX, y = locY, color = idtest), size = 3)+
  coord_fixed()+
  theme_minimal()

#get the x and y locations for that test individual
testx <- test[squirrel_id == testid, locX]
testy <- test[squirrel_id == testid, locY]

#count the number of other individuals within the radius (minus 1 to remove the test ind's location)
count <- test[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2, .N - 1]
count

#calculate the density based on that count
density <- count/((pi*radius^2)/10000)
density

# ^^^ THIS WORKS FOR INDIVIDUALS






get_density <- function(dat, id, xloc, yloc){
  
  #first get all individuals
  ids <- list(dat$id)
  
  #get all X locs of those individuals
  xcenters <- 
  
  xcenter = dat[squirrel_id == id, locX]
  ycenter = dat[squirrel_id == id, locY]
  N = dat[(xcenter - locX)^2 + (ycenter - locY)^2 <= (distance)^2, .N]
  N = N - 1
  density <- N/((pi*distance^2)/10000)
  return(density)
}


ids <- list(test$squirrel_id)

xcenters <- lapply(ids, function(z){test[squirrel_id == z, return(locX)]})

ycenters <- lapply(ids, function(z){test[squirrel_id == z, return(locY)]})




get_density(id = "5540")

test$density <- lapply(ids, get_density)

test[, density := get_density(id = squirrel_id), squirrel_id]






#create coordinates of a test location
testX = 250
testY = 250

#from Erin's code:
#(30*data$locX[i] - 30*census$locX)^2 + (30*data$locY[i] - 30*census$locY)^2 <= (distance)^2)

test[(testX - locX)^2 + (testY - locY)^2 <= (distance)^2, .N]

#how many individuals?
test[, length(unique(squirrel_id))]

#get avg locations of individuals




inds <- test[!is.na(squirrel_id), .(N = .N, 
                                    meanX = mean(locX, na.rm = TRUE),
                                    meanY = mean(locY, na.rm = TRUE)),
             by = squirrel_id]




getdensity <- function(x, y){
  
  test[(x - locX)^2 + (x - locY)^2 <= (distance)^2, .N]
  
  }


inds[, count := getdensity(x = meanX, y = meanY), squirrel_id]
inds[count == 0, type := "none"]


#plot the location of all squirrels for that test data
ggplot(inds)+
  geom_circle(aes(x0 = meanX, y0 = meanY, r = distance), color = "blue")+
  geom_point(aes(x = meanX, y = meanY, size = count, color = type))+
  coord_fixed()+
  theme_minimal()




#step 1 get the squirrel's avg location
#step 2 get the number of locations for that squirrel
#step 3 take all locations that are within the radius
#step 4 tally that number of locations
#step 5 subtract the locations that are the same squirrel
#step 6 calculate density











getdensity <- function(x, y){
  
  #tally up how many locations in the test folder are within the radius
  test2[(x - locXm)^2 + (y - locYm)^2 <= (distance)^2, .N]
  
}

test2[, .(density = getdensity(x = locXm, y = locYm))]


test2[, x := locXm][, y := locYm]

test2[(x - locXm)^2 + (y - locYm)^2 <= (distance)^2, .N, squirrel_id]






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

