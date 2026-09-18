
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped census data
data <- readRDS("Output/Data/census_full.rds")



# prep data -----------------------------------------------------------------

#make a squirrel_year col
data[, squirrel_yr := paste0(squirrel_id, "_", year)]

#make a squirrel_year_grid col
data[, squirrel_yr_gr := paste0(squirrel_id, "_", year, "_", grid)]

#convert spatial locations into meters
data[, locX := locX*30][, locY := locY*30]



# create function to get the number of neighbors ----------------------------

#build function for full data set
getneighbors <- function(id){
  testx <- data[squirrel_yr_gr == id, locX] #get the x location of that ind-year
  testy <- data[squirrel_yr_gr == id, locY] #get the y location of that ind-year
  testyear <- data[squirrel_yr_gr == id, year] #get the year of that ind-year
  testgrid <- data[squirrel_yr_gr == id, grid] #get the grid of that ind-year
  
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- data[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2 & year %in% testyear & grid %in% testgrid, .N] - 1
  
  return(count) #squirrels per hectare
}



# run function on all data  ------------------------------------------------

#run function on the entire list of squirrel yr grids
data$neighbors <- lapply(data$squirrel_yr_gr, getneighbors)
data$neighbors <- unlist(data$neighbors)

#convert to densities (squirrels/ha)
data[, density := neighbors/((pi*radius^2)/10000)]

#subset to cols of interest and grids of interest
densities <- data[grid %in% grids, 
                  .(mother_id = squirrel_id,
                    grid, 
                    year, 
                    locx = locX, 
                    locy = locY, 
                    neighbors, 
                    density,
                    loc_source)]



# save  -------------------------------------------------------- 

#save prepped data
saveRDS(densities, "Output/Data/local_densities.rds")

