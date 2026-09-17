
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
  #get the x and y locations for that test individual
  testx <- data[squirrel_yr_gr == id, locX]
  testy <- data[squirrel_yr_gr == id, locY]
  testyear <- data[squirrel_yr_gr == id, year]
  testgrid <- data[squirrel_yr_gr == id, grid]
  #count the number of other individuals within the radius (minus 1 to remove the test ind's location)
  count <- data[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2 & year %in% testyear & grid %in% testgrid, .N] - 1
  return(count) #squirrels per hectare
}



# run function on all data  ------------------------------------------------

#run function on the entire list of squirrel yr grids
data$neighbors <- lapply(data$squirrel_yr_gr, getneighbors)
data$neighbors <- unlist(data$neighbors)

#fix neg 1's (these were cases where there we no neighbors)
data[neighbors < 0, neighbors := 0]

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

