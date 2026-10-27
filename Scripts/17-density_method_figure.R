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



# Make figure for methods section -----------------------------------------------------------

#step 1 get the squirrel's location
#step 2 tally the number of neighbors within the radius of that location
#step 3 subtract the squirrel from the tally of neighbors
#step 6 calculate density

#subset the data set to test on
test <- data[grid == "SU" & year == 2015]

#ID that was selected at random, but pasted to ensure figure is always the same
testid <- 13132

#make a col to highlight that individual
test[squirrel_id == testid, testsubject := "yes"]
test[is.na(testsubject), testsubject := "no"]

#make colors for test subject
testcols <- c("yes" = "black", "no" = NA)
testshapes <- c("yes" = 16, "no" = 1)

#plot the location of all squirrels for that test data with the test individual highlighted
(testfig <- ggplot(test)+
    geom_circle(aes(x0 = locX, y0 = locY, r = radius, color = testsubject))+
    geom_point(aes(x = locX, y = locY, shape = testsubject), size = 3)+
    scale_shape_manual(values = testshapes, guide = NULL)+
    scale_color_manual(values = testcols, guide = NULL)+
    labs(x = "X coordinate (m)", y = "Y coordinate (m)")+
    xlim(-200, 700)+
    ylim(-50, 650)+
    coord_fixed()+
    theme1)

#get the x and y locations for that test individual
testx <- test[squirrel_id == testid, locX]
testy <- test[squirrel_id == testid, locY]

#count the number of other individuals within the radius (minus 1 to remove the test ind's location)
count <- test[(testx - locX)^2 + (testy - locY)^2 <= (radius)^2, .N] - 1
count #squirrels

#calculate the density based on that count
density <- round(count/((pi*radius^2)/10000), 2)
density #squirrels per hectare


#save test figure
ggsave("Output/Figures/local_density_visual.jpeg", testfig, width = 6, height = 4, unit = "in")

