
#script for collecting tree cone data

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


# get data from database --------------------------------------------------

#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

#get cone data
cones <- as.data.table(tbl(con, "cones") %>% collect())

#take cols of interest
cones <- cones[, .(grid = Grid, year = as.integer(Year), tree_id, N = as.integer(NumNew))]



# get cone count by grid and year -----------------------------------------

#doesn't seem to be any data until 1988
cones <- cones[year > 1987]

#log transform cone count plus 1 (because of zeros)
cones[, Nlog := log(N + 1)]

#get avg cone count by grid and year
cones_avg <- cones[, .(cones_tree = mean(Nlog, na.rm = TRUE)), by = .(year, grid)]

#lag this value to get t-1, or last year's cones count
cones_avg[, cones_tree_prev := lag(cones_tree), grid]



# Use Jo for BT when BT is missing (2010 + 2018 + 2023) --------------------------

#what years do we have prev cone count for BT
cones_avg[!is.na(cones_tree_prev) & grid == "BT", unique(year)]
#we are missing prev cone counts for BT in 2010, 2018, and 2023

#get prev cone count from Jo for missing BT years
BTgaps <- cones_avg[grid == "JO" & (year == 2010 | year == 2018 | year == 2023)]
BTgaps[, grid := "BT"]

#remove BT in 2018
cones_avg <- cones_avg[!(grid == "BT" & year == 2018)]

#merge back with data
cones_avg <- rbind(cones_avg, BTgaps, fill = TRUE)



# Use SU to fill in for RR (2010) -----------------------------------------

#take 
RR2010 <- cones_avg[grid == "SU" & year == 2010]
RR2010[, grid := "RR"]

cones_avg <- rbind(cones_avg, RR2010, fill = TRUE)



# Plot --------------------------------------------------------------------

conesfinal <- cones_avg[grid %in% grids]

setorder(conesfinal, grid, year)

#plot
ggplot(conesfinal)+
  geom_path(aes(x = year, y = cones_tree_prev, color = grid, group = grid))



# save --------------------------------------------------------------------

saveRDS(cones_avg, "Output/Data/tree_cone_counts.rds")
