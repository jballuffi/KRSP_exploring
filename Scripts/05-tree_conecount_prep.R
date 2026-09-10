
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



# starting data frame ------------------------------------------------------

#create starting dataset for mean cone density on grids by year
#list years
years <- 1988:2026

#pair with grids of interest
grid_data <- data.table(CJ(grid = grids, year = years))




# get cone count by grid and year -----------------------------------------

#doesn't seem to be any data until 1988
cones <- cones[year > 1987]

#log transform cone count plus 1 (because of zeros)
cones[, Nlog := log(N + 1)]

#get avg cone count by grid and year
cones_grid <- cones[, .(cones_tree_grid = mean(Nlog, na.rm = TRUE)), by = .(year,grid)]
cones_year <- cones[, .(cones_tree_year = mean(Nlog, na.rm = TRUE)), by = .(year)]


#merge with starting dataframe
dat <- merge(grid_data, cones_grid, by = c("year", "grid"), all.x = TRUE)
dat <- merge(dat, cones_year, by = "year", all.x = TRUE)

dat[is.na(cones_tree_grid), cones_tree_grid := cones_tree_year][, cones_tree_year := NULL]

#lag this value to get t-1, or last year's cones count
dat[, cones_tree_prev := lag(cones_tree_grid), grid]



# Use Jo for BT when BT is missing (2010 + 2018 + 2023) --------------------------

# #what years do we have prev cone count for BT
# cones_avg[!is.na(cones_tree_prev) & grid == "BT", unique(year)]
# #we are missing prev cone counts for BT in 2010, 2018, and 2023
# 
# #get prev cone count from Jo for missing BT years
# BTgaps <- cones_avg[grid == "JO" & (year == 2010 | year == 2018 | year == 2023)]
# BTgaps[, grid := "BT"]
# 
# #remove BT in 2018
# cones_avg <- cones_avg[!(grid == "BT" & year == 2018)]
# 
# #merge back with data
# cones_avg <- rbind(cones_avg, BTgaps, fill = TRUE)
# 
# #take only grids of interest
# cones_avg <- cones_avg[grid %in% grids]



# Plot --------------------------------------------------------------------

setorder(dat, grid, year)

#plot
ggplot(dat)+
  geom_path(aes(x = year, y = cones_tree_prev, color = grid, group = grid))



# save --------------------------------------------------------------------

saveRDS(cones_avg, "Output/Data/tree_cone_counts.rds")
