
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

#get litter data
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



# Use Jo for BT when BT is missing (2018 + 2023) --------------------------

#what years do we have prev cone count for BT
cones_avg[!is.na(cones_tree_prev) & grid == "BT", unique(year)]

#get prev cone count from Jo for those years
JO2018 <- cones_avg[grid == "JO" & year == 2018, cones_tree_prev]
JO2023 <- cones_avg[grid == "JO" & year == 2023, cones_tree_prev]

#use those values to make a short data frame for missing BT years
BTgap <- data.table(
  grid = "BT",
  year = c(2018, 2023),
  cones_tree_prev = c(JO2018, JO2023)
)

#merge back with data
cones_avg <- rbind(cones_avg, BTgap, fill = TRUE)

setorder(cones_avg, grid, year)

#plot
ggplot(cones_avg)+
  geom_path(aes(x = year, y = cones_tree_prev, color = grid, group = grid))



# save --------------------------------------------------------------------

saveRDS(cones_avg, "Output/Data/tree_cone_counts.rds")
