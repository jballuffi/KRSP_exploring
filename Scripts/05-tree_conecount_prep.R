
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
cones <- cones[, .(grid = Grid, year = Year, tree_id, N = as.integer(NumNew))]



# get cone count by grid and year -----------------------------------------

#doesn't seem to be any data until 1988
cones <- cones[year > 1987]

#take only grids of interest (list of grids is in R/variables.R)
cones <- cones[grid %in% grids]

#log transform cone count plus 1 (because of zeros)
cones[, Nlog := log(N + 1)]

#get avg cone count by grid and year
cones_avg <- cones[, .(cones_tree = mean(Nlog, na.rm = TRUE)), by = .(year, grid)]

#plot
ggplot(cones_avg)+
  geom_path(aes(x = year, y = cones_tree, color = grid, group = grid))



# save --------------------------------------------------------------------

saveRDS(cones_avg, "Output/Data/tree_cone_counts.rds")
