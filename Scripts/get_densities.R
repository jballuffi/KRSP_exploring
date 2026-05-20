
library(krsp)
library(ggplot2)
library(data.table)
library(lubridate)

#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)


#erin's code for 130 m radius
#




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

