
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




# get census data ---------------------------------------------------------

#get census data
census <- as.data.table(tbl(con, "census") %>% collect())

#take only cols of interest
census <- census[, .(cdate = census_date, grid = gr, squirrel_id)]



# calculate grid density by census ----------------------------------------

#make a date
census[, cdate := ymd(cdate)]

#get year and month
census[, yr := year(cdate)][, m := month(cdate)]

#get total number of individual per year and grid
density <- census[, .(density = length(unique(squirrel_id))), by = .(yr, grid)]

#remove the missing grid
density <- density[!grid == ""]



# save densities ----------------------------------------------------------

saveRDS(density, "Output/Data/grid_densities.rds")

