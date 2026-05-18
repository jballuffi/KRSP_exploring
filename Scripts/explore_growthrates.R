
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



# early juvenile data -----------------------------------------------------

juvs1 <- as.data.table(tbl(con, "dbajuvenile") %>% collect())





# later litter data -------------------------------------------------------

#get litter data
lit <- as.data.table(tbl(con, "litter") %>% collect())

#take only cols of interest
lit <- lit[, .(squirrel_id, yr, )]



# later juvenile data -----------------------------------------------------------

#get juvenile data 
juvs <- as.data.table(tbl(con, "juvenile") %>% collect())
#this data is only from 2013 - 2025

#take only cols of interest
juvs <- juvs[, .(squirrel_id, litter_id, sex, 
                 date1 = date_created, date2 = last_updated, 
                 weight1 = weight, weight2 = tagWT)]

#fix up dates
juvs[, date1 := tstrsplit(date1, " ", keep = 1)]
juvs[, date2 := tstrsplit(date2, " ", keep = 1)]
juvs[, date1 := ymd(date1)][, date2 := ymd(date2)]
juvs[, y := year(date1)]
juvs[, m := month(date1)]

#any weights as zero make NA 
juvs[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]

#number of cases where date 1 and date 2 are the same. What's going on here?
juvs[date1 == date2, .N]

#number of cases where weight 1 and weight 2 are the same.
juvs[weight1 == weight2, .N]

#remove cases where both weights are the same
juvs <- juvs[!weight1 == weight2]

#remove cases where weight 1 is bigger than weight 1
juvs <- juvs[!weight1 > weight2]

#calculate number of days between first and second litter
juvs[, days := as.integer(date2 - date1)]

#calculate growth rate between nests (g)
juvs[, growthrate := (weight2 - weight1)]


hist(juvs$growthrate)

hist(juvs$weight1)

