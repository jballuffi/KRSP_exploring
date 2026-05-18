
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




# later litter data -------------------------------------------------------

#get litter data
lit2 <- as.data.table(tbl(con, "litter") %>% collect())

#take only cols of interest
lit2 <- lit2[, .(squirrel_id, litter_id = id, yr, br, food,
               bdate = fieldBDate, date1, date2 = tagDt)]

#fix up dates
lit2[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)]



# later juvenile data -----------------------------------------------------------

#get juvenile data 
juv2 <- as.data.table(tbl(con, "juvenile") %>% collect())
#this data is only from 2013 - 2025

#take only cols of interest
juv2 <- juv2[, .(squirrel_id, litter_id, sex, 
                 #date1 = date_created, date2 = last_updated, 
                 weight1 = weight, weight2 = tagWT)]

# #fix up dates
# juv2[, date1 := tstrsplit(date1, " ", keep = 1)]
# juv2[, date2 := tstrsplit(date2, " ", keep = 1)]
# juv2[, date1 := ymd(date1)][, date2 := ymd(date2)]
# juv2[, m := month(date1)]

#any weights as zero make NA 
juv2[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]



# merge juv2 and lit2 -----------------------------------------------------







#number of cases where weight 1 and weight 2 are the same.
juvs[weight1 == weight2, .N]

#remove cases where both weights are the same
juvs <- juvs[!weight1 == weight2]

#remove cases where weight 1 is bigger than weight 1
juvs <- juvs[!weight1 > weight2]



