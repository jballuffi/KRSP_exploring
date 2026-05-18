
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
lit2 <- lit2[, .(mother_id = squirrel_id, litter_id = id, yr, grid, br, food, ln,
               bdate = fieldBDate, date1, date2 = tagDt)]

#fix up dates
lit2[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)]



# later juvenile data -----------------------------------------------------------

#get juvenile data 
juv2 <- as.data.table(tbl(con, "juvenile") %>% collect())
#this data is only from 2013 - 2025

#take only cols of interest
juv2 <- juv2[, .(squirrel_id, litter_id, sex, 
                 #jdate1 = date_created, jdate2 = last_updated, 
                 weight1 = weight, weight2 = tagWT)]

#any weights as zero make NA 
juv2[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]



#WHAT ARE THESE DATES??????
# juv2[, yr := tstrsplit(jdate1, "-", keep = 1)]
# juv2[, .N, yr]
#based on this it I believe that the dates are dates data was entered. 2014 has 12299 entries



# merge juv2 and lit2 and clean based on weight data -----------------------------------------------------

#merge
dat2 <- merge(lit2, juv2, by = "litter_id", all.y = TRUE)

#calculate litter size
dat2[, littersize := .N, litter_id]



# calculate growth rates --------------------------------------------------

#cases where weight 1 and weight 2 are the same.
dat2[weight1 == weight2] #this looks to be usually when first nests were missed

#number of cases where weight 1 is bigger than weight 1
dat2[weight1 > weight2, .N] #this is a rare scenario

#get time difference
dat2[, days := as.integer(date2 - date1)]

#any cases where the two dates were the same make days NA
dat2[days == 0, days := NA]

#get growth rate over that time (g/day)
dat2[, growthrate := (weight2 - weight1)/days]

#quick look at distribution
hist(dat2$growthrate)

#anything less than or equal to zero OR greater than 10 make NA
dat2[growthrate <= 0 | growthrate > 10, growthrate := NA]







