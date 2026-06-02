
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



# read in data ------------------------------------------------------------

#get litter data
lit <- as.data.table(tbl(con, "litter") %>% collect())

#get individual juvenile data 
juv <- as.data.table(tbl(con, "juvenile") %>% collect())

#get list of moms with buckets (food add)
buckets <- fread("Input/fed_squirrels.csv")



# prep bucket info --------------------------------------------------------

#remove missing squirrel IDs
buckets <- buckets[!is.na(squirrel_id_spring)]

#create a col called bucket and remove 
buckets[, bucket := 1][, bucket_reflo := NULL]

#rename id col
setnames(buckets, "squirrel_id_spring", "mother_id")



# prep litter data and merge with bucket list -------------------------------------------------------

#take only cols of interest
lit2 <- lit[, .(mother_id = squirrel_id, litter_id = id,
                 year = yr, grid, br, food, ln,
               bdate = fieldBDate, date1, date2 = tagDt)]

#create dates
lit2[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)]

#merge litter and buckets
lit2 <- merge(lit2, buckets, by = c("grid", "year", "mother_id"), all.x = TRUE)



# Assign grid treatments --------------------------------------------------

#are there cases where not food treatment but has bucket?
lit2[food == 0 & bucket == 1, unique(year), grid]
#yes... why is this?

#fix food add treatment so all squirrels during certain years and in certain grids are correctly labeled as food add
#these years are taken from Ben's instructions on google doc
lit2[grid == "AG" & (year >= 2005 & year <= 2017), food := 1]
lit2[grid == "JO" & (year >= 2006 & year <= 2014), food := 1]
lit2[grid == "LL" & (year >= 2005 & year <= 2011), food := 1]

#UNSURE HERE for now change LL in 2012 to food add
dat[grid == "LL" & year == 2012, food := 1]         ## DOUBLE CHECK



# prep juvenile data -----------------------------------------------------------

#take only cols of interest
juv2 <- juv[, .(squirrel_id, litter_id, sex = factor(sex), weight1 = weight, weight2 = tagWT)]

#any weights as zero make NA 
juv2[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]

#calculate litter size
juv2[, littersize := .N, litter_id]



# merge juvenile and litter data and clean based on weight data -----------------------------------------------------

#merge juv and litter data by the litter ID
dat <- merge(lit2, juv2, by = "litter_id", all.y = TRUE)

#take only first litters for this analysis, as done by Delaney
dat <- dat[ln == 1]

#take only cases where we have both measures of weights
dat <- dat[!(is.na(weight1) | is.na(weight2))]

#remove cases where weight 1 and weight 2 are the same.
dat <- dat[!weight1 == weight2] #this looks to be usually when first nests were missed

#remove cases where weight 1 is bigger than weight 1
dat <- dat[!weight1 > weight2] #this is a rare scenario

#THE FOLLOWING WASN'T IN DELANEY
#remove any cases where weight 1 is over 50 g
dat <- dat[!weight1 > 50]

#remove any cases where weight 2 was over 100 g
dat <- dat[!weight2 > 100]

#remove duplicate squirrels
dat2 <- dat[!duplicated(squirrel_id) == TRUE]



# calculate growth rates --------------------------------------------------

#get time difference
dat2[, days := as.integer(date2 - date1)]

#any cases where the two dates were the same make days NA
dat2[days == 0, days := NA]

#get growth rate over that time (g/day)
dat2[, growthrate := (weight2 - weight1)/days]

#quick look at distribution
hist(dat2$growthrate)

#checking my work. This should be 0 due to prior cleaning of weights
dat2[growthrate <= 0 | growthrate > 10, .N]

#remove NA growth rates
dat2 <- dat2[!is.na(growthrate)]



# Finalize treatments and factors -----------------------------------------------------

#categorize litter size into a 4-part factor
dat2[, litterf := cut(littersize, breaks = 4, labels = c("1-2", "2-3", "5-6", "7-8"))]

#make NA foods 0
dat2[is.na(food), food := 0]

#if bucket is NA and on a food add grid, make bucket 0
dat2[food == 1 & is.na(bucket), bucket := 0]

#make food, bucket, and breeding status factors
dat2[, food := factor(food)][, br := factor(br)][, bucket := factor(bucket)]



# Get basic summaries -----------------------------------------------------

#How many growth rates?
dat2[, .N]

#Get sample size by treatment
dat2[, .N, food]



# save ---------------------------------------------------

#save final growth rate
saveRDS(dat2, "Output/Data/growth_rates.rds")


