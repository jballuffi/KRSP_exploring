
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


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
lit2[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)][, bjulian := yday(bdate)]

#merge litter and buckets
lit2 <- merge(lit2, buckets, by = c("grid", "year", "mother_id"), all.x = TRUE)



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

#calculate the number of days in between weight measures
dat[, days := as.integer(date2 - date1)]

#take only first litters for this analysis, as done by Delaney
dat <- dat[ln == 1]

#take only cases where we have both measures of weights
dat <- dat[!(is.na(weight1) | is.na(weight2))]

#remove cases where weight 1 and weight 2 are the same.
dat <- dat[!weight1 == weight2] #this looks to be usually when first nests were missed

#remove cases where weight 1 is bigger than weight 1
dat <- dat[!weight1 > weight2] #this is a rare scenario

#METHODS BY FISHER 2017 
#remove any cases where weight 1 is over 50 g
dat <- dat[!weight1 > 50]

#remove any cases where weight 2 was over 100 g
dat <- dat[!weight2 > 100]

#remove cases when there were less than 5 days in between weight measures
dat <- dat[!days < 5]

#remove duplicate squirrels
dat2 <- dat[!duplicated(squirrel_id) == TRUE] ##WHY?



# calculate growth rates --------------------------------------------------

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



# finalize data sets --------------------------------------------------------

#take only cols of interest
dat2 <- dat2[, .(litter_id, year, grid, mother_id, food, bucket, bdate, bjulian, sex, littersize, growthrate)]

#average growth rate by litter
datlitter <- dat2[, .(growthrate = mean(growthrate)),
                  by = .(litter_id, year, grid, mother_id, food, bucket, bdate, bjulian, littersize)]



# Get basic summaries -----------------------------------------------------

#How many growth rates?
dat2[, .N]

#Get sample size by treatment
dat2[, .N, food]



# save ---------------------------------------------------

#save final growth rate
saveRDS(dat2, "Output/Data/growth_rates_juvs.rds")
saveRDS(datlitter, "Output/Data/growth_rates_litters.rds")


