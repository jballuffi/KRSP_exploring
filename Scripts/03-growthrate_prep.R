
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

#read in extra life history data from 2010 playback experiments
extra <- fread("Input/2010_offgrid_lifehistory.csv")



# prep bucket info --------------------------------------------------------

#remove missing squirrel IDs
buckets <- buckets[!is.na(squirrel_id_spring)]

#create a col called bucket and remove 
buckets[, bucket := 1][, bucket_reflo := NULL]

#remove duplicates
buckets <- buckets[duplicated(buckets) == FALSE]

#rename id col
setnames(buckets, "squirrel_id_spring", "mother_id")

#check for duplicates
janitor::get_dupes(buckets)



# prep litter data and merge with bucket list -------------------------------------------------------

#take only cols of interest
lit2 <- lit[, .(mother_id = squirrel_id, 
                MTaglft = mTagLft,
                litter_id = id,
                locx, locy,
                year = yr, grid, br, food, ln,
                bdate = fieldBDate, date1, date2 = tagDt)]

#create dates
lit2[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)]

#merge litter and buckets
lit2 <- merge(lit2, buckets, by = c("grid", "year", "mother_id"), all.x = TRUE)

#check for duplicates
janitor::get_dupes(lit2)



# prep juvenile data -----------------------------------------------------------

#take only cols of interest
juv2 <- juv[, .(squirrel_id, litter_id, sex = factor(sex), weight1 = weight, weight2 = tagWT)]

#any weights as zero make NA 
juv2[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]

#check for duplicates
janitor::get_dupes(juv2)



# merge juvenile and litter data, calculate litter size -----------------------------------------------------

#merge juv and litter data by the litter ID
database <- merge(lit2, juv2, by = "litter_id", all.y = TRUE)

#rename RR and SX/SUX grids
database[grid == "SX" | grid == "SUX", grid := "SU"]

#check for duplicates
janitor::get_dupes(database)

#calculate litter sizes
database[, littersize := .N, litter_id]



# collect data from 2010 rattle experiments ----------------------------

#take just rattle data from extras
extra <- extra[food == 19]

#get list of ear tags in extra data
extra_tags <- extra[, unique(MTaglft)]
#some of those squirrels exist in the database juv/litter data already but aren't scored as rattles
#remove the moms/litters who are in extra from database

#find those moms based on ear tags and score as rattles 
database[MTaglft %in% extra_tags & year == 2010, food := 19]

#return all the ear tags that are in both the extra data and the database
overlap_tags <- database[MTaglft %in% extra_tags & year == 2010, unique(MTaglft)]

#get a table of the IDs associated with those overlapping ear tags
overlap_ids <- database[MTaglft %in% extra_tags & year == 2010, .(mother_id, MTaglft)][duplicated(mother_id) == FALSE]

#remove the moms already in the database from the extras
dat2010 <- extra[!(MTaglft %in% overlap_tags)]

#create a pseudo ID for moms using their eartags
dat2010 <- dat2010[, mother_id := paste0(MTaglft, "-", MTagrt)]

#make litter id
dat2010[, litter_id := paste0(mother_id, "-", year, "-", ln)]

#check for duplicates
janitor::get_dupes(dat2010)

#set dates
dat2010[, date1 := mdy(date1)][, date2 := mdy(date2)][, bdate := mdy(bdate)]

#take only cols of interest
dat10 <- dat2010[, .(grid, year, 
                     mother_id, MTaglft,
                     litter_id, 
                     food, ln, bdate, date1, date2, 
                     locx = LocX, locy = LocY,
                     squirrel_id = DNA2, weight1, weight2, sex)]

#check for duplicates
janitor::get_dupes(dat10)

#remove two duplicates that have appeared
dat10 <- dat10[duplicated(dat10) == FALSE]

#calculate litter size
dat10[, littersize := .N, litter_id]



# merge 2010 data with database date --------------------------------------

#rbind with data from database
dat <- rbind(database, dat10, fill = TRUE)

#check for duplicates
janitor::get_dupes(dat)

#take only first litters for this analysis, as done by Delaney
dat <- dat[ln == 1]



# calculate sex ratios for litters ---------------------------------------------------

#get number of males in every litter
sexcounts <- dat[, .N, by = .(litter_id, sex)]

#make the output wide
sexratios <- dcast(sexcounts, litter_id ~ sex)

#fill in NAs with zeros
sexratios[is.na(M), M := 0][is.na(`F`), `F` := 0]

#calculate the ratio of males in litter
sexratios[, sexratio := M/(`F` + M)]



# Clean based on weight data and dates  ---------------------------------------------

#take only cases where we have both weights
dat <- dat[!(is.na(weight1) | is.na(weight2))]

#calculate the number of days in between weight measures
dat[, days := as.integer(date2 - date1)]

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

#check for duplicates
janitor::get_dupes(dat)



# calculate growth rates --------------------------------------------------

#any cases where the two dates were the same make days NA
dat[days == 0, days := NA]

#get growth rate over that time (g/day)
dat[, growthrate := (weight2 - weight1)/days]

#quick look at distribution
hist(dat$growthrate)

#checking my work. This should be 0 due to prior cleaning of weights
dat[growthrate <= 0 | growthrate > 10, .N]



# finalize data sets --------------------------------------------------------

#remove NA growth rates
dat2 <- dat[grid %in% grids]

#average growth rate by litter
datlitter <- dat2[, .(growthrate = mean(growthrate),
                      weight1 = mean(weight1),
                      weight2 = mean(weight2)),
                  by = .(litter_id, year, grid, mother_id, MTaglft,
                         food, bucket, bdate, date1, date2, littersize)]


#merge back in with litter data set
datlitter <- merge(datlitter, sexratios[, .(litter_id, sexratio)], by = "litter_id", all.x = TRUE)

#check for duplicates
janitor::get_dupes(datlitter)



# Fix birth dates ---------------------------------------------------------

datlitter[is.na(bdate) & weight1 < 50]


#create col for source of part date
datlitter[!is.na(bdate), part_status := "observed"]

#if missing part date and first weight is less than 50, it can be estimated
datlitter[is.na(bdate) & weight1 < 50, part_status := "estimated"]

#estimate part date based on weight 1 for those missing part dates
datlitter[is.na(bdate) & weight1 < 50, bdate := date1 - round((weight1 - 8)/1.8)]

#convert to julian birth day
datlitter[, bjulian := yday(bdate)]



# Get basic summaries -----------------------------------------------------

#How many growth rates?
datlitter[, .N]

#Get sample size by treatment
datlitter[, .N, food]

#missing values
datlitter[is.na(littersize), .N]
datlitter[is.na(growthrate), .N]
datlitter[is.na(bjulian), .N]
datlitter[is.na(sexratio), .N]

#check for duplicate litter IDs
datlitter[duplicated(litter_id) == TRUE]



# save ---------------------------------------------------

#save final growth rate
saveRDS(datlitter, "Output/Data/growth_rates_litters.rds")

saveRDS(overlap_ids, "Output/Data/overlap_ids.rds")
saveRDS(overlap_tags, "Output/Data/overlap_eartag.rds")


