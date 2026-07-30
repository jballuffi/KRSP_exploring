
#this script is a copy of final_merge_juvs. If that script has change, recopy over.
#script to merge and create the final data for analyses using litters as the unit
#merges local densities and average litter growth rates + litter info data


# load libraries and data -------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped data
dens <- readRDS("Output/Data/local_densities.rds")
growth <- readRDS("Output/Data/growth_rates_litters.rds") #this is avg growth rates of litters
moms <- readRDS("Output/Data/mom_lives.rds")
trees <- readRDS("Output/Data/tree_cone_counts.rds")
cones <- readRDS("Output/Data/cone_cache.rds")



# initial merge -------------------------------------------------

#take only moms from density data and remove sex col
dens <- dens[sex == "F"][, sex := NULL]

#merge data by mother id, year and grid
m1 <- merge(dens, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)

#merge with tree cone counts
m2 <- merge(m1, trees[, .(grid, year, cones_tree_prev)], by = c("year", "grid"), all.x = TRUE)

#merge with cone cache data
m3 <- merge(m2, cones, by = c("mother_id", "year", "grid"), all.x = TRUE)

#merge in mom age and lifespan
dat <- merge(m3, moms, by = c("mother_id"), all.x = TRUE)



# create additional factors or co variates -------------------------

#calculate mother age
dat[, mother_age := year - mother_byear]

#make grid-year
dat[, gridyear := paste0(grid, "-", year)]

#MAKE MAST YEAR COL YES OR NO
dat[year %in% mast_years, mast := "yes"][is.na(mast), mast := "no"]




# Correct issues with treatment -----------------------------------------------------

#correct for food add based on grid and year
dat[grid == "AG" & (year >= 2005 & year <= 2017), food := 1]
dat[grid == "JO" & (year > 2006 & year <= 2014), food := 1]
dat[grid == "LL" & (year >= 2005 & year <= 2011), food := 1]

#if food is NA, make foods 0
dat[is.na(food), food := 0]

#if bucket is NA, make bucket 0
dat[is.na(bucket), bucket := 0]

#are there cases where not food treatment but has bucket?
dat[food == 0 & bucket == 1, .N, by = .(grid, year)] #Just LL 2012 which we will remove

#create treatment col based on food col.
dat[food == 0 | food == 18, treatment := "Control"][food == 1, treatment := "Food add"][food == 19, treatment := "Rattle"]

#make a treatment+bucket
dat[treatment == "Control", treatment_food := "Control"]
dat[treatment == "Food add" & bucket == 1, treatment_food := "Bucket"]
dat[treatment == "Food add" & bucket == 0, treatment_food := "No Bucket"]
unique(dat$treatment_food)



# Additional info for Rattle treatment ------------------------------------

#get everyone's first year of the playback experiment
firstrattles <- dat[food == 19, .(rattle1 = min(year)), mother_id]

#merge this info in and pull number of years into rattle experiment
dat <- merge(dat, firstrattles, by = "mother_id", all.x = TRUE)
dat[food == 19, rattle_length := year - rattle1]



# clear out bad data ------------------------------------------------

#remove any rows without density
dat2 <- dat[!is.na(density)]

#subset to grids and treatments of interest (found in R/variables.R)
dat2 <- dat2[grid %in% grids & food %in% foods]

#exclude BT and JO 2015 - 2017 due to cort experiment
dat2 <- dat2[!((grid == "BT" | grid == "JO") & (year <= 2017 & year >= 2015))]

#exclude AG 2019 due to shaving experiment
dat2 <- dat2[!(grid == "AG" & year == 2019)]

#exclude LL prior to 2006 and from 2012
dat2 <- dat2[!(grid == "LL" & (year < 2006 | year == 2012))]

#remove weird years with food add on KL
dat2 <- dat2[!(grid == "KL" & food == 1)]



# standardize continuous variables ----------------------------------------

#standardize density globally
dat2[, density_std := scale(density), year]

#standardize cones globally
dat2[, cones_tree_std := scale(cones_tree_prev), year]

#standardize cone cache globally
dat2[, cache_std := scale(cache_prev), year]

#standardize birth date by grid year
dat2[, bjulian_std := scale(bjulian), gridyear]

#standardize mother age by grid year
dat2[, mother_age_std := scale(mother_age), gridyear]

#standardize litter size by grid year
dat2[, littersize_std := scale(littersize), gridyear]

#standardize growth rate by grid year
dat2[, growthrate_std := scale(growthrate), gridyear]



# finalize what are factors -----------------------------------------------

#food, treatment, bucket
dat2[, food := factor(food)][, treatment := factor(treatment)][, bucket := factor(bucket)]

#mast year
dat2[, mast := factor(mast)]

#make litter size numeric instead of integer
dat2[, littersize := as.numeric(littersize)]



# subset data for different analyses --------------------------------------

#make a control only dataset
controls <- dat2[grid == "SU" | grid == "KL"]

#make food treatment data set, All food add except the weird two years in KL
food <- dat2[food == 1]  #just food add

#make cache data set
cacheyears <- dat2[!is.na(cache_prev), unique(year)]  #get years with cache size data
cache <- dat2[(grid == "SU" | grid == "KL") & year %in% cacheyears]  #only long term control grids

#make rattle experiment dataset
rattleyears <- dat2[food == 19, unique(year)] #get years with rattles
rattle <- dat2[year %in% rattleyears & !grid == "AG"] #take all data from year of rattle playbacks but 



# save --------------------------------------------------------------------

#save final data
saveRDS(dat2, "Output/Data/full_cleaned_litters.rds")
saveRDS(controls, "Output/Data/control_cleaned_litters.rds")
saveRDS(food, "Output/Data/food_cleaned_litters.rds")
saveRDS(cache, "Output/Data/cache_cleaned_litters.rds")


