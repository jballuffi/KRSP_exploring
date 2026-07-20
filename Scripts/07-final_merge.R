
#script to merge and create the final data for analyses going forward
#merges local densities and juvenile growth rates + litter info data


# load libraries and data -------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped data
dens <- readRDS("Output/Data/local_densities.rds")
growth <- readRDS("Output/Data/growth_rates.rds")
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


#up until this point the plan was to only use KL, SU, AG, JO, LL, BT
#should I go back and try to include SX from 2010? No data from RR. 
#no data for BT before 2016
#we can make back of AG rattle in 2010, but its currently listed as all food add.
#I don't see any of those reflocs in the data, but there are a lot of NA reflocs for that year in AG that don't have densities.
#do you have the IDs and years of squirrels who were in the rattle experiment? This is the easiest way
#no data for BT 2010
#no indication of rattle playback for KL but doesn't matter much since its chickadee I guess. Could add 2010 KL into the rattle test if we find this data 



# finalize what are factors -----------------------------------------------

#food, treatment, bucket
dat2[, food := factor(food)][, treatment := factor(treatment)][, bucket := factor(bucket)]

#BR
dat2[, br := factor(br)]

#mast year
dat2[, mast := factor(mast)]



# current summaries -------------------------------------------------------

foodaddsample <- dat2[food == 1, .N, by = .(grid, year)]
setorder(foodaddsample, grid, year)

#summary by treatment
dat2[, .N, by = .(treatment)]

#summary by bucket
dat2[, .N, bucket]

#create a table showing sample sizes by categories for non-control grids
summary <- dat2[!(grid == "SU" | grid == "KL"), .N, by = .(grid, year, treatment)]
setorder(summary, grid, year)




# get amount of missing data ----------------------------------------------

#growth rate, litter size, tree cone, midden cone, mother birth year

#make function to count NAs using the sum function
getNA <- function(x){
  NAcount <- sum(is.na(x))
  return(NAcount)
}

#collect number of missing data by year and grid
missingdata <- 
  dat2[, .(growthrate = getNA(growthrate), 
         littersize = getNA(littersize),
         density = getNA(density),
         cones_tree = getNA(cones_tree_prev), 
         cache_prev = getNA(cache_prev), 
         mother_byear = getNA(mother_byear)),
     by = .(year, grid)]



# save --------------------------------------------------------------------

#save final data
saveRDS(dat2, "Output/Data/final_data.rds")

#save and send to ben
write.csv(summary, "Output/Data/N_summary_treatments.csv")

#save output of missing data
write.csv(missingdata, "Output/Data/Missing_data_summary.csv")
