
#script to merge and create the final data for analyses going forward
#merges local densities and juvenile growth rates + litter info data


# load libraries and data -------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped data
dens <- readRDS("Output/Data/local_densities.rds")
growth <- readRDS("Output/Data/growth_rates.rds")
moms <- readRDS("Output/Data/mom_lives.rds")



# initial merge -------------------------------------------------

#take only moms from density data and remove sex col
dens <- dens[sex == "F"][, sex := NULL]

#merge data by mother id, year and grid
m1 <- merge(dens, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)

#merge in mom age and lifespan
dat <- merge(m1, moms, by = c("grid", "mother_id"), all.x = TRUE)



# calculate additional co variates and standardize -------------------------

#calculate mother age
dat[, mother_age := year - mother_byear]

#make grid-year
dat[, gridyear := paste0(grid, "-", year)]

#MAKE MAST YEAR COL YES OR NO
dat[year %in% mast_years, mast := "yes"][is.na(mast), mast := "no"]

#create a category for density
dat[, densityf := cut(round(density), breaks = 4, labels = c("0 - 2", "2 - 4", "4 - 6", "6 - 8"))]

#standardize density globally
dat[, density_std := scale(density), year]

#create a category for density
dat[, densityf_std := cut(round(density_std, 1), breaks = 4)]

#standardize cones globally
### next step ^

#standardize birth date by grid year
dat[, bjulian_std := scale(bjulian), gridyear]

#standardize litter size by grid year
dat[, littersize_std := scale(littersize), gridyear]

#standardize growth rate by grid year
dat[, growthrate_std := scale(growthrate), gridyear]



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



# Additional info for Rattle treatment ------------------------------------

#get everyone's first year of the playback experiment
firstrattles <- dat[food == 19, .(rattle1 = min(year)), mother_id]

#merge this info in and pull number of years into rattle experiment
dat <- merge(dat, firstrattles, by = "mother_id", all.x = TRUE)
dat[food == 19, rattle_length := year - rattle1]



# clear out bad data ------------------------------------------------

#remove any rows without density
dat2 <- dat[!is.na(density)]

#list the grids we are interested in
grids <- c("KL", "SU", "AG", "JO", "LL", "BT")

#list the treatments we are interested in 
foods <- c(1, 0, 18, 19)

#subset to that list of grids and treatments, excluding data where density is missing
dat2 <- dat2[grid %in% grids & food %in% foods]

#exclude BT and JO 2015 - 2017 due to cort experiment
dat2 <- dat2[!((grid == "BT" | grid == "JO") & (year <= 2017 & year >= 2015))]

#exclude AG 2019 due to shaving experiment
dat2 <- dat2[!(grid == "AG" & year == 2019)]

#exclude LL prior to 2006 and from 2012
dat2 <- dat2[!(grid == "LL" & (year < 2006 | year == 2012))]



# finalize what are factors -----------------------------------------------

#make factors
dat2[, food := factor(food)][, treatment := factor(treatment)]

#make factors
dat2[, br := factor(br)][, bucket := factor(bucket)]



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



# save --------------------------------------------------------------------

#save final data
saveRDS(dat2, "Output/Data/final_data.rds")

#save and send to ben
write.csv(summary, "Output/Data/N_summary_treatments.csv")


