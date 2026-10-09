
#script to merge all prepped data for final analyses using litters as the unit
#merges litter growth rate and sizes, local densities, tree cone counts, cache sizes, and mother birth years


# load libraries and data -------------------------------------------------

#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

#read in prepped data
dens <- readRDS("Output/Data/local_densities.rds")
growth <- readRDS("Output/Data/growth_rates_litters.rds") #this is avg growth rates of litters
moms <- readRDS("Output/Data/mom_lives.rds")
cones <- readRDS("Output/Data/tree_cone_counts.rds")
cache <- readRDS("Output/Data/cone_cache.rds")

overlap_ids <- readRDS("Output/Data/overlap_ids.rds")
overlap_tags<- readRDS("Output/Data/overlap_eartag.rds")

#get median densities by grid + year
meandensities <- dens[, .(griddensity = median(density, na.rm = TRUE)), by = .(grid, year)]



# fix density data to use ear tag in cases where normal ID is missing -------------------------------------------------

#take left ear tag for cases where ID was both ear tags
dens[grep("-", mother_id), MTaglft := tstrsplit(mother_id, "-", keep = 1)]

#change name in overlap id
setnames(overlap_ids, "mother_id", "overlap_id")

#merge overlap ear tags to density
dens1 <- merge(dens, overlap_ids, by = "MTaglft", all.x = TRUE)

#where there is an overlap ID, make overlap ID the mother ID
dens1[!is.na(overlap_id), mother_id := overlap_id][, overlap_id := NULL][, MTaglft := NULL]



# initial merge -----------------------------------------------------------

#merge density and growth data by mother id, year and grid
m1 <- merge(dens1, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)

#merge with median densities
m1 <- merge(m1, meandensities, by = c("grid", "year"), all.x = TRUE)

#first change RR to LL now that density merge is done
m1[grid == "RR", grid := "LL"]

#anyone without local density gets the median density of their grid
m1[!is.na(density), density_source := "count"]
m1[is.na(density) & !is.na(griddensity), density_source := "median"]
m1[is.na(density), density := griddensity]

#merge with tree cone counts
m2 <- merge(m1, cones[, .(grid, year, cones_current = cones, cones_prev, cones_prev_source)], by = c("year", "grid"), all.x = TRUE)

#merge with cone cache data
m3 <- merge(m2, cache, by = c("mother_id", "year", "grid"), all.x = TRUE)

#merge in mom age and lifespan
dat <- merge(m3, moms, by = c("mother_id"), all.x = TRUE)



# create additional factors or co variate -------------------------

#if mother birth year is missing still, make birth year the year before
dat[is.na(mother_byear), mother_byear := year - 1]

#calculate mother age
dat[, mother_age := year - mother_byear]

#make grid-year
dat[, gridyear := paste0(grid, "-", year)]

#MAKE MAST YEAR COL YES OR NO
dat[year %in% mast_years, mast := "yes"][is.na(mast), mast := "no"]



# Correct issues with treatment -----------------------------------------------------

#correct for food add based on grid and year
dat[grid == "AG" & (year >= 2005 & year < 2010), food := 1] #AG between 2005 and 2010 was food add
dat[grid == "AG" & year == 2010 & !food == 19, food := 1] #AG in 2010 has some rattles in the back, otherwise food add
dat[grid == "AG" & (year > 2010 & year <= 2017), food := 1] #AG between 2010 and 2017 was food add

dat[grid == "JO" & (year > 2006 & year <= 2014), food := 1] #Jo from 2006 to 2014 was food add

dat[grid == "LL" & (year >= 2005 & year < 2010), food := 1] #Lloyd between 2005 and 2010 was food add
dat[grid == "LL" & year == 2010 & !food == 19, food := 1] #Lloyd in 2010 had some rattles but otherwise was food add
dat[grid == "LL" & year == 2011, food := 1] #Lloyd in 2011 was just food add

#if food is NA, make foods 0
dat[is.na(food), food := 0]

#if bucket is NA, make bucket 0
dat[is.na(bucket), bucket := 0]

#are there cases where not food treatment but has bucket?
dat[food == 0 & bucket == 1, .N, by = .(grid, year)] #Just LL 2012 which we will remove

#cases where rattle has a bucket? According to the PB bucket table this midden was given PB in 2010.
#However it looks like there is a duplicate value for this mom, so I wonder if she had two middens and that one was given PB
#and the other was off grid
dat <- dat[!(food == 19 & bucket == 1)]

#check for any duplicate litters
dat[duplicated(litter_id) == TRUE]

#create treatment col based on food col.
dat[food == 0 | food == 18, treatment_gr := "Control"][food == 1, treatment_gr := "Food add"][food == 19, treatment_gr := "Rattle"]

#make a treatment+bucket
dat[treatment_gr == "Control", treatment_ind := "Control"]
dat[treatment_gr == "Food add" & bucket == 1, treatment_ind := "Bucket"]
dat[treatment_gr == "Food add" & bucket == 0, treatment_ind := "No Bucket"]
dat[treatment_gr == "Rattle", treatment_ind := "Rattle"]
unique(dat$treatment_ind)



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
dat2 <- dat2[!(food == 1 & (grid == "SU" | grid == "KL"))]



# standardize continuous variables ----------------------------------------

#standardize density globally
dat2[, density_std := scale(density)]

#standardize cones globally
dat2[, cones_std := scale(cones_prev)]

#standardize birth date by grid year
dat2[, bjulian_std := scale(bjulian), gridyear]

#standardize mother age by grid year
dat2[, mother_age_std := scale(mother_age)]

#standardize litter size by grid year
dat2[, littersize_std := scale(littersize), gridyear]

#standardize growth rate by grid year
dat2[, growthrate_std := scale(growthrate), gridyear]



# make density categories ---------------------------------------------------

#get density quantiles
dens25Q <- dat2[, round(quantile(density_std)[2], 2)]
dens75Q <- dat2[, round(quantile(density_std)[4], 2)]

#categorize density based on quantiles
dat2[density_std < dens25Q, density_cat := "low"]
dat2[density_std > dens25Q & density_std < dens75Q, density_cat := "medium"]
dat2[density_std > dens75Q, density_cat := "high"]



# finalize what are factors -----------------------------------------------

#food, treatment, bucket
dat2[, food := factor(food)][, treatment_gr := factor(treatment_gr)][, bucket := factor(bucket)][, treatment_ind := factor(treatment_ind)]

#mast year
dat2[, mast := factor(mast)]

#make litter size numeric instead of integer
dat2[, littersize := as.numeric(littersize)]

#density category with levels
dat2[, density_cat := factor(density_cat, levels = c("low", "medium", "high"))]



# make cache size data set ---------------------------------------------------

#take control years with cache data
cache <- dat2[(grid == "SU" | grid == "KL") & !is.na(cache_prev)]

#standardize cone cache globally
cache[, cache_std := scale(cache_prev)]

#cache[, cache_cat := cut(cache_std, breaks = 3, labels = c("small", "medium", "large"))]

#get cache size quantiles
cache25Q <- cache[, round(quantile(cache_std, na.rm = TRUE)[2], 2)]
cache75Q <- cache[, round(quantile(cache_std, na.rm = TRUE)[4], 2)]

#categorize density based on quantiles
cache[cache_std < cache25Q, cache_cat := "small"]
cache[cache_std > cache25Q & cache_std < cache75Q, cache_cat := "medium"]
cache[cache_std > cache75Q, cache_cat := "large"]

#make cache cat a factor with levels
cache[, cache_cat := factor(cache_cat, levels = c("small", "medium", "large"))]



# subset data for other analyses --------------------------------------

#make a control only data set
controls <- dat2[grid == "SU" | grid == "KL"]
controls[, treatment_ind := factor(treatment_ind)][, treatment_gr := factor(treatment_gr)]

#make food 2 data set: control and food add grids during food years
foodyears <- dat2[food == 1, unique(year)] #get years with rattles
food <- dat2[year %in% foodyears & (food == 1 | grid == "SU" | grid == "KL")]
food[, treatment_ind := factor(treatment_ind)][, treatment_gr := factor(treatment_gr)]


#make rattle experiment dataset
rattleyears <- dat2[food == 19, unique(year)] #get years with rattles
rattle <- dat2[year %in% rattleyears & (food == 19 | food == 18 | food == 0)] #take all data from year of rattle playbacks
rattle[, treatment_ind := factor(treatment_ind)][, treatment_gr := factor(treatment_gr)]



# save --------------------------------------------------------------------

#save final data
saveRDS(dat2, "Output/Data/full_cleaned_litters.rds")
saveRDS(controls, "Output/Data/control_cleaned_litters.rds")
saveRDS(food, "Output/Data/food_cleaned_litters.rds")
saveRDS(cache, "Output/Data/cache_cleaned_litters.rds")
saveRDS(rattle, "Output/Data/rattle_cleaned_litters.rds")

write.csv(rattle, "Output/Data/rattle_cleaned_litters.csv")
