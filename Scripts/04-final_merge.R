
#script to merge and create the final data for analyses going forward
#merges local densities and juvenile growth rates + litter info data


# load libraries and data -------------------------------------------------

library(data.table)
library(ggplot2)

#read in prepped data
dens <- readRDS("Output/Data/local_densities.rds")
growth <- readRDS("Output/Data/growth_rates.rds")



# initial merge and clean -------------------------------------------------

#take only moms from density data and remove sex col
moms <- dens[sex == "F"][, sex := NULL]

#merge data by mother id, year and grid
dat <- merge(moms, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)



# Finalize treatments and factors -----------------------------------------------------

#if food is NA, make foods 0
dat[is.na(food), food := 0]

#if bucket is NA, make bucket 0
dat[is.na(bucket), bucket := 0]

#create treatment col based on food col.
dat[food == 0 | food == 18, treatment := "Control"][food == 1, treatment := "Food add"][food == 19, treatment := "Rattle"]

#make factors
dat[, br := factor(br)][, bucket := factor(bucket)]

#are there cases where not food treatment but has bucket?
dat[food == 0 & bucket == 1, .N, by = .(grid, year)] #WHY?



# remove data from grid/years that would be problematic ------------------------------------------------

#list the grids we are interested in
grids <- c("KL", "SU", "AG", "JO", "LL", "BT")

#list the treatments we are interested in 
foods <- c(1, 0, 18, 19)

#subset to that list of grids and treatments, excluding data where density is missing
dat2 <- dat[grid %in% grids & food %in% foods & !is.na(density)]

#test bucket issue again
dat2[food == 0 & bucket == 1, .N, by = .(grid, year)] #WHY?

#exclude BT and JO 2015 - 2017 due to cort experiment
dat2 <- dat2[!((grid == "BT" | grid == "JO") & (year <= 2017 & year >= 2015))]

#exclude AG 2019 due to shaving experiment
dat2 <- dat2[!(grid == "AG" & year == 2019)]

#exclude LL prior to 2006 and from 2012
dat2 <- dat2[!(grid == "LL" & (year < 2006 | year == 2012))]


# #fix food add treatment so all squirrels during certain years and in certain grids are correctly labeled as food add
# #these years are taken from Ben's instructions on google doc
# lit2[grid == "AG" & (year >= 2005 & year <= 2017), food := 1]
# lit2[grid == "JO" & (year > 2006 & year <= 2014), food := 1]
# lit2[grid == "LL" & (year >= 2005 & year <= 2011), food := 1]

#make factors
dat2[, food := factor(food)][, treatment := factor(treatment)]



# current summaries -------------------------------------------------------

#test bucket issue again
bucketissues <- dat2[food == 0 & bucket == 1, .N, by = .(grid, year)] #WHY?
setorder(bucketissues, grid, year)

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


