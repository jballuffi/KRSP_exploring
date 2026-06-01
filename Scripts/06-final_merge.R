
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

#remove duplicated rows of data
#WHY ARE THESE HERE
moms <- moms[duplicated(moms) == FALSE]

#merge data by mother id, year and grid
dat <- merge(moms, growth, by = c("mother_id", "year", "grid"), all.y = TRUE)

#check for duplication after merge
dat[duplicated(squirrel_id) == TRUE]



# subset data and write out treatments ------------------------------------------------

#list the grids we are interested in
grids <- c("KL", "SU", "AG", "JO", "LL", "BT")

#list the treatments we are interested in 
foods <- c(1, 0, 18, 19)

#subset to that list of grids and treatments, excluding data where density is missing
dat2 <- dat[grid %in% grids & food %in% foods & !is.na(density)]

#redo factor of food
dat2[, food := factor(food)]



# create summary ----------------------------------------------------------

#summary by treatment
dat2[, .N, by = .(treatment)]

#create a table showing sample sizes by categories for non-control grids
summary <- dat2[!(grid == "SU" | grid == "KL"), .N, by = .(grid, year, treatment)]
setorder(summary, grid, year)



# save --------------------------------------------------------------------

#save final data
saveRDS(dat2, "Output/Data/final_data.rds")

#save and send to ben
write.csv(summary, "Output/Data/N_summary_treatments.csv")


