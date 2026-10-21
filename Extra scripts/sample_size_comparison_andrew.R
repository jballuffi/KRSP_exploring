
library(data.table)

#read in andrew's data
load("Input/andrews_data.RData")

#rename his litter data
al <- as.data.table(litter)

#take just SU and KL with growth rates
Acontrols <- al[(grid == "SU" | grid == "KL") & !is.na(mean_growth)]

#read in my control dataset
controls <- readRDS("Output/Data/control_cleaned_litters.rds")

#create a comparison of my sample sizes and andrew's sample sizes and avg growth rates by year
Jsamp <- controls[, .(J_N = .N, J_growth = mean(growthrate)), year]
Asamp <- Acontrols[, .(A_N = .N, A_growth = mean(mean_growth)), year]

samp <- merge(Jsamp, Asamp, by = "year", all = TRUE)
samp[, diff := A_N - J_N]



