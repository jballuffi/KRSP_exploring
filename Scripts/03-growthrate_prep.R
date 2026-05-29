
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



# prep litter data -------------------------------------------------------

#take only cols of interest
lit <- lit[, .(mother_id = squirrel_id, litter_id = id,
                 year = yr, grid, br, food, ln,
               bdate = fieldBDate, date1, date2 = tagDt)]

#create dates
lit[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)]

#the col ln is litter number. This is an annual number.
#ln == 2 is the 2nd litter of a female that year, not over her life

#assume NA is a control
lit[is.na(food), food := 0]

#make food factor
lit[, food := factor(food)]



# prep juvenile data -----------------------------------------------------------

#take only cols of interest
juv <- juv[, .(squirrel_id, litter_id, sex = factor(sex), weight1 = weight, weight2 = tagWT)]

#any weights as zero make NA 
juv[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]



# merge juv2 and lit2 and clean based on weight data -----------------------------------------------------

#merge juv and litter data by the litter ID
dat <- merge(lit, juv, by = "litter_id", all.y = TRUE)

#calculate litter size
dat[, littersize := .N, litter_id]

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



# merge with bucket info --------------------------------------------------

#remove missing squirrel IDs
buckets <- buckets[!is.na(squirrel_id_spring)]

#create a col called bucket and remove 
buckets[, bucket := 1][, bucket_reflo := NULL]

#rename id col
setnames(buckets, "squirrel_id_spring", "squirrel_id")

#merge with census data
data2 <- merge(data, buckets, by = c("grid", "year", "squirrel_id"), all.x = TRUE)

#any bucket NAs become zero
data2[is.na(bucket), bucket := 0]

#make bucket factor
data2[, bucket := as.factor(bucket)]



# calculate growth rates --------------------------------------------------

#get time difference
dat2[, days := as.integer(date2 - date1)]

#any cases where the two dates were the same make days NA
dat2[days == 0, days := NA]

#get growth rate over that time (g/day)
dat2[, growthrate := (weight2 - weight1)/days]

#quick look at distribution
hist(dat2$growthrate)

#check my work, this should be 0 due to prior cleaning of weights
dat2[growthrate <= 0 | growthrate > 10, .N]

#remove NA growth rates
dat2 <- dat2[!is.na(growthrate)]

#test if there are dup ids in growth rate data
dat2[duplicated(squirrel_id) == TRUE]

#make factors
dat2[, br := as.factor(br)]



# Create litter factor -----------------------------------------------------

#make a new col for litter as a factor
dat2[, litterf := littersize]

#first I will set anything 6 and above as being one category
dat2[litterf >= 6, litterf := 6]

#make litter size a character
dat2[, litterf := as.character(litterf)]

#change litters of 6 to 6+
dat2[litterf == "6", litterf := "6+"]

#next I will make levels with 
dat2[, litterf := as.factor(litterf)]



# Get basic summaries -----------------------------------------------------

#How many growth rates?
dat2[, .N]

#Get sample size by treatment
dat2[, .N, food]



# figures -----------------------------------------------------------------

basic <- dat2[food == 1 | food == 0]

#look at a t-test of food add and growth rate
(foodadd <- ggplot(basic)+
  geom_boxplot(aes(x = food, y = growthrate))+
  labs(x = "Food add", y = "Growth rate (g/day)")+
  theme_bw())

#get birth month based on bdate
basic[, bmonth := as.factor(month(bdate))]

#if bmonth is NA, use month of date1
basic[is.na(bmonth), bmonth := as.factor(month(date1))]

#look at growth rate in response to birth month and food add
(birthmonth <- ggplot(basic)+
  geom_boxplot(aes(x = bmonth, y = growthrate, color = food))+
  labs(x = "Birth month", y = "Growth rate (g/day)")+
  theme_bw())

#get average growth rates by year
annual <- basic[, .(growthrate = mean(growthrate, na.rm = TRUE)), by = .(year, food)]
setorder(annual, year)

#plot avg annual growth rate over time and by food add
(annualplot <- ggplot(annual)+
  geom_line(aes(x = year, y = growthrate, color = food), linewidth = 1)+
  labs(x = "Year", y = "Growth rate (g/day)")+
  theme_bw())

#growth rate in response to litter size
(ls <- ggplot(basic)+
    geom_boxplot(aes(x = litterf, y = growthrate))+
    labs(x = "Litter size", y = "Growth rate (g/day)", subtitle = "Controls only")+
    theme_minimal())



# save ---------------------------------------------------

saveRDS(dat2, "Output/Data/growth_rates.rds")

ggsave("Output/Figures/GR_foodadd.jpeg", foodadd, width = 3, height = 4, unit = "in")
ggsave("Output/Figures/GR_birthmonth.jpeg", birthmonth, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_annual.jpeg", annualplot, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_littersize.jpeg", ls, width = 6, height = 4, unit = "in")

