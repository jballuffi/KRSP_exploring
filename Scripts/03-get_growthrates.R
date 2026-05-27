
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



# later litter data -------------------------------------------------------

#get litter data
lit2 <- as.data.table(tbl(con, "litter") %>% collect())

#take only cols of interest
lit2 <- lit2[, .(mother_id = squirrel_id, litter_id = id, year = yr, grid, br, food, ln,
               bdate = fieldBDate, date1, date2 = tagDt)]

#fix up dates
lit2[, bdate := ymd(bdate)][, date1 := ymd(date1)][, date2 := ymd(date2)]



# later juvenile data -----------------------------------------------------------

#get juvenile data 
juv2 <- as.data.table(tbl(con, "juvenile") %>% collect())
#this data is only from 2013 - 2025

#take only cols of interest
juv2 <- juv2[, .(squirrel_id, litter_id, sex, weight1 = weight, weight2 = tagWT)]

#any weights as zero make NA 
juv2[weight1 == 0, weight1 := NA][weight2 == 0, weight2 := NA]



# merge juv2 and lit2 and clean based on weight data -----------------------------------------------------

#merge
dat2 <- merge(lit2, juv2, by = "litter_id", all.y = TRUE)

#calculate litter size
dat2[, littersize := .N, litter_id]



# calculate growth rates --------------------------------------------------

#cases where weight 1 and weight 2 are the same.
dat2[weight1 == weight2] #this looks to be usually when first nests were missed

#number of cases where weight 1 is bigger than weight 1
dat2[weight1 > weight2, .N] #this is a rare scenario

#get time difference
dat2[, days := as.integer(date2 - date1)]

#any cases where the two dates were the same make days NA
dat2[days == 0, days := NA]

#get growth rate over that time (g/day)
dat2[, growthrate := (weight2 - weight1)/days]

#quick look at distribution
hist(dat2$growthrate)

#anything less than or equal to zero OR greater than 10 make NA
dat2[growthrate <= 0 | growthrate > 10, growthrate := NA]

#make sex factor
dat2[, sex := as.factor(sex)]

#look at example analyses 
#exclude certain litters for growth rate calculations (Dulaney paper and David Fisher paper exclude certain litters)
#excl
#first mass over 50 g or 2nd mass was over 100



# figures -----------------------------------------------------------------

basic <- dat2[food == 1 | food == 0 | is.na(food)]

#assume NA is a control
basic[is.na(food), food := 0]

#make food factor
basic[, food := as.factor(food)]

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
(annual <- ggplot(annual)+
  geom_line(aes(x = year, y = growthrate, color = food), linewidth = 1)+
  labs(x = "Year", y = "Growth rate (g/day)")+
  theme_bw())



# save ---------------------------------------------------

saveRDS(dat2, "Output/Data/growth_rates.rds")
ggsave("Output/Figures/GR_foodadd.jpeg", foodadd, width = 3, height = 4, unit = "in")
ggsave("Output/Figures/GR_birthmonth.jpeg", birthmonth, width = 6, height = 4, unit = "in")
ggsave("Output/Figures/GR_annual.jpeg", annual, width = 6, height = 4, unit = "in")


