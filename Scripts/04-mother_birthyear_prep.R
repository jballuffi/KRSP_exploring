
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)



# get data ----------------------------------------------------------------

#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

#for total life span either take this data or take trapping data
#squirrel <- as.data.table(tbl(con, "squirrel") %>% collect())

#get individual juvenile data 
juv <- as.data.table(tbl(con, "juvenile") %>% collect())

#get litter data
lit <- as.data.table(tbl(con, "litter") %>% collect())



# subset data --------------------------------------------------------------

#subset juvenile data to just mom id and litter id
juv2 <- juv[sex == "F", .(mother_id = squirrel_id, litter_id)]

#take only cols of interest
lit2 <- lit[, .(litter_id = id, mother_byear = yr)]

# #make date format in squirrel
# squirrel[, trap_date := ymd(trap_date)]
# 
# #subset squirrel data to just females and 
# moms <- squirrel[sex == "F", .(mother_id = id, mother_last = year(trap_date))]



# merge info to get mom birth years  ----------------------------------

#merge with juv data to get litter id
momlives <- merge(juv2, lit2, by = "litter_id", all.x = TRUE)

# #merge with litter data to get birth year
# momlives <- merge(momlives, lit2, by = "litter_id", all.x = TRUE)
# 
# #calc lifespan based on last year trapped
# momlives[, mother_lifespan := mother_last - mother_byear]


#look at NA for birth year
momlives[is.na(mother_byear), .N]



# save --------------------------------------------------------------------

saveRDS(momlives, "Output/Data/mom_lives.rds")



