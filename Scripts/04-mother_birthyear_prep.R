
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

#get individual juvenile data 
juv <- as.data.table(tbl(con, "juvenile") %>% collect())

#get litter data
lit <- as.data.table(tbl(con, "litter") %>% collect())

#for total life span either take this data or take trapping data
trapping <- as.data.table(tbl(con, "trapping") %>% collect())



# subset and collect data --------------------------------------------------------------

#subset juvenile data to just mom id and litter id
juv2 <- juv[sex == "F", .(mother_id = squirrel_id, litter_id)]

#take just litter id and birth year
lit2 <- lit[, .(litter_id = id, mother_byear = yr)]

#take just mother id and trap dates
trap2 <- trapping[sex == "F", .(mother_id = squirrel_id, trapdate = ymd(date))]

#collect first year trapped for each mom in case they don't have a birth year 
momtrap <- trap2[, .(mother_firsttrap = year(min(trapdate, na.rm = TRUE))), mother_id]



# merge info  ----------------------------------

#merge with juv data to get litter id
mombirths <- merge(juv2, lit2, by = "litter_id", all.x = TRUE)

#remove litter id
mombirths[, litter_id := NULL]

#merge birth year and trap year
momlives <- merge(mombirths, momtrap, by = "mother_id", all = TRUE)

#if birth year is missing, make it the year before first trap year
momlives[is.na(mother_byear), mother_byear := mother_firsttrap - 1]



# save --------------------------------------------------------------------

saveRDS(momlives, "Output/Data/mom_lives.rds")



