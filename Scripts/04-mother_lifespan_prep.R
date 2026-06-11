
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

#get litter data
squirrel <- as.data.table(tbl(con, "squirrel") %>% collect())
#might want to try trapping datasheet to get total lifespan if birthday unknown

#get pedigree data
pedigree <- as.data.table(tbl(con, "pedigree_clean") %>% collect())

#make date format
squirrel[, trap_date := ymd(trap_date)]

#subset squirrel
moms <- squirrel[sex == "F", .(mother_id = id, grid = gr, mother_last = year(trap_date))]

#subset pedigree data
mombirths <- pedigree[sex == "F", .(mother_id = squirrel_id, mother_byear = byear)]



# merge info about moms  ----------------------------------

#this will have birth year and last trapped
momlives <- merge(moms, mombirths, by = "mother_id", all.x = TRUE)

#calc lifespan based on last year trapped
momlives[, mother_lifespan := mother_last - mother_byear]

#any lifespan less than 0, make zero. Some sort of error
momlives[mother_lifespan < 0, mother_lifespan := 0]

#look at lifespans
hist(momlives$mother_lifespan)



# save --------------------------------------------------------------------

saveRDS(momlives, "Output/Data/mom_lives.rds")



