
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

#get PB data
pb <- as.data.table(tbl(con, "pb_data") %>% collect())

#get bucket data
#buckets <- fread("Input/fed_squirrels.csv")

#get final merged data
dat <- readRDS("Output/Data/final_data.rds")



# clean up data -----------------------------------------------------------

#subset
pb <- pb[, .(grid = Grid, reflo = Midden, date = ymd(Date), pb = PB_Added, pb_rem = PB_Remaining)]
#buckets <- buckets[, .(grid, year, reflo = bucket_reflo, mother_id = squirrel_id_spring)]

#make year in pb
pb[, year := year(date)]

#test bucket issue again, where they have bucket but listed as control
bucketissues <- dat[food == 0 & bucket == 1, .(grid, year, mother_id, reflo)] #WHY?
setorder(bucketissues, grid, year)
#bucketissues[, problematic := "yes"]

#remove duplicates (probably because it by juvenile not litter)
bucketissues <- unique(bucketissues)


#merge bucket issue with pb
pb2 <- merge(pb, bucketissues, by = c("grid", "year", "reflo"), all.y = TRUE)
setorder(pb2, grid, year, reflo)

nopb <- pb2[pb_rem == 0 & pb == 0]
setorder(nopb, grid, year, reflo)
nopb[, length(unique(mother_id)), by = .(grid, year)]


write.csv(bucketissues, "Output/Data/food_and_bucket_issues.csv")


