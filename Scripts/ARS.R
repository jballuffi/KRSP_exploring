
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)


#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)


#get individual juvenile data 
juv <- as.data.table(tbl(con, "juvenile") %>% collect())
juv <- juv[, .(squirrel_id, litter_id)]

#get litter data
lit <- as.data.table(tbl(con, "litter") %>% collect())
lit <- lit[, .(mother_id = squirrel_id, litter_id = id, year = yr, grid, food, ln, bdate = ymd(fieldBDate))]

#lifetime data
lifetime <- as.data.table(tbl(con, "flastall2") %>% collect())
lifetime <- lifetime[, .(squirrel_id, sdate = ymd(dates), f1, fdate = ymd(datee), f2)]



# merge everything together -----------------------------------------------

dat <- merge(lit, juv, by = "litter_id", all.y = TRUE)
dat <- merge(dat, lifetime, by = "squirrel_id", all.x = TRUE)

#clean out unwanted data
dat <- dat[(grid == "SU" | grid == "KL") & ln == 1 & food == 0]



# calculate ARS -----------------------------------------------------------

#calculate known lifespan
dat[, lifespan := fdate - bdate]

#who lived to be over 200 days
dat[lifespan >= 200, survival := "yes"]
dat[lifespan < 200, survival := "no"]

#tally up number of surviving juveniles by mother and year
survive <- dat[survival == "yes", .(ars = .N), by = .(mother_id, year, litter_id)]
nosurvive <- dat[survival == "no", .(died = .N), by = .(mother_id, year, litter_id)]

ars <- merge(survive, nosurvive, by = c("mother_id", "year", "litter_id"), all = TRUE)
ars[is.na(ars), ars := 0][, died := NULL]


#save
saveRDS(ars, "Output/Data/ars.rds")

    