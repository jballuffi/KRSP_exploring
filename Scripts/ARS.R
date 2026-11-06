
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

#take only cols of interest
juv <- juv[, .(squirrel_id, litter_id)]


#get litter data
lit <- as.data.table(tbl(con, "litter") %>% collect())

#take only cols of interest
lit <- lit[, .(mother_id = squirrel_id, 
                litter_id = id,
                year = yr, grid, food, ln,
                bdate = fieldBDate)]

#lifetime data
lifetime <- as.data.table(tbl(con, "flastall2") %>% collect())


