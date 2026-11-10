
#source the R folder to load any packages and functions
lapply(dir('R', '*.R', full.names = TRUE), source)

mcadam <- fread("Litter_data_mcadam2019.csv")
controls <- readRDS("Output/Data/control_cleaned_litters.rds")




# fix my data to try and match --------------------------------------------

controls <- controls[year < 2018 & year > 1988]

controls[, littersize_std_2 := littersize_std^2, gridyear]

controls[, .N]
controls[, length(unique(mother_id))]
controls[, length(unique(year))]



# run model on mcadam data ------------------------------------------------

mcadam[, std_LS2 := scale(ls^2), by = .(grid, year)]

ggplot(mcadam)+
  geom_boxplot(aes(x = mast, y = litter_fit))

mod <- glmer(litter_fit ~ grid + mast + std_Julian + std_Growt + std_LS + std_LS2 + std_LS*mast + (1|squirrel_id) + (1|year),
             data = mcadam[!is.na(mean_growth)],
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)


# run model on my data ----------------------------------------------------

ggplot(controls)+
  geom_boxplot(aes(x = mast, y = ars))


mod <- glmer(ars ~ grid + mast + bjulian_std + growthrate_std + littersize_std + littersize_std_2 + littersize_std*mast + (1|mother_id) + (1|year),
             data = controls,
             family = poisson(link = "log"),
             control = glmerControl(optimizer = "bobyqa"))

summary(mod)


# #connemod# #connect with database
# con <- krsp_connect(
#   host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
#   dbname = "krsp",
#   username = Sys.getenv("krsp_user"),
#   password = Sys.getenv("krsp_password")
# )
# 
# 
# #get individual juvenile data 
# juv <- as.data.table(tbl(con, "juvenile") %>% collect())
# juv <- juv[, .(squirrel_id, litter_id)]
# 
# #get litter data
# lit <- as.data.table(tbl(con, "litter") %>% collect())
# lit <- lit[, .(mother_id = squirrel_id, litter_id = id, year = yr, grid, food, ln, bdate = ymd(fieldBDate))]
# 
# #lifetime data
# lifetime <- as.data.table(tbl(con, "flastall2") %>% collect())
# lifetime <- lifetime[, .(squirrel_id, sdate = ymd(dates), f1, fdate = ymd(datee), f2)]
# 
# 
# 
# # merge everything together -----------------------------------------------
# 
# dat <- merge(lit, juv, by = "litter_id", all.y = TRUE)
# dat <- merge(dat, lifetime, by = "squirrel_id", all.x = TRUE)
# 
# #clean out unwanted data
# dat <- dat[(grid == "SU" | grid == "KL") & ln == 1 & food == 0]
# 
# 
# 
# # calculate ARS -----------------------------------------------------------
# 
# #calculate known lifespan
# dat[, lifespan := fdate - bdate]
# 
# #who lived to be over 200 days
# dat[lifespan >= 200, survival := "yes"]
# dat[lifespan < 200, survival := "no"]
# 
# #tally up number of surviving juveniles by mother and year
# survive <- dat[survival == "yes", .(ars = .N), by = .(mother_id, year, litter_id)]
# nosurvive <- dat[survival == "no", .(died = .N), by = .(mother_id, year, litter_id)]
# 
# ars <- merge(survive, nosurvive, by = c("mother_id", "year", "litter_id"), all = TRUE)
# ars[is.na(ars), ars := 0][, died := NULL]
# 
# 
# #save
# saveRDS(ars, "Output/Data/ars.rds")
# 
#     