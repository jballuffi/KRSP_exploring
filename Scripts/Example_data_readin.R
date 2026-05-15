

#This is an example of how to get data from the SQL database

#load packages
library(krsp)
library(ggplot2)
library(data.table)


#connect with database
con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)

# Define mast years
mast_years <- c(1993, 1998, 2005, 2010, 2014, 2019, 2022)


# examples from guidebook -------------------------------------------------

# Cone data
cone_count <- tbl(con, "cones") %>%
  collect() %>%
  # change argument from character to numeric
  mutate(NumNew = as.numeric(NumNew))

# Litter data
litter <- tbl(con, "litter") %>%
  collect() %>%
  # Rename column yr to match with cones tables 
  dplyr::rename(Year = yr)




# my version to get them into data.tables immediately ---------------------


cone_count <- as.data.table(tbl(con, "cones") %>% collect())

litter <- as.data.table(tbl(con, "litter") %>% collect() %>% dplyr::rename(Year = yr))



