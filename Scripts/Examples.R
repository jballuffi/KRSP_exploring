library(krsp)
library(ggplot2)
library(data.table)


con <- krsp_connect(
  host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
  dbname = "krsp",
  username = Sys.getenv("krsp_user"),
  password = Sys.getenv("krsp_password")
)


# ---------------------------
#   Load and clean data
# ---------------------------

# Define mast years
mast_years <- c(1993, 1998, 2005, 2010, 2014, 2019, 2022)

# Cone data
cone_count <- tbl(con, "cones") %>%
  collect() %>%
  # change argument from character to numeric
  mutate(NumNew = as.numeric(NumNew))


# Summarize cone production by year
cones_year <- cone_count %>%
  group_by(Year) %>%
  summarise(NumNew = sum(NumNew, na.rm = TRUE)) %>%
  mutate(mast = as.numeric(Year) %in% mast_years)

View(cones_year)

# Plot cone yearly production over time
ggplot(cones_year, aes(x = as.numeric(Year), y = NumNew)) +
  geom_line(linewidth = 1) +
  geom_point(aes(color = mast), size = 3) +
  scale_color_manual(values = c("black", "red")) +
  labs(
    title = "Cone Production Over Time (Mast Years Highlighted)",
    x = "Year",
    y = "Total New Cones",
    color = "Mast Year"
  ) +
  theme_minimal()


# Litter data
litter <- tbl(con, "litter") %>%
  collect() %>%
  # Rename column yr to match with cones tables 
  dplyr::rename(Year = yr)

# Average partuition date each year
litter_year <- litter %>%
  group_by(Year) %>%
  summarise(fieldBDate = mean(as.Date(fieldBDate), na.rm = TRUE)) %>%
  # Convert to Julian date (day-of-year) and add mast variable 
  mutate(JulianDay = as.numeric(format(fieldBDate, "%j")), 
         mast = as.numeric(Year) %in% mast_years)

ggplot(litter_year, aes(x = as.numeric(Year), y = JulianDay)) +
  geom_line() +
  geom_point(aes(color = mast), size = 3) +
  scale_color_manual(values = c("black", "red")) +
  labs(
    title = "Parturition Timing (Mast Years Highlighted)",
    x = "Year",
    y = "Day of Year"
  ) +
  theme_minimal()


# ---------------------------
# Merge datasets for analysis
# ---------------------------

# Ensure that the Year column is numeric in both datasets
# This is important so that the join works correctly
cones_year <- cones_year %>%
  mutate(Year = as.numeric(Year))

litter_year <- litter_year %>%
  mutate(Year = as.numeric(Year))

# Join the cone production and litter datasets by Year
# inner_join keeps only years that are present in both datasets
df <- cones_year %>%
  inner_join(litter_year, by = "Year") %>%
  # After joining, both datasets have a "mast" column (mast.x and mast.y)
  # Rename mast.x to mast for simplicity
  rename(mast = mast.x) %>%
  # Remove the duplicate mast column from the second dataset
  select(-mast.y)

# Scatterplot: cone production vs parturition timing (show correlation between variables)
ggplot(df, aes(x = NumNew, y = JulianDay)) +
  geom_point(aes(color = mast), size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("black", "red")) +
  labs(
    title = "Relationship Between Cone Production and Parturition Timing",
    x = "Total New Cones",
    y = "Parturition Timing (Julian Day)",
    color = "Mast Year"
  ) +
  theme_minimal()



