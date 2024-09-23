#https://la.myneighborhooddata.org/2021/06/poverty/

library(tidyverse)
library(janitor)
library(lubridate)

library(readxl)
sqf_2023 <- read_excel("data/sqf-2023.xlsx",
                       na = "(null)")
glimpse(sqf_2023)

sqf_2023 <- sqf_2023 |> 
  clean_names() |> 
  relocate(1:6, stop_duration_minutes,
         suspect_arrested_flag,
         summons_issued_flag,
         suspect_reported_age,
         suspect_sex,
         suspect_race_description,
         suspect_height,
         suspect_weight,
         stop_location_boro_name) |> 
  mutate(suspect_height = as.numeric(suspect_height),
         stop_frisk_date = ymd(stop_frisk_date),
         month2 = factor(month2, 
                         levels = month.name,  # use the built-in month names for correct order
                         ordered = TRUE),
         day2 = factor(day2, 
                         levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"),
                         ordered = TRUE),
         suspect_arrested_flag = as.factor(suspect_arrested_flag),
         summons_issued_flag = as.factor(summons_issued_flag),
         suspect_sex = as.factor(suspect_sex),
         suspect_race_description = as.factor(suspect_race_description),
         stop_location_boro_name = as.factor(stop_location_boro_name))

sqf_2023 |> 
  count(suspect_race_description) 
  

saveRDS(sqf_2023, "./LAB_02/sqf_2023.rds")

###############

grocery_1519 <- read_csv("data/All Years Grocery Store Access Calculations.csv") %>% 
  clean_names()
frpl_1523 <- read_csv("data/All Years FRPL Calculations.csv") %>% 
  clean_names()
poverty_1022 <- read_csv("data/All Years Poverty Calculations.csv") %>% 
  clean_names()
total_pop_1022 <- read_csv("data/All Years Total Population Calculations.csv") %>% 
  clean_names()

count(grocery_1519, year) #2015 & 2019 only
count(frpl_1523, year) #2015 - 2023 all
count(poverty_1022, year) #2010 - 2022 all


length(unique(grocery_1519$geoid))
length(unique(frpl_1523$geoid))

food_desert_19 <- grocery_1519 %>% 
  filter(year == 2019) %>% 
  mutate(pct_lowaccess = lowaccess_count/denom_total_pop,
         # food_desert = case_when(pct_lowaccess >= 1/3 ~ "yes",
         #                          #lowaccess_count >= 500 ~ "yes",
         #                          TRUE ~ "no"),
         food_desert = case_when(pct_lowaccess >= 1/3 ~ "yes",
                                 lowaccess_count >= 500 ~ "yes",
                                 pct_lowaccess < 1/3 ~ "no")) %>% 
  select(geoid, geoid20, food_desert)

data <- frpl_1523 %>% 
  left_join(food_desert_19) %>% 
  left_join(poverty_1022) %>% 
  left_join(total_pop_1022) %>% 
  filter(year > 2014, year < 2023)

library(sf)
# Load the shapefiles
census_tracts <- st_read("data/tl_2019_06_tract/tl_2019_06_tract.shp")
county_subdivisions <- st_read("data/tl_2019_06_cousub/tl_2019_06_cousub.shp")
cong_dist <- st_read("data/tl_2023_06_cd118/tl_2023_06_cd118.shp")

# Perform spatial join
linked_data <- st_join(census_tracts, cong_dist,
                       join = st_intersects)

# Extract relevant columns (e.g., geoid and county subdivision)
result <- linked_data %>%
  filter(COUNTYFP == "037") %>%
  select(census_tract_geoid = GEOID.x, cong_dist = NAMELSAD.y) %>%
  st_drop_geometry() %>%
  mutate(census_tract_geoid = as.numeric(census_tract_geoid)) %>%
  distinct()


cong_dist <- data %>%
  left_join(result, by = c("geoid20" = "census_tract_geoid")) %>%
  distinct(geoid20, year, .keep_all = TRUE) %>%
  group_by(cong_dist, year) %>%
  mutate(n_tracts = n()) %>%
  filter(n_tracts >= 10) %>%
  ungroup() %>%
  select(-n_tracts)

write_csv(data, "./data/la_county_food_insecurity.csv")
write_csv(data, "./LAB_02/la_county_food_insecurity.csv")

data_22 <- data %>% 
  filter(year == 2022)

summary(data_22$frpl_pct)
summary(data_22$reducedmeals_count)
summary(data_22$freemeals_count)

sum(data_22$denom_known_poverty)

data %>% 
  group_by(year) %>% 
  summarize(n_total_pop = sum(total_pop, na.rm = TRUE),
            n_denom_known = sum(denom_known_poverty, na.rm = TRUE),
            n_below_poverty = sum(pop_below_100_count, na.rm = TRUE),
            med_pop_below_100_pct = median(pop_below_100_pct, na.rm = TRUE))

cong_dist %>% 
  filter(year == 2019) %>% 
  group_by(cong_dist) %>% 
  summarize(n = n(),
            min = min(frpl_pct, na.rm = TRUE),
            Q1 = quantile(frpl_pct, .25, na.rm = TRUE),
            Q2 = quantile(frpl_pct, .5, na.rm = TRUE),
            median = median(frpl_pct, .5, na.rm = TRUE),
            Q3 = quantile(frpl_pct, .75, na.rm = TRUE),
            max = max(frpl_pct, na.rm = TRUE)
  ) %>% 
  arrange(min)


library(openintro)
summary(pm25_2022_durham)

library(readxl)
sqf_2023 <- read_excel("data/sqf-2023.xlsx")
glimpse(sqf_2023)

sqf_2023 |> 
  count(SUSPECT_RACE_DESCRIPTION, WEAPON_FOUND_FLAG) |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  mutate(n/sum(n)) |> 
  filter(WEAPON_FOUND_FLAG == "Y")

sqf_2023 <- sqf_2023 |> 
  mutate(crime = if_else(SUSPECT_ARRESTED_FLAG == "Y" |
                           SUMMONS_ISSUED_FLAG == "Y", "Y", "N")) 
sqf_2023|> 
  count(SUSPECT_RACE_DESCRIPTION, crime) |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  mutate(n/sum(n)) |> 
  filter(crime == "N")

sqf_2023 |> count(STOP_LOCATION_BORO_NAME, crime) |> 
  group_by(STOP_LOCATION_BORO_NAME) |> 
  mutate(prop = n/sum(n))

sqf_2023 |> 
  count(FRISKED_FLAG)

sqf_2023 |> 
  count(SEARCHED_FLAG)

race <- read_csv("data/All Years Race and Ethnicity Calculations.csv") |> 
  clean_names()

race22 <- race |> 
  filter(year == 2022) |> 
  mutate(poc_pct = 100 - white_pct,
         majority = if_else(poc_pct > 50, "majority POC", "majority white"),
         poc_q = case_when(poc_pct < 54.5 ~ "Q1",
                           poc_pct < 82.6 ~ "Q2",
                           poc_pct < 94.7 ~ "Q3",
                           poc_pct >= 94.7 ~ "Q4"))

race22 |> 
  count(poc_q)

quantile(race22$poc_pct, na.rm = TRUE)

la_county_food_insecurity <- read_csv("data/la_county_food_insecurity.csv")
la_data22 <- la_county_food_insecurity |> 
  filter(year == 2022) |> 
  left_join(race22)

count(la_data22, poc_q, food_desert) |> 
  group_by(poc_q) |> 
  mutate(prop = n/sum(n)) |> 
  filter(food_desert == "yes")

la_data22 |> 
  group_by(poc_q) |> 
  summarize(mean(frpl_pct))


#compute majority race

la_data22 <- la_data22 |> 
  rowwise() |> 
  mutate(
    largest_race = case_when(
      black_pct == max(c(black_pct, white_pct, asian_pct, latino_pct)) ~ "Black",
      white_pct == max(c(black_pct, white_pct, asian_pct, latino_pct)) ~ "White",
      asian_pct == max(c(black_pct, white_pct, asian_pct, latino_pct)) ~ "Asian",
      latino_pct == max(c(black_pct, white_pct, asian_pct, latino_pct)) ~ "Latino"
    )
  ) %>%
  ungroup()

la_data22 |> 
  count(largest_race, food_desert) |> 
  group_by(food_desert) |> 
  mutate(prop = n/sum(n))

library(ggridges)
ggplot(la_data22, aes(x = black_pct, y = food_desert)) +
  geom_density_ridges()

la_data22 |> 
  group_by(food_desert) |> 
  summarize(mean(asian_pct, na.rm = TRUE))

la_data22 |> 
  group_by(largest_race) |> 
  summarize(food_ins = mean(frpl_pct))
