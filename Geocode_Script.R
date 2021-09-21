# Download file and add lat long manually

library(tidyverse)
library(vroom)
school_covid <- vroom ("https://data.ontario.ca/dataset/b1fef838-8784-4338-8ef9-ae7cfd405b41/resource/dc5c8788-792f-4f91-a400-036cdf28cfe8/download/schoolrecentcovid2021_2022.csv",
                          col_types = cols(.default = "c")) %>% 
# locale = readr::locale(encoding = "latin1")) %>% 
  drop_na(reported_date) %>% 
  mutate (reported_date = as.Date (reported_date)) %>% 
  arrange (reported_date)

# max (school_covid$reported_date)

current_date <- max (school_covid$reported_date)

if (weekdays(Sys.Date()) == "Monday"){
  previous_date <- as.Date (current_date) - 3
} else {
  previous_date <- as.Date (current_date) - 1
  }

# Google API --------------------------------------------------------------

API <- "AIzaSyB-B0urQUZELd27NgEq1zAfiuzVBokIUMg"

library("ggmap")
register_google(key = API)

# Separate Current Data ---------------------------------------------------

current_data <- school_covid %>%
  dplyr::filter (reported_date > "2021-01-01") %>%
  mutate (school = str_replace(school, 
                               "\u0092",
                               "'")) %>% 
  mutate (school = str_replace(school, 
                               "Sacré-C\u009cur", 
                               "Sacré-Coeur")) 

# ----
current_data <- school_covid %>%
  dplyr::filter (reported_date == current_date) %>%
  mutate (school = str_replace(school, 
                               "\u0092",
                               "'")) %>% 
  mutate (school = str_replace(school, 
                               "Sacré-C\u009cur", 
                               "Sacré-Coeur")) 

# Separate out previous geocodes and new schools --------------------------

`%notin%` <- Negate (`%in%`)

master_geocode <- vroom ("Data/school_geocode_master.csv")  %>% 
  mutate (unique_id = paste0 (school, municipality)) %>% 
  mutate (DUP = duplicated(unique_id)) %>% 
  dplyr::filter (DUP != "TRUE") %>% 
  select (-DUP)

new_geocodes <- current_data %>% 
  select (school, municipality) %>% 
  mutate (unique_id = paste0 (school, municipality)) %>% 
  dplyr::filter (unique_id %notin% master_geocode$unique_id)

new_geocodes <- new_geocodes %>% 
  cbind (geocode (location = paste0(new_geocodes$school, ", ", new_geocodes$municipality, ", Ontario"),
                  source = "google"))


table (is.na(new_geocodes$lon))
# new_geocodes$lon[1] <- -80.9947477
# new_geocodes$lat[1] <- 46.4991633

test <- master_geocode %>%
  rbind (new_geocodes)

write_csv (test,
           file = "Data/school_geocode_master.csv")

current_data <- current_data %>%
  left_join (test, by = c ("school", "municipality"))


table (is.na (current_data$lon))

write_csv (current_data %>% select (-unique_id),
           file = "school_covid.csv"
)

# Master school geocode list ----------------------------------------------

# master_school_geocode <- school_covid %>% 
#   distinct (school, municipality) %>% 
#   mutate (school = str_replace_all(school, "\u0092", "'")) %>% 
#   mutate (school = str_replace(school, 
#                                "Sacré-C\u009cur", 
#                                "Sacré-Coeur")) 
# 
# master_school_geocode <- cbind (geocode(location = paste0(master_school_geocode$school, ", ", master_school_geocode$municipality, ", Ontario"),
#                                         source = "google")) 
# 
# BRRR::skrrrahh(24)
# 
# test <- school_covid %>% 
#   distinct (school, municipality) %>% 
#   mutate (school = str_replace_all(school, "\u0092", "'")) %>% 
#   mutate (school = str_replace(school, 
#                                "Sacré-C\u009cur", 
#                                "Sacré-Coeur")) %>% 
#   cbind (master_school_geocode)
# 
# table (is.na (test$lon))  
#   
# write_csv (test, 
#            path = "Data/school_geocode_master.csv")

# Maple Ridge	Maple Ridge P.S.	DDSB	43.84773576	-79.09849971
# Pine Ridge S.S.	DDSB	43.85187978	-79.0965624
# Walpole 42.9230006 -80.1089396


