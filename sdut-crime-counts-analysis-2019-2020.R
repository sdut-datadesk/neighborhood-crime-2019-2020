# Libraries
library(readxl)
library(dplyr)
library(readr)
library(janitor)
library(base)
library(stringr)
library(tidyverse)
library(lubridate)
library(data.table)

#Set working directory
setwd("~/Desktop")

#################################################################################################
######################################## IMPORT #################################################
#################################################################################################

# Import 2019 original data
## Row count == 236,133
library(readr)
library(readr)
c2019 <- read_csv("SANDAG-crime-2019.csv", 
                  col_types = cols(...1 = col_skip(), ActPK = col_character(), 
                                   zipCode = col_character()))

# Import 2020 original data
## Received warnings when loading .xlsx version, using .csv created with original
### Row count == 231,852
library(readr)
c2020 <- read_csv("Final_PRA_Request_2020_Crime_data.csv", 
                  col_types = cols(ActPK = col_character(), 
                                   zipCode = col_character()))

# Give each row a unique ID
c2020 <- mutate(c2020, id = paste0("UT20-", rownames(c2020)))

# Rearrange
c2020 <- c2020 %>%
  select(id, everything())

# We have a mismatched number of columns
names(c2019)
names(c2020)

# Remove "activityNumber" from c2020
c2020 <- c2020 %>% select(-activityNumber)

# Merge all datasets together
## Row count == 467,985
## Column count == 15
master <- rbind(c2019, c2020)

# Remove originals
remove(c2019, c2020)

#################################################################################################
######################################## FILTER #################################################
#################################################################################################

# Clean column names
library(janitor)
master <- master %>% 
  clean_names()

# Make column names uppercase
library(base)
names(master) <- toupper(names(master))

# Remove leading and trailing whitespace from entire master
library(stringr)
master <- master %>% 
  mutate_if(is.character, str_trim)

# Remove all extra types of whitespace and replace with one space
master$AGENCY <- gsub("\\s+", " ", master$AGENCY)
master$ACTIVITY_STATUS <- gsub("\\s+", " ", master$ACTIVITY_STATUS)
master$ACTIVITYDATE <- gsub("\\s+", " ", master$ACTIVITYDATE)
master$BLOCK_ADDRESS <- gsub("\\s+", " ", master$BLOCK_ADDRESS)
master$CITY <- gsub("\\s+", " ", master$CITY)
master$ZIP_CODE <- gsub("\\s+", " ", master$ZIP_CODE)
master$CRIME_CATEGORY <- gsub("\\s+", " ", master$CRIME_CATEGORY)
master$CRIME_DESCRIPTION <- gsub("\\s+", " ", master$CRIME_DESCRIPTION)

# Test for abnormalities
test <- table(master$CRIME_CATEGORY) %>% as.data.frame()
test2 <- table(master$CRIME_DESCRIPTION) %>% as.data.frame()
## There are none
remove(test, test2)

# There are $ in CRIME_DESCRIPTION that may cause issues later, remove them
master$CRIME_DESCRIPTION <- gsub("\\$", "", master$CRIME_DESCRIPTION)

# There are + in CRIME_DESCRIPTION that may cause issues later, remove them
## Replace with "MORE THAN " to keep meaning of + sign
master$CRIME_DESCRIPTION <- gsub("\\+", "MORE THAN ", master$CRIME_DESCRIPTION)

# There's a & in CRIME_DESCRIPTION that may cause issues later, remove it
master$CRIME_DESCRIPTION <- gsub("\\&", "AND", master$CRIME_DESCRIPTION)

# There's a / in CRIME_DESCRIPTION that may cause issues later, remove it
master$CRIME_DESCRIPTION <- gsub("\\/", " ", master$CRIME_DESCRIPTION)

# Test for abnormalities in AGENCY
test <- table(master$AGENCY) %>% as.data.frame()
## There's one reported by "DISTRICT ATTORNEY" - Leave for now?
remove(test)

#################################################################################################
################################### TRACT/BLOCK FIX #############################################
#################################################################################################

# Remove ' from TRACT and BLOCK columns
master$CENSUS_TRACT <- gsub("'", "", master$CENSUS_TRACT)
master$CENSUS_BLOCK <- gsub("'", "", master$CENSUS_BLOCK)

# Create second block column and make it a number
master$CENSUS_BLOCK2 <- as.numeric(master$CENSUS_BLOCK)

# Create new column for just the BG (first number of CENSUS BLOCK)
master$BG <- substr(master$CENSUS_BLOCK2, 0, 1)

# Create GEOID column to merge with shapefiles
master$GEOID <- paste0("06073", master$CENSUS_TRACT, master$BG)

# Count the number of characters in GEOID to make sure they're all 12
master$GEOID_COUNT <- nchar(master$GEOID)

# Filter to GEOID_COUNT that's less than 12
test <- master %>% filter(GEOID_COUNT < 12)
## Row count == 1,030
### They all have missing / incomplete location information
### 1,030 / 467,985 == approx .22 percent of total data, OK to remove

# Remove rows (based on ID) that appear in test
## New row count of master == 466,955
master <- master[!master$ID %in% test$ID,]
remove(test)

# Filter to GEOID_COUNT that's more than 12
test <- master %>% filter(GEOID_COUNT > 12)
## There are none
remove(test)

# Remove GEOID_COUNT column
master <- master %>% select(-GEOID_COUNT)

# Create second tract column and make it a number
master$CENSUS_TRACT2 <- as.numeric(master$CENSUS_TRACT)

# Create TRACT_BG column
master$TRACT_BG <- paste0(master$CENSUS_TRACT2, "-", master$BG)

# Check for crimes with missing / incomplete location information
test <- master %>% filter(CENSUS_TRACT == "000000")
# There are 30,600
## 30,600 / 466,955 == ~6.6 percent of all data

# Determine if there are roughly an equal number in each year
test2 <- table(test$REPORTING_YEAR) %>% as.data.frame()

# Var1  Freq
#	2019	16907
#	2020	13693

# Remove rows with missing / incomplete location information
## New row count == 436,355
master <- master %>% filter(CENSUS_TRACT != "000000")

remove(test, test2)

# Remove census_tract2 column
master <- master %>% select(-CENSUS_TRACT2)

#################################################################################################
#################################### DATE/TIME FIX ##############################################
#################################################################################################

# Create second date and time column
master$ACTIVITYDATE2 <- master$ACTIVITYDATE

# Check for spaces between time and PM/AM
## There are none
test <- master %>% filter(str_detect(ACTIVITYDATE2, " PM| AM"))
remove(test)

# Split ACTIVITYDATE2 into MONTH, DATE, YEAR and TIME columns
library(tidyverse)
master <- separate(data = master, col = ACTIVITYDATE2, into  = c('MONTH', 'DATE', 'YEAR', 'TIME'), sep = ' ')

# Make the month column a number instead of name
master$MONTH <- match(master$MONTH,month.abb)

# Paste month, date and year together for final activity date column
master$ACTIVITY_DATE2 <- paste(master$MONTH, master$DATE, master$YEAR, sep="/")

# Convert into date format
master$ACTIVITY_DATE2 <- as.Date(master$ACTIVITY_DATE2, "%m/%d/%Y")

# Create a "weekday" column
library(lubridate)
master <- master %>% 
  mutate(WEEKDAY = lubridate::wday(ACTIVITY_DATE2, label = TRUE)) # column with the day of the week spelled out

# Remove month, date, year and original activity_date
master <- master %>% select(-MONTH, -DATE, -YEAR, -ACTIVITYDATE)

# Rename fixed activity date column
master <- rename(master, ACTIVITYDATE = ACTIVITY_DATE2)

# Create new time column
master$TIME2 <- master$TIME

# Add space between time and AM/PM
master$TIME2 <- gsub("AM", " AM", master$TIME2)
master$TIME2 <- gsub("PM", " PM", master$TIME2)

# Make new time column into 24 hour format
master$TIME2 <- format(strptime(master$TIME2, "%I:%M %p"), "%H:%M:%S")

# Remove TIME original and rename TIME2
master <- master %>% select(-TIME)
master <- rename(master, TIME = TIME2)

library(chron)
# Create categorical breaks
breaks <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
            13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24) / 24 # Times are internally fractions of a day

# Create category labels
labels <- c("Midnight - 1 AM", "1 AM - 2 AM", "2 AM - 3 AM", "3 AM - 4 AM",
            "4 AM - 5 AM", "5 AM - 6 AM", "6 AM - 7 AM", "7 AM - 8 AM",
            "8 AM - 9 AM", "9 AM - 10 AM", "10 AM - 11 AM", "11 AM - Noon",
            "Noon - 1 PM", "1 PM - 2 PM", "2 PM - 3 PM", "3 PM - 4 PM",
            "4 PM - 5 PM", "5 PM - 6 PM", "6 PM - 7 PM", "7 PM - 8 PM",
            "8 PM - 9 PM", "9 PM - 10 PM", "10 PM - 11 PM", "11 PM - Midnight")

# Create new time column to categorize into hours of the day
master$TIME2 <- master$TIME

# Append a chron "times" class column
master$TIME2 <- times(master$TIME2)

# Create new time_category based on breaks, labels and time in time2
master$HOUR_CAT <- cut(master$TIME2, breaks, labels, include.lowest = TRUE)

# Remove breaks and labels
remove(breaks, labels)

# Create new hour column, rounding time to nearest hour
## 3600 seconds in an hour, put 60 seconds to round to nearest minute
library(hms)
master$HOUR <- round_hms(as.hms(master$TIME), 3600)

# Early morning = EM = midnight to 5:59 am
# Morning = M = 6 a.m. to 11:59 am
# Afternoon = A = noon to 5:59 p.m.
# Night = N = 6 p.m. to 11:59 p.m.
# Create categorical breaks for period of day
breaks <- c(0, 6, 12, 18, 24) / 24 # Times are internally fractions of a day

# Create category labels
labels <- c("EM", "M", "A", "N")

# Create new day_category based on breaks, labels and time in time2
master$DAY_CAT <- cut(master$TIME2, breaks, labels, include.lowest = TRUE)

# Remove TIME2
master <- master %>% select(-TIME2)

# Remove breaks and labels
remove(breaks, labels)

# Filter to crimes that actually occurred between 1/1/2019 - 12/31/2020
## New row count == 424,631
master2 <- master[master$ACTIVITYDATE >= "2019-01-01" & master$ACTIVITYDATE <= "2020-12-31",]

# For some reason, two rows with all NAs are added when we filter above
## Remove those two rows
### New row count == 424,629
master2 <- master2 %>% filter(!is.na(ID))

# table the year to see overall year by year
table(master2$REPORTING_YEAR)
# 2019   2020 
# 209181 215448

master2 %>% 
  filter(REPORTING_YEAR == 2019) %>% 
  summarise(TOTAL = sum(NUMBER_ACTUAL_REPORTED))
# == 205,837

master2 %>% 
  filter(REPORTING_YEAR == 2020) %>% 
  summarise(TOTAL = sum(NUMBER_ACTUAL_REPORTED))
# == 210,745

#################################################################################################
################################# IMPORT SHAPES/HOODS ###########################################
#################################################################################################

# Import census shapefiles
## If you get an error, try this:
### options(tigris_use_cache = TRUE) or FALSE
library(tigris)
bg <- block_groups("CA", county = 073, year = 2019, cb = TRUE, refresh = TRUE)

# Find water (San Diego Bay/Egger Highlands) tract
which(grepl(060730099021, bg$GEOID))
bg <- bg[-1659,]

# Import hood names, 2019 population and whether BG falls within City of SD (City2 column)
pop <- read_csv("pop2019.csv", 
                col_types = cols(BLKGRPCE = col_character(), 
                                 TRACTCE = col_character()))

# Make column names uppercase
names(pop) <- toupper(names(pop))

# Rename tract-bg column
pop <- rename(pop, TRACT_BG = 'TRACT-BG')

# Create GEOID column in pop from ID column
pop$GEOID <- substr(pop$ID, 10, 21)

# Merge master2 with pop
master2 <- left_join(master2, pop, by = "GEOID")

# Test for crimes that have a GEOID not inside San Diego County
test <- master2 %>% filter(is.na(HOOD))
## There are none
remove(test)

# Remove duplicated / unnecessary columns and rename
master2 <- master2 %>% select(-TRACT_BG.y, -TRACTCE, -BLKGRPCE)
master2 <- rename(master2, GEOID_LONG = ID.y)
master2 <- rename(master2, UTID = ID.x)
master2 <- rename(master2, TRACT_BG = TRACT_BG.x)
master2 <- rename(master2, CRIME_CITY = CITY.x)
master2 <- rename(master2, CITY1 = CITY.y)

# Create table of different crimes in CRIME_DESCRIPTION
descriptions <- master2 %>%
  group_by(CRIME_CATEGORY, CRIME_DESCRIPTION) %>%
  summarise(COUNT = n())

# Export to csv for meeting with David and Greg
# write.csv(descriptions, "descriptions.csv")

remove(descriptions)

# Filter to crimes reported to SAN DIEGO agency
sd <- master2 %>% 
  filter(AGENCY == "SAN DIEGO")
## Row count == 177,965

sd %>% 
  filter(REPORTING_YEAR == 2019) %>% 
  summarise(TOTAL = sum(NUMBER_ACTUAL_REPORTED))
# == 79495

sd %>% 
  filter(REPORTING_YEAR == 2020) %>% 
  summarise(TOTAL = sum(NUMBER_ACTUAL_REPORTED))
# == 95301

remove(master, master2)

#################################################################################################
################################# SD WEEKDAY ANALYSIS ###########################################
#################################################################################################

# Filter to just the crimes we're looking at for max analyses
sd_crimes <- sd %>% 
  filter(CRIME_CATEGORY %in% c("Aggravated Assault", "Armed Robbery", "Larceny < $400", 
                               "Larceny >= $400", "Murder", "Non Res Burglary", "Rape",
                               "Res Burglary", "Strong ArmRobbery") |
           CRIME_DESCRIPTION %in% c("CHILD AND FAMILY", "FRAUD", "MALICIOUS MISCHIEF"))
## Row count == 69765

# Most common day of week by BG
day19 <- sd_crimes %>% 
  filter(REPORTING_YEAR == 2019) %>% 
  group_by(GEOID, WEEKDAY) %>%
  summarise(COUNT = sum(NUMBER_ACTUAL_REPORTED))

# Spread
day19 <- day19 %>% 
  spread(key = WEEKDAY, value = COUNT, fill = 0)

# Find most common day
library(data.table)
j1 <- max.col(day19[, c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")], "first")
day19$max_day <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[j1]

# Find second most common day in case there's a tie (so we'll at least have two)
j2 <- max.col(day19[, c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")], "last")
day19$max_day2 <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[j2]

# If max_day and max_day2 are the same, keep one
## If they're different, paste together, separated by comma
day19$common_days19 <- ifelse(day19$max_day == day19$max_day2, day19$max_day, paste0(day19$max_day, ", ", day19$max_day2))

# Select only columns we need
day19 <- day19 %>% 
  select(GEOID, common_days19)

remove(j1, j2)

# Do the same for 2020
# Most common day of week by BG
day20 <- sd_crimes %>% 
  filter(REPORTING_YEAR == 2020) %>% 
  group_by(GEOID, WEEKDAY) %>%
  summarise(COUNT = sum(NUMBER_ACTUAL_REPORTED))

# Spread
day20 <- day20 %>% 
  spread(key = WEEKDAY, value = COUNT, fill = 0)

# Find most common day
j1 <- max.col(day20[, c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")], "first")
day20$max_day <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[j1]

# Find second most common day in case there's a tie (so we'll at least have two)
j2 <- max.col(day20[, c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")], "last")
day20$max_day2 <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")[j2]

# If max_day and max_day2 are the same, keep one
## If they're different, paste together, separated by comma
day20$common_days20 <- ifelse(day20$max_day == day20$max_day2, day20$max_day, paste0(day20$max_day, ", ", day20$max_day2))

# Select only columns we need
day20 <- day20 %>% 
  select(GEOID, common_days20)

remove(j1, j2)

# Merge together
days <- left_join(day19, day20, by = "GEOID")

remove(day19, day20)

#################################################################################################
################################### SD TIME_CAT ANALYSIS ########################################
#################################################################################################

# Determine the most common "time" of day using DAY_CAT (early morning, morning, afternoon, night)
# Most common day of week by BG
time19 <- sd_crimes %>% 
  filter(REPORTING_YEAR == 2019) %>% 
  group_by(GEOID, DAY_CAT) %>%
  summarise(COUNT = sum(NUMBER_ACTUAL_REPORTED))

# Spread
time19 <- time19 %>% 
  spread(key = DAY_CAT, value = COUNT, fill = 0)

# Find most common
j1 <- max.col(time19[, c("EM", "M", "A", "N")], "first")
time19$max_time <- c("EM", "M", "A", "N")[j1]

# Find second most common day in case there's a tie (so we'll at least have two)
j2 <- max.col(time19[, c("EM", "M", "A", "N")], "last")
time19$max_time2 <- c("EM", "M", "A", "N")[j2]

# If max_time and max_time2 are the same, keep one
## If they're different, paste together, separated by comma
time19$common_times19 <- ifelse(time19$max_time == time19$max_time2, time19$max_time, paste0(time19$max_time, ", ", time19$max_time2))

# Select only columns we need
time19 <- time19 %>% 
  select(GEOID, common_times19)

remove(j1, j2)

# Do the same for 2020
time20 <- sd_crimes %>% 
  filter(REPORTING_YEAR == 2020) %>% 
  group_by(GEOID, DAY_CAT) %>%
  summarise(COUNT = sum(NUMBER_ACTUAL_REPORTED))

# Spread
time20 <- time20 %>% 
  spread(key = DAY_CAT, value = COUNT, fill = 0)

# Find most common
j1 <- max.col(time20[, c("EM", "M", "A", "N")], "first")
time20$max_time <- c("EM", "M", "A", "N")[j1]

# Find second most common day in case there's a tie (so we'll at least have two)
j2 <- max.col(time20[, c("EM", "M", "A", "N")], "last")
time20$max_time2 <- c("EM", "M", "A", "N")[j2]

# If max_time and max_time2 are the same, keep one
## If they're different, paste together, separated by comma
time20$common_times20 <- ifelse(time20$max_time == time20$max_time2, time20$max_time, paste0(time20$max_time, ", ", time20$max_time2))

# Select only columns we need
time20 <- time20 %>% 
  select(GEOID, common_times20)

remove(j1, j2)

# Merge together
times <- left_join(time19, time20, by = "GEOID")

remove(time19, time20, sd_crimes)

#################################################################################################
################################ PIVOT CRIME CATEGORY ###########################################
#################################################################################################

# Group
group <- sd %>%
  group_by(GEOID, CRIME_CATEGORY, REPORTING_YEAR) %>%
  summarise(TOTAL = sum(NUMBER_ACTUAL_REPORTED))

# Create subset for each year
c2019 <- filter(group, REPORTING_YEAR == 2019)
c2020 <- filter(group, REPORTING_YEAR == 2020)

sum(c2019$TOTAL)
# 79495 == matches earlier calculations, OK to proceed
sum(c2020$TOTAL)
# 95301 == matches earlier calculations, OK to proceed

# Spread
c2019 <- c2019 %>%  
  select(-REPORTING_YEAR) %>% 
  spread(key = CRIME_CATEGORY, value = TOTAL, fill = 0)

c2020 <- c2020 %>%  
  select(-REPORTING_YEAR) %>% 
  spread(key = CRIME_CATEGORY, value = TOTAL, fill = 0)

# Create total columns for all crimes reported to SD Agency
# c2019$ALL_CRIMES <- rowSums(c2019[, c(2:14)])
# c2020$ALL_CRIMES <- rowSums(c2020[, c(2:14)])

# Create total columns for violent crimes reported to SD Agency
c2019$ALL_V_CRIMES <- rowSums(c2019[, c(2, 3, 7, 10, 13)])
c2020$ALL_V_CRIMES <- rowSums(c2020[, c(2, 3, 7, 10, 13)])

# Filter to columns / categories we want
c2019 <- c2019 %>% select(GEOID, `Aggravated Assault`, `Armed Robbery`,
                          Murder, `Non Res Burglary`, Rape, `Res Burglary`,
                          `Strong ArmRobbery`, ALL_V_CRIMES)
c2020 <- c2020 %>% select(GEOID, `Aggravated Assault`, `Armed Robbery`,
                          Murder, `Non Res Burglary`, Rape, `Res Burglary`,
                          `Strong ArmRobbery`, ALL_V_CRIMES)

# Rename columns for merge
names(c2019) <- c("GEOID", "aa19", "ro19", "m19", "nrb19", "r19", "rb19", "sr19", "tv19")
names(c2020) <- c("GEOID", "aa20", "ro20", "m20", "nrb20", "r20", "rb20", "sr20", "tv20")

# Combine them all together
all1 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(c2019, c2020))
## Row count == 994

# Remove the pivots and group
remove(c2019, c2020, group)

#################################################################################################
################################ PIVOT OTHER CRIMES #############################################
#################################################################################################

# Filter to just the "other" crimes we want to look at
group2 <- sd %>% 
  filter(CRIME_DESCRIPTION %in% c("RAPE ATTEMPT", "FRAUD", "CHILD AND FAMILY", "MALICIOUS MISCHIEF") | 
           CRIME_CATEGORY %in% c("Larceny < $400", "Larceny >= $400"))
## New row count == 53,052

# Create new category to break apart larceny into serious / less serious
group2$CRIME_DESC_USE <- group2$CRIME_DESCRIPTION

# Rename the larceny crimes in new column
group2$CRIME_DESC_USE <- ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 BICYCLE", "msl",
                                ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 BUILDING", "msl",
                                       ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 COIN MACH", "msl",
                                              ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 FROM VEHICL", "msl",
                                                     ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 OTHER", "msl",
                                                            ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 PICKPOCKET", "msl",
                                                                   ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 PURSESNATCH", "msl",
                                                                          ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 SHOPLIFT", "msl",
                                                                                 ifelse(group2$CRIME_DESC_USE == "LARCENY 200-400 VEH PARTS", "msl", group2$CRIME_DESC_USE)))))))))

# More renaming
group2$CRIME_DESC_USE <- ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 BICYCLE", "lsl",
                                ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 BUILDING", "lsl",
                                       ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 COIN MACH", "lsl",
                                              ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 FROM VEHICLE", "lsl",
                                                     ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 OTHER", "lsl",
                                                            ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 PICK POCKET", "lsl",
                                                                   ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 PURSE SNATCH", "lsl",
                                                                          ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 SHOPLIFT", "lsl",
                                                                                 ifelse(group2$CRIME_DESC_USE == "LARCENY 50-199 VEH PARTS", "lsl", group2$CRIME_DESC_USE)))))))))

# More renaming
group2$CRIME_DESC_USE <- ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 BICYCLE", "lsl",
                                ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 BUILDING", "lsl",
                                       ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 COIN MACH", "lsl",
                                              ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 FROM VEHIC", "lsl",
                                                     ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 OTHER", "lsl",
                                                            ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 PICKPOCKET", "lsl",
                                                                   ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 PURSE SNAT", "lsl",
                                                                          ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 SHOPLIFT", "lsl",
                                                                                 ifelse(group2$CRIME_DESC_USE == "LARCENY UNDER 50 VEH PARTS", "lsl", group2$CRIME_DESC_USE)))))))))

# More renaming
group2$CRIME_DESC_USE <- ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 BICYCLE", "msl",
                                ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 FROM BUILDING", "msl",
                                       ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 COIN MACHINE", "msl",
                                              ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 FROM VEHICLE", "msl",
                                                     ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 OTHER", "msl",
                                                            ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 PICK POCKET", "msl",
                                                                   ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 PURSE SNATCH", "msl",
                                                                          ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 SHOPLIFT", "msl",
                                                                                 ifelse(group2$CRIME_DESC_USE == "LARCENY MORE THAN 400 VEHICLE PARTS", "msl", group2$CRIME_DESC_USE)))))))))

# Check
table(group2$CRIME_DESC_USE)

# Group
group2 <- group2 %>%
  group_by(GEOID, CRIME_DESC_USE, REPORTING_YEAR) %>%
  summarise(TOTAL = sum(NUMBER_ACTUAL_REPORTED))

# Create subset for each year
c2019 <- filter(group2, REPORTING_YEAR == 2019)
c2020 <- filter(group2, REPORTING_YEAR == 2020)

# Spread
c2019 <- c2019 %>%  
  select(-REPORTING_YEAR) %>% 
  spread(key = CRIME_DESC_USE, value = TOTAL, fill = 0)

c2020 <- c2020 %>%  
  select(-REPORTING_YEAR) %>% 
  spread(key = CRIME_DESC_USE, value = TOTAL, fill = 0)

# Rename columns for merge
names(c2019) <- c("GEOID", "cf19", "f19", "lsl19", "mm19", "msl19", "ra19")
names(c2020) <- c("GEOID", "cf20", "f20", "lsl20", "mm20", "msl20", "ra20")

# Combine them all together
all2 <- Reduce(function(x, y) merge(x, y, all=TRUE), list(c2019, c2020))
## Row count == 858

# Remove the pivots and group
remove(c2019, c2020, group2)

# Combine all1 and all2 together
all <- Reduce(function(x, y) merge(x, y, all=TRUE), list(all2, all1))

remove(all1, all2)

#################################################################################################
#################################### ALL ANALYSIS ###############################################
#################################################################################################

# Join all with pop
all <- left_join(all, pop, by = "GEOID")

names(all)

# Remove columns we don't need
all <- all %>% 
  select(-ID, -NAME, -TRACTCE, -BLKGRPCE)

# Rearrange columns
all <- all %>%
  select(GEOID, TRACT_BG, CITY, CITY2, HOOD, X2019, cf19, f19, lsl19, msl19, mm19, 
         nrb19, rb19, aa19, ro19, m19, r19, ra19, sr19, tv19, cf20, f20, lsl20, 
         msl20, mm20, nrb20, rb20, aa20, ro20, m20, r20, ra20, sr20, tv20)

# Check for null values
## When none of the selected crimes occurred in a given GEOID, it returns NA
sum(is.na(all))
# 3,009

# Change NAs to 0 for summing
all[is.na(all)] <- 0

# Create total columns for each type of crime
all$totalcf <- rowSums(all[, c(7,21)])
all$totalf <- rowSums(all[, c(8,22)])
all$totallsl <- rowSums(all[, c(9,23)])
all$totalmsl <- rowSums(all[, c(10,24)])
all$totalmm <- rowSums(all[, c(11,25)])
all$totalnrb <- rowSums(all[, c(12,26)])
all$totalrb <- rowSums(all[, c(13,27)])
all$totalaa <- rowSums(all[, c(14,28)])
all$totalro <- rowSums(all[, c(15,29)])
all$totalm <- rowSums(all[, c(16,30)])
all$totalr <- rowSums(all[, c(17,31)])
all$totalra <- rowSums(all[, c(18,32)])
all$totalsr <- rowSums(all[, c(19,33)])

# Create total column for 19 and 20 all violent crimes
all$total_violent_1920 <- rowSums(all[, c(20,34)])

# Create total column for "other crimes" in 2019
all$t_other19 <- rowSums(all[, c(7:13)])

# Create total column for "other crimes" in 2020
all$t_other20 <- rowSums(all[, c(21:27)])

# Create total column for 1920 "other crimes"
all$t_other_1920 <- rowSums(all[, c(49,50)])

names(all)

# Rearrange columns again
all <- all %>%
  select(GEOID, TRACT_BG,	CITY,	CITY2, HOOD, X2019,	cf19,	f19, lsl19, msl19, 
         mm19, nrb19, rb19, aa19, ro19, m19, r19, ra19, sr19, tv19, t_other19,
         cf20, f20, lsl20, msl20, mm20, nrb20, rb20, aa20, ro20, m20, r20,
         ra20, sr20, tv20, t_other20, totalcf, totalf, totallsl, totalmsl,
         totalmm, totalnrb, totalrb, totalaa, totalro, totalm, totalr, totalra,
         totalsr, total_violent_1920, t_other_1920)

#################################################################################################
################################### OVERALL CALCS ###############################################
#################################################################################################

# Average of violent crimes reported to SD Agency
mean(all$total_violent_1920)
## mean == 10.27163 ~ 10 violent crimes per geoid

# Average of other sdut crimes reported to SD Agency
mean(all$t_other_1920)
## mean == 58.32998 ~ 58 sdut selected crimes per geoid

# Filter to just crimes within the city of SD (all bgs within and touching)
temp <- all %>% 
  filter(CITY == "San Diego")

# Average of violent crimes in SD
mean(temp$total_violent_1920)
## mean == 11.99177 ~ 12 violent crimes per geoid

# Average of "other crimes" in SD
mean(temp$t_other_1920)
## mean == 68.10341 ~ 68 sdut selected crimes per geoid

remove(temp)

#################################################################################################
################################# CHANGE AMONG SDPD #############################################
#################################################################################################

# Calculate changes in other and violent crimes for all crimes reported to SD agency
## Sum other and violent crimes columns
totals_sdpd <- colSums(all[,c(20,21,35,36)]) %>% as.data.frame()

# Transpose
totals_sdpd <- t(totals_sdpd) %>% as.data.frame()

# Calculate percent change in all violent crimes reportd to SD agency
totals_sdpd <- totals_sdpd %>% 
  mutate(t_violent_change1920 = round(((tv20 - tv19) / tv19)*100, digits = 1))

# Calculate percent change in all other crimes reported to SD agency
totals_sdpd <- totals_sdpd %>% 
  mutate(t_other_change1920 = round(((t_other20 - t_other19) / t_other19)*100, 
                                    digits = 1))

names(all)
# Calculate changes in each crime cat for all crimes reported to SD agency
cats_sdpd <- colSums(all[,c(7,8,9,10,11,12,13,
                            14,15,16,17,18,19,
                            22,23,24,25,26,27,
                            28,29,30,31,32,33,34)]) %>% as.data.frame()

# Transpose
cats_sdpd <- t(cats_sdpd) %>% as.data.frame()

# Calculate percent change in each crime cat for all crimes reported to SD agency
cats_sdpd <- cats_sdpd %>% 
  mutate(cf_change = round(((cf20 - cf19) / cf19)*100, digits = 1),
         f_change = round(((f20 - f19) / f19)*100, digits = 1),
         lsl_change = round(((lsl20 - lsl19) / lsl19)*100, digits = 1),
         msl_change = round(((msl20 - msl19) / msl19)*100, digits = 1),
         mm_change = round(((mm20 - mm19) / mm19)*100, digits = 1),
         nrb_change = round(((nrb20 - nrb19) / nrb19)*100, digits = 1),
         rb_change = round(((rb20 - rb19) / rb19)*100, digits = 1),
         aa_change = round(((aa20 - aa19) / aa19)*100, digits = 1),
         ro_change = round(((ro20 - ro19) / ro19)*100, digits = 1),
         m_change = round(((m20 - m19) / m19)*100, digits = 1),
         r_change = round(((r20 - r19) / r19)*100, digits = 1),
         ra_change = round(((ra20 - ra19) / ra19)*100, digits = 1),
         sr_change = round(((sr20 - sr19) / sr19)*100, digits = 1))

write.csv(cats_sdpd, "cats_sdpd.csv")

#################################################################################################
################################# CHANGE AMONG CITY #############################################
#################################################################################################

# Filter to just bgs / crimes in city of SD (CITY, not CITY2)
temp <- all %>% 
  filter(CITY == "San Diego")

# Calculate changes in other and violent crimes for all crimes in city of SD
totals_sd <- colSums(temp[,c(20,21,35,36)]) %>% as.data.frame()

# Transpose
totals_sd <- t(totals_sd) %>% as.data.frame()

# Calculate percent change in all violent crimes in city of SD
totals_sd <- totals_sd %>% 
  mutate(t_violent_change1920 = round(((tv20 - tv19) / tv19)*100, digits = 1))

# Calculate percent change in all other crimes in city of SD
totals_sd <- totals_sd %>% 
  mutate(t_other_change1920 = round(((t_other20 - t_other19) / t_other19)*100, 
                                    digits = 1))

names(temp)
# Calculate changes in each crime cat for all crimes in city of SD
cats_sd <- colSums(temp[,c(7,8,9,10,11,12,13,
                           14,15,16,17,18,19,
                           22,23,24,25,26,27,
                           28,29,30,31,32,33,34)]) %>% as.data.frame()

# Transpose
cats_sd <- t(cats_sd) %>% as.data.frame()

# Calculate percent change in each crime cat for all crimes in city of SD
cats_sd <- cats_sd %>% 
  mutate(cf_change = round(((cf20 - cf19) / cf19)*100, digits = 1),
         f_change = round(((f20 - f19) / f19)*100, digits = 1),
         lsl_change = round(((lsl20 - lsl19) / lsl19)*100, digits = 1),
         msl_change = round(((msl20 - msl19) / msl19)*100, digits = 1),
         mm_change = round(((mm20 - mm19) / mm19)*100, digits = 1),
         nrb_change = round(((nrb20 - nrb19) / nrb19)*100, digits = 1),
         rb_change = round(((rb20 - rb19) / rb19)*100, digits = 1),
         aa_change = round(((aa20 - aa19) / aa19)*100, digits = 1),
         ro_change = round(((ro20 - ro19) / ro19)*100, digits = 1),
         m_change = round(((m20 - m19) / m19)*100, digits = 1),
         r_change = round(((r20 - r19) / r19)*100, digits = 1),
         ra_change = round(((ra20 - ra19) / ra19)*100, digits = 1),
         sr_change = round(((sr20 - sr19) / sr19)*100, digits = 1))

remove(temp)

# Export categories as csv
write.csv(cats_sd, "cats_sd.csv")

### BACK TO CALCULATIONS OF ALL DF
# Change NAs to 0 for calculations
all[is.na(all)] <- 0

# Calculate percent change in violent crimes
all <- all %>% 
  mutate(t_violent_change1920 = round(((tv20 - tv19) / tv19)*100, digits = 1))

# Calculate percent change in sdut selected crimes
all <- all %>% 
  mutate(t_other_change1920 = round(((t_other20 - t_other19) / t_other19)*100, 
                                    digits = 1))

# Merge with most common days
all <- left_join(all, days, by = "GEOID")

# Merge with most common times
all <- left_join(all, times, by = "GEOID")

# Create csv file with final analysis
write.csv(all,file="r_analysis_1920.csv")

remove(days, times, sd, pop)