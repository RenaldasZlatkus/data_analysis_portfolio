#Set your working directory to where you want to save the data via setwd(~your path), or Session -> Set Working Directory -> Choose Directory.

#Install packages required packages for this process.

#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("plyr")
#install.packages("data.table")
#install.packages("DataExplorer")
#install.packages("lubridate")
#install.packages("janitor")
#install.packages("tidygeocoder")
#install.packages("rnoaa")
#install.packages("weathermetrics")

#Load library.

library(tidyverse)
library(plyr)
library(data.table)
library(DataExplorer)
library(lubridate)
library(janitor)
library(tidygeocoder)
library(rnoaa)
library(weathermetrics)


zip_location <- getwd()

#Download data between 2020_09 and 2021_08 from https://divvy-tripdata.s3.amazonaws.com/index.html.

download.file("https://divvy-tripdata.s3.amazonaws.com/202009-divvy-tripdata.zip", paste("zip_location", "202009-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202010-divvy-tripdata.zip", paste("zip_location", "202010-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202011-divvy-tripdata.zip", paste("zip_location", "202011-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202012-divvy-tripdata.zip", paste("zip_location", "202012-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202101-divvy-tripdata.zip", paste("zip_location", "202101-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202102-divvy-tripdata.zip", paste("zip_location", "202102-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202103-divvy-tripdata.zip", paste("zip_location", "202103-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202104-divvy-tripdata.zip", paste("zip_location", "202104-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202105-divvy-tripdata.zip", paste("zip_location", "202105-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202106-divvy-tripdata.zip", paste("zip_location", "202106-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202107-divvy-tripdata.zip", paste("zip_location", "202107-divvy-tripdata.zip"))
download.file("https://divvy-tripdata.s3.amazonaws.com/202108-divvy-tripdata.zip", paste("zip_location", "202108-divvy-tripdata.zip"))

#Extracting all csv files from the zip files saved in your Working directory.

my_dir <- getwd()
zip_file <- list.files(path = my_dir, pattern = "*.zip",
                       full.names = TRUE)

ldply(.data = zip_file, .fun = unzip, exdir = my_dir)

#Load extracted csv files into one data.table.

dataset <- ldply(list.files(path = my_dir, pattern = "*.csv", full.names = TRUE), fread, header=T, na.string = c("", "NA"))

#Investigate data table.

colnames(dataset)

#Rename column names.

setnames(dataset,c("rideable_type", "start_station_name", "end_station_name", "member_casual"),
         c("bike_type", "start_station", "end_station", "user_type"))

colnames(dataset)

#Remove "_bike" from column rideable_type.

str(dataset)

dataset <- dataset %>%
  mutate_at("bike_type", str_replace, "_bike", "")

str(dataset)
gc()

#Check unique strings in start_station_name and end_station_name.
#We can see in row 659 "HIBBARD ST BIKE CHECKING (LBS-WH-TEST)", 707 "WEST CHI-WATSON", 726 "DIVVY CASSETTE REPAIR MOBILE STATION", 747 the number "351".
#These will be investigate more, but they seem not to fit in. 

uniq_street_name <- data.frame(unique(dataset$start_station))
glimpse(uniq_street_name)
str(c(uniq_street_name[707,], uniq_street_name[726,], uniq_street_name[747,]))


#Checking for rows with only uppercase characters

upper_case <- dataset %>%
  filter(str_detect(start_station, '[:upper:]') & !str_detect(start_station, '[:lower:]'))

#Viewing upper_case confirms my suspicion that these are no regular user rides and thus can be deleted.

str(upper_case)

#Start_station_id instead of start_station_name

check_1 <- dataset %>% 
  filter(nchar(start_station) < 4)

#start_station_id 351 is Cottage Grove Ave & 51st St

dataset$start_station[dataset$start_station == "351"] <- "Cottage Grove Ave & 51st St"

#DiVVY has 1424 observation that match in start_station. 

check_2 <- dataset[dataset$start_station %like% "DIVVY", ]

#Delete 1424 rows containing "DIVVY".

dataset <- dataset[!grepl("DIVVY",dataset$start_station),]

#TEST has 47 observation that match in start_station. 

check_3 <- dataset[dataset$start_station %like% "TEST", ]

#Delete 47 rows containing "TEST".

dataset <- dataset[!grepl("TEST",dataset$start_station),]

#WATSON has 83 observation that match in start_station.

check_4 <- dataset[dataset$start_station %like% "WATSON", ]

#Delete 83 rows containing "WATSON".

dataset <- dataset[!grepl("WATSON",dataset$start_station),]

#DIVVY has 4 observation that match in end_station. 

check_5 <- dataset[dataset$end_station %like% "DIVVY", ]

#Delete 4 rows containing "Divvy".

dataset <- dataset[!grepl("DIVVY",dataset$end_station),]

#TEST has 141 observation that match in end_station.

check_6 <- dataset[dataset$end_station %like% "TEST", ]

#Delete 141 row containing "TEST".

dataset <- dataset[!grepl("TEST",dataset$end_station),]

#WATSON has 0 observations that match in end_station.

check_7 <- dataset[dataset$end_station %like% "WATSON", ]

#Zero uppercases
upper_case <- dataset %>%
  filter(str_detect(start_station, '[:upper:]') & !str_detect(start_station, '[:lower:]'))

rm(check_1, check_2, check_3, check_4, check_5, check_6, check_7, upper_case, uniq_street_name)

gc()

#Further investigation of how the data is structured.

head(dataset)
tail(dataset)
glimpse(dataset)
summary(dataset)

#Dubble check data type of each column.

is.character(dataset$ride_id)
is.character(dataset$bike_type)
is.POSIXct(dataset$started_at)
is.POSIXct(dataset$ended_at)
is.character(dataset$start_station)
is.character(dataset$start_station_id)
is.character(dataset$end_station)
is.character(dataset$end_station_id)
is.numeric(dataset$start_lat)
is.numeric(dataset$start_lng)
is.numeric(dataset$end_lat)
is.numeric(dataset$end_lng)
is.character(dataset$user_type)


#Check for NA's
# 6 columns appear to have missing values and must be addressed. 

plot_missing(dataset)

check_9 <- dataset %>% 
  filter(is.na(start_station))

check_10 <- dataset %>% 
  filter(is.na(end_station))

#Data enrichment, locating streets through latitude and longitude data.
#Tidygeocoder is a newly published R package which provides a tidyverse-style interface for geocoding. 
#It returns latitude and longitude coordinates in tibble format from addresses using the US Census or Nominatim (OSM) geocoder services.
#Alternative solutions are possible, but they are limited with the amount of free api calls available. Thus, using tidygecoder will circumvent this.
#However, this will take approximately 30 min to complete.

enriched_start_station <- check_9 %>% 
  reverse_geocode(lat = start_lat, long = start_lng, method = "osm",
                  full_results = TRUE) %>% 
  select(ride_id, address, road)

enriched_end_station <- check_10 %>% 
  reverse_geocode(lat = end_lat, long = end_lng, method = "osm",
                  full_results = TRUE) %>% 
  select(ride_id, address, road)

rm(check_9, check_10)
gc()

#Combine road column with A (address) into one column.

enriched_start <- as.data.table(enriched_start_station) %>% 
  separate(address, c("A", "B", ",")) %>%
  mutate(start_station = coalesce(road, A)) %>% 
  select(c(ride_id, start_station))

enriched_end <- as.data.table(enriched_end_station) %>% 
  separate(address, c("A", "B", ",")) %>%
  mutate(end_station = coalesce(road, A)) %>% 
  select(c(ride_id, end_station))             

rm(enriched_start_station, enriched_end_station)
gc()

#Merge data set with the enriched data.

dataset <- as.data.table(dataset)

setkey(dataset, ride_id)
setkey(enriched_start, ride_id)
setkey(enriched_end, ride_id)

merge1 <- merge.data.table(dataset, enriched_start, all = TRUE)
merge2 <- merge.data.table(merge1, enriched_end, all = TRUE)

dataset <- merge2 %>% 
  mutate(start_station = coalesce(start_station.x, start_station.y)) %>% 
  mutate(end_station = coalesce(end_station.x, end_station.y)) %>% 
  select(-c(start_station.x, end_station.x, start_station.y, end_station.y)) %>% 
  relocate(start_station, .after = ended_at) %>% 
  relocate(end_station, .after = start_station_id)

#NA data is significantly reduced. However, end_lng, end_lat, and end_station still have 0.1% missing, but will
#not be deleted

plot_missing(dataset)

remove(enriched_end, enriched_start, merge1, merge2)
gc()

#Data enrichment from National Oceanic and Atmospheric Administration using their r-package (rnoaa)

weather_stations <- ghcnd_stations()

#Chicago  latitude and longitude data frame.

id = "chicago"
latitude <- as.numeric("41.881832")
longitude <- as.numeric("-87.623177")
lat_lon_df <- data.frame(id, latitude, longitude)

#Locate a weather stations that records all possible weather variables and has records of 2020 and 2021.

lcd_weather_stations <- meteo_distance(weather_stations, lat_lon_df$latitude, lat_lon_df$longitude, radius = 25, limit = NULL)
isd_weather_stations <- isd_stations_search(lat = lat_lon_df$latitude, lon = lat_lon_df$longitude, radius = 25)

str(lcd_weather_stations)
str(isd_weather_stations)

#Chicago Midway Airport seems to meet all the criteria.

isd_2020 <- isd_weather_stations %>% 
  filter(end > 20200101)

lcd_2020 <- lcd_weather_stations %>% 
  filter(last_year > 2020)


#Select all variables and then determine which to use.

columns <- lcd_columns(STATION = "character",
                       DATE = "POSIXct",
                       LATITUDE = "numeric",
                       LONGITUDE = "numeric",
                       ELEVATION = "numeric",
                       NAME = "character",
                       REPORT_TYPE = "character",
                       SOURCE = "character",
                       HourlyAltimeterSetting = "character",
                       HourlyDewPointTemperature = "character",
                       HourlyDryBulbTemperature = "character",
                       HourlyPrecipitation = "character",
                       HourlyPresentWeatherType = "character",
                       HourlyPressureChange = "character",
                       HourlyPressureTendency = "integer",
                       HourlyRelativeHumidity = "character",
                       HourlySkyConditions = "character",
                       HourlySeaLevelPressure = "character",
                       HourlyStationPressure = "character",
                       HourlyVisibility = "character",
                       HourlyWetBulbTemperature = "character",
                       HourlyWindDirection = "character",
                       HourlyWindGustSpeed = "character",
                       HourlyWindSpeed = "character",
                       Sunrise = "numeric",
                       Sunset = "numeric",
                       DailyAverageDewPointTemperature = "character",
                       DailyAverageDryBulbTemperature = "character",
                       DailyAverageRelativeHumidity = "character",
                       DailyAverageSeaLevelPressure = "character",
                       DailyAverageStationPressure = "character",
                       DailyAverageWetBulbTemperature = "character",
                       DailyAverageWindSpeed = "character",
                       DailyCoolingDegreeDays = "numeric",
                       DailyDepartureFromNormalAverageTemperature = "numeric",
                       DailyHeatingDegreeDays = "numeric",
                       DailyMaximumDryBulbTemperature = "numeric",
                       DailyMinimumDryBulbTemperature = "numeric",
                       DailyPeakWindDirection = "numeric",
                       DailyPeakWindSpeed = "numeric",
                       DailyPrecipitation = "character",
                       DailySnowDepth = "character",
                       DailySnowfall = "character",
                       DailySustainedWindDirection = "numeric",
                       DailySustainedWindSpeed = "numeric",
                       DailyWeather = "character",
                       MonthlyAverageRH = "numeric",
                       MonthlyDaysWithGT001Precip = "character",
                       MonthlyDaysWithGT010Precip = "character",
                       MonthlyDaysWithGT32Temp = "numeric",
                       MonthlyDaysWithGT90Temp = "numeric",
                       MonthlyDaysWithLT0Temp = "numeric",
                       MonthlyDaysWithLT32Temp = "numeric",
                       MonthlyDepartureFromNormalAverageTemperature = "numeric",
                       MonthlyDepartureFromNormalCoolingDegreeDays = "numeric",
                       MonthlyDepartureFromNormalHeatingDegreeDays = "numeric",
                       MonthlyDepartureFromNormalMaximumTemperature = "numeric",
                       MonthlyDepartureFromNormalMinimumTemperature = "numeric",
                       MonthlyDepartureFromNormalPrecipitation = "numeric",
                       MonthlyDewpointTemperature = "numeric",
                       MonthlyGreatestPrecip = "character",
                       MonthlyGreatestPrecipDate = "character",
                       MonthlyGreatestSnowDepth = "numeric",
                       MonthlyGreatestSnowDepthDate = "character",
                       MonthlyGreatestSnowfall = "character",
                       MonthlyGreatestSnowfallDate = "character",
                       MonthlyMaxSeaLevelPressureValue = "numeric",
                       MonthlyMaxSeaLevelPressureValueDate = "character",
                       MonthlyMaxSeaLevelPressureValueTime = "character",
                       MonthlyMaximumTemperature = "numeric",
                       MonthlyMeanTemperature = "numeric",
                       MonthlyMinSeaLevelPressureValue = "numeric",
                       MonthlyMinSeaLevelPressureValueDate = "character",
                       MonthlyMinSeaLevelPressureValueTime = "character",
                       MonthlyMinimumTemperature = "numeric",
                       MonthlySeaLevelPressure = "numeric",
                       MonthlyStationPressure = "numeric",
                       MonthlyTotalLiquidPrecipitation = "character",
                       MonthlyTotalSnowfall = "character",
                       MonthlyWetBulb = "numeric",
                       AWND = "numeric",
                       CDSD = "numeric",
                       CLDD = "numeric",
                       DSNW = "numeric",
                       HDSD = "numeric",
                       HTDD = "numeric",
                       NormalsCoolingDegreeDay = "numeric",
                       NormalsHeatingDegreeDay = "numeric",
                       ShortDurationEndDate005 = "character",
                       ShortDurationEndDate010 = "character",
                       ShortDurationEndDate015 = "character",
                       ShortDurationEndDate020 = "character",
                       ShortDurationEndDate030 = "character",
                       ShortDurationEndDate045 = "character",
                       ShortDurationEndDate060 = "character",
                       ShortDurationEndDate080 = "character",
                       ShortDurationEndDate100 = "character",
                       ShortDurationEndDate120 = "character",
                       ShortDurationEndDate150 = "character",
                       ShortDurationEndDate180 = "character",
                       ShortDurationPrecipitationValue005 = "numeric",
                       ShortDurationPrecipitationValue010 = "numeric",
                       ShortDurationPrecipitationValue015 = "numeric",
                       ShortDurationPrecipitationValue020 = "numeric",
                       ShortDurationPrecipitationValue030 = "numeric",
                       ShortDurationPrecipitationValue045 = "numeric",
                       ShortDurationPrecipitationValue060 = "numeric",
                       ShortDurationPrecipitationValue080 = "numeric",
                       ShortDurationPrecipitationValue100 = "numeric",
                       ShortDurationPrecipitationValue120 = "numeric",
                       ShortDurationPrecipitationValue150 = "numeric",
                       ShortDurationPrecipitationValue180 = "numeric",
                       REM = "character",
                       BackupDirection = "character",
                       BackupDistance = "character",
                       BackupDistanceUnit = "character",
                       BackupElements = "character",
                       BackupElevation = "character",
                       BackupEquipment = "character",
                       BackupLatitude = "character",
                       BackupLongitude = "character",
                       BackupName = "character",
                       WindEquipmentChangeDate = "character")

#Combine USAF and WBAN numbers from isd_2020 to get the lcd station id.

chicago_midway <- "72534014819"

#Gather all weather data from 2020 and 2021.

midway_2020 <- data.frame(lcd(station = chicago_midway, year = 2020, col_types = columns))
midway_2021 <- data.frame(lcd(station = chicago_midway, year = 2021, col_types = columns))

#Merge data from 2020 and 2021 into one data frame.

midway_data <- merge(midway_2020, midway_2021, all = TRUE) %>% 
  select(date, hourlydewpointtemperature, hourlydrybulbtemperature, hourlyprecipitation,
         hourlyrelativehumidity, hourlywetbulbtemperature, hourlywinddirection,
         hourlywindspeed) %>% 
  dplyr::rename(dew_point = hourlydewpointtemperature, 
         drybulb_temperature = hourlydrybulbtemperature, 
         precipitation = hourlyprecipitation, 
         humidity = hourlyrelativehumidity, 
         wetbulb_temperature = hourlywetbulbtemperature, 
         wind_direction = hourlywinddirection, 
         wind_speed = hourlywindspeed)

str(midway_data)

#https://data.noaa.gov/dataset/dataset/u-s-local-climatological-data-lcd/resource/ee7381ea-647a-434f-8cfa-81202b9b4c05 provides 
#documentation for abbreviation on how data is measured.
# Under special indicator appendix in the lcd documentation s stands for suspect value. Thus, these values can be used but the s has to be
#removed from the data.

midway_test1 <- midway_data %>%
  group_by(dew_point) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#In midway_test1 we can see that values that are blank/* or those ending with a "s" need to be changed. For values that are blank or have an *
#I will change them to NA's.

midway_data$dew_point <- gsub(pattern = "s", replacement = "", midway_data$dew_point)
midway_data$dew_point <- gsub(pattern = "[*]", replacement = "", midway_data$dew_point)

midway_test1_1 <- midway_data %>%
  group_by(dew_point) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))


#Dry bulb temperature.

midway_test2 <- midway_data %>%
  group_by(drybulb_temperature) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

midway_data$drybulb_temperature <- gsub(pattern = "s", replacement = "", midway_data$drybulb_temperature)
midway_data$drybulb_temperature <- gsub(pattern = "[*]", replacement = "", midway_data$drybulb_temperature)

midway_test2_1 <- midway_data %>%
  group_by(drybulb_temperature) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#In the midway_test3 we can see the letter T  stands according to the NOAA documentation as a trace amount, but since a trace amount is hard to
#quantitative I will replace the T with 0.005 since the trace amount is more than "nothing" but lower than the recorded lowest value of 0.00.

midway_test3 <- midway_data %>%
  group_by(precipitation) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

midway_data$precipitation <- gsub(pattern = "s", replacement = "", midway_data$precipitation)
midway_data$precipitation <- gsub(pattern = "T", replacement = "0.005", midway_data$precipitation)

midway_test3_1 <- midway_data %>%
  group_by(precipitation) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#Humidity.

midway_test4 <- midway_data %>%
  group_by(humidity) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

midway_data$humidity <- gsub(pattern = "[*]", replacement = "", midway_data$humidity)

midway_test4_1 <- midway_data %>%
  group_by(humidity) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#Wet bulb temperature.

midway_test5 <- midway_data %>%
  group_by(wetbulb_temperature) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

midway_data$wetbulb_temperature <- gsub(pattern = "[*]", replacement = "", midway_data$wetbulb_temperature)

midway_test5_1 <- midway_data %>%
  group_by(wetbulb_temperature) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#Wind direction.
#According to https://cementanswers.com/what-does-vrb-mean-weather/, VRB stands for "Wind direction indicates the direction from which 
#the wind is blowing in degrees true (geographic, not magnetic). In the case of a variable wind, the direction (ddd) may be encoded
#as VRB when the mean wind speed is 3 knots or less, and a direction cannot be determined".

midway_test6 <- midway_data %>%
  group_by(wind_direction) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#Instead of deleting VRB or replacing it with an average I will instead use 999 to indicate this rare phenomena.

midway_data$wind_direction <- gsub(pattern = "VRB", replacement = "999", midway_data$wind_direction)

midway_test6_1 <- midway_data %>%
  group_by(wind_direction) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

#Wind speed.

midway_test7 <- midway_data %>%
  group_by(wind_speed) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

midway_data$wind_speed <- gsub(pattern = "s", replacement = "", midway_data$wind_speed)

midway_test7_1 <- midway_data %>%
  group_by(wind_speed) %>%
  dplyr::summarise(volume = n()) %>%
  arrange(desc(volume))

rm(midway_test1, midway_test1_1, midway_test2, midway_test2_1, midway_test3, midway_test3_1, midway_test4, midway_test4_1,
   midway_test5, midway_test5_1, midway_test6, midway_test6_1, midway_test7, midway_test7_1, weather_stations, isd_2020,
   isd_weather_stations,   lat_lon_df, lcd_2020, lcd_weather_stations, midway_2020, midway_2021)

#Transform character data types in the appropriate data types.

weather_data <- midway_data                                   # Duplicate data frame
weather_data$date <- as.numeric(weather_data$date)            # Change date into a numeric datatype
weather_data[weather_data == "" | weather_data == " "] <- NA  # Replace blank & space by NA
weather_data$date <- as.POSIXct(weather_data$date, tz= "UTC", origin = "1970-01-01")  #Change date into date datatype

#Drop rows from column dew_point because those empty rows are the remnants of column that only recorded the sunrise and sunset values.
weather_data <- weather_data %>% 
  drop_na(dew_point)

str(weather_data)
rm(midway_data)
#Change all character data types into numeric data types.

weather_data <- weather_data %>%
  dplyr::mutate(across(c(dew_point, drybulb_temperature, precipitation, humidity, wetbulb_temperature,
                                                 wind_direction, wind_speed), as.numeric))

str(weather_data)
#Combine weather data with the main data set.

weather_data <- as.data.table(weather_data)
setkey(dataset, started_at)
setkey(weather_data, date)
dataset <- weather_data[dataset, roll = "nearest"]
View(dataset)
str(dataset)
gc()

#Check for duplicated data.

dup_data<- dataset %>% 
  get_dupes(ride_id)

View(dup_data)

#Inspecting the duplicated data they all seem to have the same date. This could mean that these bikes have been taken out of the circulation for maintenance or any other reason.
#Although I can not verify this assumption I do feel confident that by deleting the duplicates this will not affect the analysis.

dataset$duplicate_data <- duplicated(dataset$ride_id)

#This will remove 211 rows with the duplicate data

dataset <- dataset[!(dataset$duplicate_data == TRUE),]

#Remove duplicate column

dataset$duplicate_data <- NULL 

rm(dup_data)
gc()

#Create a column with trip duration

dataset$trip_duration <- as.numeric(difftime(dataset$ended_at, dataset$date))

#Trip duration has a negative trip duration

summary(dataset)

#After inspection neg_trip subset I can see that the started_at and ended_at have been mixed up. Renaming started_at to ended_at 
#and ended_at to started_at and then relocating started_at before ended_at should revolve this issue.

neg_trip_duration <- dataset %>% 
  filter(trip_duration < 0)

View(neg_trip_duration)

neg_trip <- dataset %>% 
  select(everything(dataset)) %>% 
  filter(trip_duration < 0) %>% 
  relocate(date, .after = bike_type) %>% 
  relocate(ended_at, .before = dew_point) %>% 
  dplyr::rename(ended_at = date,
                date = ended_at)

neg_trip$trip_duration <- as.numeric(difftime(neg_trip$ended_at, neg_trip$date))

#Merge subset with main data set and arrange in ascending order

dataset <- rbind(dataset, neg_trip)
dataset <- dataset %>% 
  arrange(date)


#Delete rows with negative ride duration and rides with less than 60 seconds which according to Divvy are users who have cancelled their ride.
#Drop columns start/end_station_id as it will do be used for the analysis.

bike_data <- subset(dataset, trip_duration > 59) %>% 
  select(-c("start_station_id", "end_station_id"))

rm(neg_trip, neg_trip_duration)

#Add month, day, and day_of_week columns into data set. 

bike_data$month <- format(as.Date(bike_data$date), "%m")
bike_data$day <- format(as.Date(bike_data$date), "%d")
bike_data$day_of_week <- format(as.Date(bike_data$date), "%A")

str(bike_data)
summary(bike_data)
plot_missing(bike_data)

#Convert weather variables from Fahrenheit to Celsius

bike_data <- bike_data %>%
  dplyr::mutate(across(c(dew_point, drybulb_temperature, wetbulb_temperature), fahrenheit.to.celsius))

#Miles to kilometers per hour

bike_data$wind_speed <- convert_wind_speed(bike_data$wind_speed, old_metric = "mph", new_metric = "kmph", round = 2)

#Inches to altimeters

bike_data$precipitation <- convert_precip(bike_data$precipitation, old_metric = "inches", new_metric = "mm", round = 3)

#Heat index

bike_data$heat_index <- heat.index(t = bike_data$drybulb_temperature, dp = bike_data$dew_point, temperature.metric = "celsius",
                                   output.metric = "celsius", round = 2)

#Remove white space in every column that has data type as.character.

bike_data <- bike_data %>%
  dplyr::mutate(across(where(is.character), str_trim))

plot_missing(bike_data)

bike_data %>% mutate_if(is.character, str_to_upper) -> bike_data

gc()

dim(dataset)

#Save data
fwrite(dataset, file = "cyclistic_data_wrangling_process.csv", sep = ",", na = "NA", row.names = FALSE, col.names = TRUE)
