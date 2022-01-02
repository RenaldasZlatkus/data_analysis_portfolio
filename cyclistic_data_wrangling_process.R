#Set your working directory to where you want to save the data via setwd(~your path), or Session -> Set Working Directory -> Choose Directory.

#Install packages required packages for this process.

#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("DataExplorer")
#install.packages("janitor")
#install.packages("tidygeocoder")

#Load library.

library(tidyverse)
library(data.table)
library(DataExplorer)
library(lubridate)
library(janitor)
library(tidygeocoder)

#Download data between 2020_09 and 2021_08 from https://divvy-tripdata.s3.amazonaws.com/index.html.

download.file("https://divvy-tripdata.s3.amazonaws.com/202009-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202009-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202010-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202010-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202011-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202011-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202012-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202012-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202101-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202101-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202102-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202102-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202103-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202103-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202104-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202104-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202105-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202105-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202106-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202106-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202107-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202107-divvy-tripdata.zip")
download.file("https://divvy-tripdata.s3.amazonaws.com/202108-divvy-tripdata.zip", "C:/Users/renal/OneDrive/Bureaublad/final_Project_divvy_data/202108-divvy-tripdata.zip")

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

setnames(dataset, old = c("rideable_type", "start_station_name", "end_station_name", "member_casual"),
         new = c("bike_type", "start_station", "end_station", "user_type"))

#Remove "_bike" from column rideable_type.

dataset <- dataset %>%
  mutate_at("bike_type", str_replace, "_bike", "")

#Further investigation of how the data is structured.

head(dataset)
tail(dataset)
str(dataset)
glimpse(dataset)

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

summary(dataset)
plot_missing(dataset)

#----------------------------Check max and min length of start_station_name, end_station_name, start_station_id, 
#end_station_id, and member_casual variable length to see for any irregularities.

max(nchar(dataset$start_station), na.rm = T)
min(nchar(dataset$start_station), na.rm = T)
max(nchar(dataset$start_station_id), na.rm = T)
min(nchar(dataset$start_station_id), na.rm = T)
max(nchar(dataset$end_station), na.rm = T)
min(nchar(dataset$end_station), na.rm = T)
max(nchar(dataset$end_station_id), na.rm = T)
min(nchar(dataset$end_station_id), na.rm = T)
max(nchar(dataset$user_type), na.rm = T)
min(nchar(dataset$user_type), na.rm = T)

#Start_station_id instead of start_station_name
check_1 <- dataset %>% 
  filter(nchar(start_station) < 4)

#start_station_id 351 is Cottage Grove Ave & 51st St
dataset$start_station[dataset$start_station == "351"] <- "COTTAGE GROVE AVE & 51ST ST"

#Shows DIVVY CASSETTE REPAIR MOBILE STATION as a start and end_station
check_2 <- dataset %>% 
  filter(nchar(start_station_id) > 35)

#This tells me that there are rides with either testing or repairing. Thus I have to check if start or end _station_name
#for partial matches on "test" and "repair"

#DiVVY has 1424 observation that match in start_station. 
check_3 <- dataset[dataset$start_station %like% "DIVVY", ]
#Delete 1424 rows containing "DIVVY".
dataset <- dataset[!grepl("DIVVY",dataset$start_station),]

#TEST has 47 observation that match in start_station. 
check_4 <- dataset[dataset$start_station %like% "TEST", ]
#Delete 47 rows containing "TEST".
dataset <- dataset[!grepl("TEST",dataset$start_station),]

#WATSON has 83 observation that match in start_station. 
check_5 <- dataset[dataset$start_station %like% "WATSON", ]
#Delete 83 rows containing "WATSON".
dataset <- dataset[!grepl("WATSON",dataset$start_station),]

#DIVVY has 4 observation that match in end_station. 
check_6 <- dataset[dataset$end_station %like% "DIVVY", ]
#Delete 4 rows containing "Divvy".
dataset <- dataset[!grepl("DIVVY",dataset$end_station),]

#TEST has 141 observation that match in end_station.
check_7 <- dataset[dataset$end_station %like% "TEST", ]
#Delete 141 row containing "TEST".
dataset <- dataset[!grepl("TEST",dataset$end_station),]

#WATSON has 0 observations that match in end_station.
check_8 <- dataset[dataset$end_station %like% "WATSON", ]

rm(check_1, check_2, check_3, check_4, check_5, check_6, check_7, check_8)

#-----------------------------Investigating NA data

check_9 <- dataset %>% 
  filter(is.na(start_station))

check_10 <- dataset %>% 
  filter(is.na(end_station))

#-----------------------------------Data enrichment, find streets through latitude and longitude data.

enriched_start_station <- check_9 %>% 
  reverse_geocode(lat = start_lat, long = start_lng, method = "osm",
                  full_results = TRUE) %>% 
  select(ride_id, address, road)

enriched_end_station <- check_10 %>% 
  reverse_geocode(lat = end_lat, long = end_lng, method = "osm",
                  full_results = TRUE) %>% 
  select(ride_id, address, road)

#-----------------------------------Combine road column with A (address) into one column.

enriched_start <- as.data.table(enriched_start_station) %>% 
  separate(address, c("A", "B", ",")) %>%
  mutate(start_station = coalesce(road, A)) %>% 
  select(c(ride_id, start_station))

enriched_end <- as.data.table(enriched_end_station) %>% 
  separate(address, c("A", "B", ",")) %>%
  mutate(end_station = coalesce(road, A)) %>% 
  select(c(ride_id, end_station))             

#---------------------------------Merge dataset with the enriched data.

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

dataset %>% mutate_if(is.character, str_to_upper) -> dataset

plot_missing(dataset)

remove(enriched_end, enriched_end_station, enriched_start, enriched_start_station, merge1, merge2, check_9, check_10)
gc()
#-----------------------------------------------------------------------------------------------------BACKUP after enrichment
backup <- dataset 
dataset <- backup

#----------------------------Check for duplicated data.

dup_data<- dataset %>% 
  get_dupes(ride_id)

View(dup_data)

#Inspecting the duplicated data they all seem to have the same date. This could mean that these bikes have been taken out of the circulation for maintainance or any other reason.
#Allthough I can not varify this assumption I do feel confident that by deleting the duplicates this will not affect the analysis.

dataset$duplicate_data <- duplicated(dataset$ride_id)

#This will remove 211 rows with the duplicate data
dataset <- dataset[!(dataset$duplicate_data == TRUE),]

#Remove duplicate column
dataset$duplicate_data <- NULL 

rm(dup_data)

#------------------------------Creat a column with trip duration

dataset$trip_duration <- as.numeric(difftime(dataset$ended_at, dataset$started_at))

#Trip duration has a negative trip duration
summary(dataset)

#-----------------------------#Select all the columns for the merger later on
#After inspection neg_trip subset I can see that the started_at and ended_at have been mixed up. Renaming started_at to ended_at 
#and ended_at to started_at and then relocating started_at before ended_at should revolve this issue.

neg_trip <- dataset %>% 
  select(everything(dataset)) %>% 
  filter(trip_duration < 0) %>% 
  relocate(ended_at, .after = bike_type) %>% 
  dplyr::rename(started_at = ended_at,
                ended_at = started_at)

neg_trip$trip_duration <- as.numeric(difftime(neg_trip$ended_at, neg_trip$started_at))

#Merge subset with main data set and arrange in ascending order
dataset <- rbind(dataset, neg_trip)
dataset <- dataset %>% 
  arrange(started_at)


#Delete rows with negative ride duration and rides with less than 60 seconds which according to Divvy are user who have cancelled their ride.
#Drop columns start/end_station_id as it will do be used for the analysis.

dataset <- subset(dataset, trip_duration > 59) %>% 
  select(-c("start_station_id", "end_station_id"))


#-------------------------------Add month, day, and day_of_week columns into dataset. 

dataset$month <- format(as.Date(dataset$started_at), "%m")
dataset$day <- format(as.Date(dataset$started_at), "%d")
dataset$day_of_week <- format(as.Date(dataset$started_at), "%A")


#----------------------------Remove white space in every column that has data type as.character.

dataset %>% mutate(across(where(is.character), str_trim))

fwrite(dataset, file = "cyclistic_data_wrangling_process.csv", sep = ",", na = "NA", row.names = FALSE, col.names = TRUE)
write.csv(dataset, file = "cyclistic_data_wrangling_process.csv", sep = ",", na = "NA", row.names = FALSE, col.names = TRUE)
