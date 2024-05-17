#Exercise 4 ----
library("jsonlite")
library("readr")
library("sf")
library("dplyr")
library("tmap")
library("lubridate")
library("stringr")
library("tidyr")
##Preparation ----

Sarah_Wirth_Data<-fromJSON("location-history.json")

head(Sarah_Wirth_Data)
str(Sarah_Wirth_Data)

###Extract coordinates----
Sarah_Wirth_Data$startplace_n <-substring(Sarah_Wirth_Data$activity$start,5, 13)
Sarah_Wirth_Data$startplace_e <-substring(Sarah_Wirth_Data$activity$start,15, 22)
Sarah_Wirth_Data$endplace_n <-substring(Sarah_Wirth_Data$activity$end,5, 13)
Sarah_Wirth_Data$endplace_e <-substring(Sarah_Wirth_Data$activity$end,15, 22)

Sarah_Wirth_Data <-Sarah_Wirth_Data[!(Sarah_Wirth_Data$startplace_n %in% NA),]#drop rows without coordinates

###Extract time ----
Sarah_Wirth_Data$tz <-str_sub(Sarah_Wirth_Data$endTime, -5,-1)#get timezones, as the data has different timezones (+01, +02) for reasons
Sarah_Wirth_Data <-Sarah_Wirth_Data[!(Sarah_Wirth_Data$tz %in% ".000Z"),]#drop visits, which  have the "timezone" .000Z
#still ignoring the timezone issues

Sarah_Wirth_Data$startTime <- as.POSIXct(Sarah_Wirth_Data$startTime, format="%Y-%m-%dT%H:%M:%OS")#Format start and endtime while losing miliseconds

Sarah_Wirth_Data$endTime <- as.POSIXct(Sarah_Wirth_Data$endTime, format="%Y-%m-%dT%H:%M:%OS")#Format start and endtime while losing miliseconds

Sarah_Wirth_Data <-Sarah_Wirth_Data |> 
  select(endTime,startTime,startplace_n,startplace_e,endplace_n,endplace_e, tz)#drop every row which isn't a time or a coordinate or tz

Sarah_Wirth_Start <-Sarah_Wirth_Data |> 
  select(startTime,startplace_n,startplace_e)
colnames(Sarah_Wirth_Start) <- c("Datetime","N", "E")

Sarah_Wirth_End <-Sarah_Wirth_Data |> 
  select(endTime,endplace_n,endplace_e)
colnames(Sarah_Wirth_End) <- c("Datetime","N", "E")

Sarah_Wirth_fixed <-rbind(Sarah_Wirth_Start,Sarah_Wirth_End)

Sarah_Wirth_fixed <- Sarah_Wirth_fixed |> 
  arrange(Datetime)

row.names(Sarah_Wirth_fixed)<-NULL#fix row names

Sarah_Wirth_geo_WGS84 <- Sarah_Wirth_fixed |>
  st_as_sf(coords = c("E", "N"), crs = 4326, remove = FALSE)

Sarah_Wirth_geo_CH <- st_transform(Sarah_Wirth_geo_WGS84, 2056)

Sarah_Wirth_geo_CH<-cbind(Sarah_Wirth_geo_CH,st_coordinates(Sarah_Wirth_geo_CH))

Sarah_Wirth_geo_CH |>
  ggplot(aes(X, Y)) +
  geom_path() +
  geom_point() +
  coord_fixed() +
  theme(legend.position = "bottom")
View(Sarah_Wirth_geo_CH)

filterdays <-Sarah_Wirth_geo_CH |> filter(Datetime >= "2024-03-26 05:08:43", Datetime < "2024-03-28 00:00:00")#as i only have a small amount of timestamps per day i choose to use data from the 26.03 and the 27.03 which contain overall 24 timestamps

##Task 1 ----

##Task 2 ----

##Task 3 ----

##Task 4 ----

##Task 5 ----

##Task 6 ----
