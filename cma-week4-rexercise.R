#Exercise 4 ----
library("jsonlite")
library("readr")
library("sf")
library("dplyr")
library("tmap")
library("lubridate")
library("stringr")
library("tidyr")
library("ggplot2")
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

filterdays <-Sarah_Wirth_geo_CH |> filter(Datetime >= "2024-03-26 05:08:43", Datetime < "2024-04-06 00:00:00")#as i only have a small amount of timestamps per day i choose to use data from the 26.03 and the 06.04 which contain overall 58 timestamps

##Task 1 ----
#as i do not have a fixed sampling rate and my google-data have not picked up many timestamps, and not many twice. Therefore i'll just use it to get rid of the cases where it couldn't localise me precisely, when i was at home.

distance_by_element <- function(later, now) {
  as.numeric(
    st_distance(later, now, by_element = TRUE)
  )
}

filterdays<- filterdays |>
  mutate( 
    nMinus1 = distance_by_element(lag(geometry, 1), geometry),  
    nPlus1  = distance_by_element(geometry, lead(geometry, 1)) 
    )

filterdays <- filterdays |>
  rowwise() |>
  mutate(
    stepMean = mean(c(nMinus1, nPlus1))
  ) |>
  ungroup()

##Task 2 ----
hist(filterdays$stepMean, breaks=10)#lots of small movements and some big ones
small_distances<- filter(filterdays, stepMean<5000)
hist(small_distances$stepMean)#lots below 500
smaller_distances<-filter(filterdays, stepMean<500)
hist(smaller_distances$stepMean)#9 below 100
summary(filterdays$stepMean)#mean is 8170.36
#my new threshold is 100

filterdays <- filterdays |>
  mutate(static = stepMean < 100)
filterdays <- filterdays |>  drop_na(stepMean)

new_filterdays <- filterdays |>
  filter(!static)

##Task 3 ----
filterdays |>
  ggplot(aes(X, Y,colour=static)) +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom")

##Task 4 ----
rle_id <- function(vec) {
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times = x))
}

filterdays <- filterdays |>
  mutate(segment_id = rle_id(static))#7 segments

filterdays |>
  ggplot(aes(X, Y,colour=segment_id)) +
  geom_path() +
  geom_point() +
  coord_equal() +
  theme(legend.position = "bottom")

##Task 5 ----

##Task 6 ----
