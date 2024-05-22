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
pedestrian <-read_delim("pedestrian.csv")

head(pedestrian)
str(pedestrian)
summary(pedestrian)

pedestrian |>
  ggplot(aes(E, N, colour= factor({TrajID})) )+
  geom_path() +
  geom_point() +
  coord_equal() +
  facet_wrap(vars(TrajID))+
  ggtitle("Visual comparison of the 6 trajectories", subtitle = "Each subplot highlights a trajectory")+
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

##Task 6 ----
#install.packages("SimilarityMeasures")
library("SimilarityMeasures")
set.seed(796)

help(package = "SimilarityMeasures")

dftraj1<-pedestrian[pedestrian$TrajID == 1,]
dftraj2<-pedestrian[pedestrian$TrajID == 2,]
dftraj3<-pedestrian[pedestrian$TrajID == 3,]
dftraj4<-pedestrian[pedestrian$TrajID == 4,]
dftraj5<-pedestrian[pedestrian$TrajID == 5,]
dftraj6<-pedestrian[pedestrian$TrajID == 6,]

dftraj1<-dftraj1[,2:3]
dftraj2<-dftraj2[,2:3]
dftraj3<-dftraj3[,2:3]
dftraj4<-dftraj4[,2:3]
dftraj5<-dftraj5[,2:3]
dftraj6<-dftraj6[,2:3]

##Make  Matrices
traj1<-data.matrix(dftraj1)
traj2<-data.matrix(dftraj2)
traj3<-data.matrix(dftraj3)
traj4<-data.matrix(dftraj4)
traj5<-data.matrix(dftraj5)
traj6<-data.matrix(dftraj6)

TrajCheck(traj1, traj2)
TrajCheck(traj1, traj3)
TrajCheck(traj1, traj4)
TrajCheck(traj1, traj5)
TrajCheck(traj1, traj6)# all fine

#trajectories i percieve to be most similar:1 and 6

#trajectories i percieve to be most dissimilar:2 and 4, although i can't really compare them. but 1 and 4 also seem quite dissimilar

dtw12 <-DTW(traj1,traj2)
dtw13 <-DTW(traj1,traj3)
dtw14 <-DTW(traj1,traj4)
dtw15 <-DTW(traj1,traj5)
dtw16 <-DTW(traj1,traj6)

ed12 <-EditDist(traj1,traj2)
ed13 <-EditDist(traj1,traj3)
ed14 <-EditDist(traj1,traj4)
ed15 <-EditDist(traj1,traj5)
ed16 <-EditDist(traj1,traj6)

fr12 <-Frechet(traj1,traj2)
fr13 <-Frechet(traj1,traj3)
fr14 <-Frechet(traj1,traj4)
fr15 <-Frechet(traj1,traj5)
fr16 <-Frechet(traj1,traj6)

lc12 <-LCSS(traj1,traj2, pointSpacing = 1, errorMarg=3)
lc13 <-LCSS(traj1,traj3, pointSpacing = 1, errorMarg=3)
lc14 <-LCSS(traj1,traj4, pointSpacing = 1, errorMarg=3)
lc15 <-LCSS(traj1,traj5, pointSpacing = 1, errorMarg=3)
lc16 <-LCSS(traj1,traj6, pointSpacing = 1, errorMarg=3)

DTW<-rbind(dtw12,dtw13,dtw14,dtw15,dtw16)
ed<-rbind(ed12,ed13,ed14,ed15,ed16)
fr<-rbind(fr12,fr13,fr14,fr15,fr16)
lc<- rbind(lc12,lc13,lc14,lc15,lc16)

metrics <-rbind(DTW,ed,fr,lc)
names <- c("DTW","Edit_Dist","Frechet","LCSS")
namecol <-rep(names, each=5)
values <-rep(c("2","3","4","5","6"), times= 4)
results<-data.frame(cbind(metrics,namecol, values))

results$V1<-as.numeric(results$V1)

results |>
  ggplot(aes(values,V1, fill=values)) +
  geom_col()+
  facet_wrap(vars(namecol), scales = "free_y")+
  ggtitle("Computed similarities using different measures between trajectory 1 and all other trajectories")+
  theme(axis.line=element_line())+
  ylab("Value")+
  xlab("Comparison trajectory")

#my results show similar results to those from the pt-github page, but the LCSS for trajectory 6 was lots longer than in the example due to me choosing the error margin quite big.