install.packages("ff")
library(ff)
rm(list = ls())

#To read the large data in  r 
#uses lazy loading of data
#data_yellowJune <- ff::read.csv.ffdf(source(file.choose()))


#find the boundary latitudes and longitudes
latRT=40.902414
longRT=-73.689511

latLT=40.902414
longLT=-74.052060

latLD=40.557954
longLD=-74.052060

latRD=40.557954
longRD=-73.689511

#Corners of the New york city
#leftDown latlong
#40.557954, -74.052060

#leftTop 
#40.902414, -74.052060

#rightTop
#40.902414, -73.689511

#right down
#40.557954, -73.689511

#add them into Data frame to save in csv for future reference
NewyorkCornersdf <- data.frame()
NewyorkCornersdf[1,"Latitude"] <-latLD#longLD) 
NewyorkCornersdf[1,"Longitude"] <-longLD 
NewyorkCornersdf <-rbind(NewyorkCornersdf,c(latRD,longRD),c(latLT,longLT),c(latRT,longRT)) 

#save into csv file
setwd("E:\\2016-12-1")
write.csv(x = NewyorkCornersdf,file ="NewyorkCorners.csv" )

# Calculate distance in Meters between two points
# reference https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
earth.dist <- function (long1, lat1, long2, lat2)
{
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d*1000)
}


x <- earth.dist(longLT,latLT,longLD,latLD)/50

y <- earth.dist(longLT,latLT,longRT,latRT)/50

x*y


c(latLT-latLD)/0.0005

c(longLT-longRT)/0.0005

#corners used for loop to create the Zones
startLat=latLT
startLong=longLT
stopLat=latRD
stopLong=longRD

#Data frame to store the Zones four corners lat and long 
zonesDf <- data.frame(zoneId=character(),latLT=double(),longLT=double(),latRT=double(),longRT=double(),latLD=double(),longLD=double(),latRD=double(),longRD=double())
zonesDf$zoneId<- as.character()

# str(zonesDf)
#0.0001 => 11.132mts 
decimal50mts <- 0.0005
j=1
fileexists<- 0
existingData <- data.frame()
rownumber <-1
#while loop start
while(startLat-j*decimal50mts>stopLat){
i=1
while(startLong+i*decimal50mts < stopLong){
zonesDf[rownumber,1] <- paste(j,i,sep = "_")
zonesDf$latLT[rownumber] <- startLat    - (j-1) *decimal50mts
zonesDf$longLT[rownumber] <- startLong  + (i-1) *decimal50mts

zonesDf$latLD[rownumber] <- startLat - j*decimal50mts 
zonesDf$longLD[rownumber] <- startLong + (i-1) *decimal50mts

zonesDf$latRT[rownumber] <- startLat -(j-1) *decimal50mts
zonesDf$longRT[rownumber] <- startLong+i*decimal50mts

zonesDf$latRD[rownumber] <- startLat- j*decimal50mts
zonesDf$longRD[rownumber] <- startLong + i*decimal50mts
i=i+1
rownumber=rownumber+1
if(rownumber%%15001==1){
  
  if(fileexists==0){
    write.csv(x=zonesDf,file="Zones.csv",row.names = FALSE)
  }else{
    existingData <- read.csv("Zones.csv")
    write.csv(x=rbind(existingData,zonesDf),file="Zones.csv",row.names = FALSE)
  }
  fileexists<-1
  existingData <- NULL
  zonesDf <-NULL
  zonesDf <- data.frame(zoneId=character(),latLT=double(),longLT=double(),latRT=double(),longRT=double(),latLD=double(),longLD=double(),latRD=double(),longRD=double())
  zonesDf$zoneId<- as.character()
  rownumber=1
}

}
print(j) 
j=j+1
}
-74.0397+100*0.0005
existingData <- read.csv("Zones.csv")
write.csv(x=rbind(existingData,zonesDf),file="Zones.csv",row.names = FALSE)

#rm(zonesDf1)

#head(zonesDf)
#head(existingData)

# zonesDf <- zonesDf[!is.na(zonesDf$zoneId), ]
# zonesDf1 <- read.csv("Zones.csv")
# zonesDf1 <- zonesDf1[!is.na(zonesDf1$zoneId), ]
nrow(existingData[existingData$latLT<= startLat  && existingData$latLT>= stopLat   ,])
max(existingData$latLD)
max(existingData$longLD)

head(existingData)
