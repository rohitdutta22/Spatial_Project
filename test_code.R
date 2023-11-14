setwd("C:/Users/HP/Dropbox/My PC (LAPTOP-S7638K91)/Desktop/3rd_Sem_IITK/Arnab_Hazra/Spatial_project")

################load the 2014 data ############

library(ncdf4)
maxtemp2014 <- nc_open("tmax.2014.nc")
v1 <- maxtemp2014$var[[1]]


data_array_2014 <- ncvar_get(maxtemp2014, v1)

temp <- maxtemp2014$var$tmax$dim[[3]]$vals
lats <- maxtemp2014$var$tmax$dim[[2]]$vals
lons <- maxtemp2014$var$tmax$dim[[1]]$vals

library(plot3D)
S <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S[ , 1] > 180)
S[keep, 1] <- -(360 - S[keep, 1])

#library(plot3D)
#scatter2D(x = S[ , 1], y = S[ ,2], colvar = c(data_array[ , , 200]))

library(maps)

cc <- map.where(x = S[ , 1], y = S[ , 2])

keep2 <- which(cc == "USA")

S.US <- S[keep2, ]
Y.US <- c(data_array[ , , 360])[keep2]

dat3 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
attach(dat3)
#dat3[which(lat == 40.75), which(long == -85.25),]
dat3 %>% 
  filter(lat == 40.75,long == -85.25)

library(plot3D)
scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)

for(i in 1:length(locations$longitude)){
  points(x = locations$longitude[i], y = locations$latitude[i], col = "magenta", pch = 16)
}

####### making data frame for 2014 max temp data near fort wain grid ############

temp_2014 <- array(0)
for (i in 1: 365){

  Y.US <- c(data_array[ , , i])[keep2]
  
  dat3 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat3 %>% 
    filter(lat == 40.75,long == -85.25)
  temp_2014 <- c(temp_2014, foo$temp)
  print(paste(i))
  
}

temp_2014 <- temp_2014[-1]

dat_temp_2014 <- data.frame(max_temp = temp_2014)
save(dat_temp_2014, file = "dat_2014.Rdata")

#################################



############# making 2015 dataframe ############

library(ncdf4)
maxtemp2015 <- nc_open("tmax.2015.nc")
v1 <- maxtemp2015$var[[1]]


data_array_2015 <- ncvar_get(maxtemp2015, v1)

temp <- maxtemp2015$var$tmax$dim[[3]]$vals
lats <- maxtemp2015$var$tmax$dim[[2]]$vals
lons <- maxtemp2015$var$tmax$dim[[1]]$vals

library(plot3D)
S_2015 <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S_2015[ , 1] > 180)
S_2015[keep, 1] <- -(360 - S_2015[keep, 1])

library(maps)

cc <- map.where(x = S_2015[ , 1], y = S_2015[ , 2])

keep2 <- which(cc == "USA")

S.US <- S_2015[keep2, ]
Y.US <- c(data_array_2015[ , , 360])[keep2]

dat6 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
attach(dat6)
#dat3[which(lat == 40.75), which(long == -85.25),]
dat6 %>% 
  filter(lat == 40.75,long == -85.25)

library(plot3D)
scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)


####### making data frame for 2015 max temp data near fort wain grid ############

temp_2015 <- array(0)
for (i in 1: 365){
  
  Y.US <- c(data_array_2015[ , , i])[keep2]
  
  dat6 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat6 %>% 
    filter(lat == 40.75,long == -85.25)
  temp_2015 <- c(temp_2015, foo$temp)
  print(paste(i))
  
}

temp_2015 <- temp_2015[-1]

dat_temp_2015 <- data.frame(max_temp = temp_2015)
save(dat_temp_2015, file = "dat_2015.Rdata")


############# making 2016 dataframe ############

library(ncdf4)
maxtemp2016 <- nc_open("tmax.2016.nc")
v1 <- maxtemp2016$var[[1]]


data_array_2016 <- ncvar_get(maxtemp2016, v1)

temp <- maxtemp2016$var$tmax$dim[[3]]$vals
lats <- maxtemp2016$var$tmax$dim[[2]]$vals
lons <- maxtemp2016$var$tmax$dim[[1]]$vals

library(plot3D)
S_2016 <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S_2016[ , 1] > 180)
S_2016[keep, 1] <- -(360 - S_2016[keep, 1])

library(maps)

cc <- map.where(x = S_2016[ , 1], y = S_2016[ , 2])

keep2 <- which(cc == "USA")

S.US <- S_2016[keep2, ]
Y.US <- c(data_array_2016[ , , 360])[keep2]

dat4 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
attach(dat4)
#dat3[which(lat == 40.75), which(long == -85.25),]
dat4 %>% 
  filter(lat == 40.75,long == -85.25)

library(plot3D)
scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)


####### making data frame for 2016 max temp data near fort wain grid ############

temp_2016 <- array(0)
for (i in 1: 366){
  
  Y.US <- c(data_array_2016[ , , i])[keep2]
  
  dat4 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat4 %>% 
    filter(lat == 40.75,long == -85.25)
  temp_2016 <- c(temp_2016, foo$temp)
  print(paste(i))
  
}

temp_2016 <- temp_2016[-1]

dat_temp_2016 <- data.frame(max_temp = temp_2016)
save(dat_temp_2016, file = "dat_2016.Rdata")

############# making 2017 dataframe ############

library(ncdf4)
maxtemp2017 <- nc_open("tmax.2017.nc")
v1 <- maxtemp2017$var[[1]]


data_array_2017 <- ncvar_get(maxtemp2017, v1)

temp <- maxtemp2017$var$tmax$dim[[3]]$vals
lats <- maxtemp2017$var$tmax$dim[[2]]$vals
lons <- maxtemp2017$var$tmax$dim[[1]]$vals

library(plot3D)
S_2017 <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S_2017[ , 1] > 180)
S_2017[keep, 1] <- -(360 - S_2017[keep, 1])

library(maps)

cc <- map.where(x = S_2017[ , 1], y = S_2017[ , 2])

keep2 <- which(cc == "USA")

S.US <- S_2017[keep2, ]
Y.US <- c(data_array_2017[ , , 360])[keep2]

dat5 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
attach(dat5)
#dat3[which(lat == 40.75), which(long == -85.25),]
dat5 %>% 
  filter(lat == 40.75,long == -85.25)

library(plot3D)
scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)


####### making data frame for 2017 max temp data near fort wain grid ############

temp_2017 <- array(0)
for (i in 1: 365){
  
  Y.US <- c(data_array_2017[ , , i])[keep2]
  
  dat5 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat5 %>% 
    filter(lat == 40.75,long == -85.25)
  temp_2017 <- c(temp_2017, foo$temp)
  print(paste(i))
  
}

temp_2017 <- temp_2017[-1]

dat_temp_2017 <- data.frame(max_temp = temp_2017)
save(dat_temp_2017, file = "dat_2017.Rdata")



########## This part is done###################
####### cleaning the data expo data set ###############


dat <- read.csv("histWeather.csv")
length(table(dat$AirPtCd))
dat_KBHB <- dat[dat$AirPtCd == "KBHB",]

dat_KBHB <- dat %>% filter(AirPtCd == "KBHB") 
dim(dat_KBHB)

dat_KACY <- dat %>% filter(AirPtCd == "KACY")
fun <- right_join(dat_KBHB,dat_KACY,by = "Date")
fun_1 <- data.frame(fun$Date, fun$Max_TemperatureF.x,fun$Max_TemperatureF.y)

which(unique(dat$AirPtCd) == "KACY")
which(unique(dat$AirPtCd) == "KLWS")

ap <- unique(dat$AirPtCd)[-c(12,97)]

maxt <- data.frame(date = dat_KACY$Date, KACY = dat_KACY$Max_TemperatureF)
for (i in ap) {
  data <- dat %>% filter(AirPtCd == i)
  te <- right_join(data,dat_KACY,by = "Date")
  maxt <- data.frame(maxt,i = (5*(te[,2] - 32)/9))
  print(paste0(i))
}
colnames(maxt) <- c("max_temp","KACY",ap)
maxt$KACY <- (5*(maxt$KACY - 32)/9)




dat_KLWS <- dat %>% filter(AirPtCd == "KLWS")
a <- 916:1281
dat_KLWS <- dat_KLWS[-a,]
maxt <- data.frame(maxt,KLWS = (5*(dat_KLWS$Max_TemperatureF - 32)/9))

save(maxt, file = "maxt.Rdata")





############### data set after missing imputation ##############3

fdata <- read.csv("final_data_expo.csv") ## missing imputed data using KNN algo
fdata <- data.frame(Date = maxt$max_temp,fdata)
save(fdata, file = "final_data_expo.Rdata")


