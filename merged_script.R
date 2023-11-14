################load the 2014 data ############

library(ncdf4)
maxtemp2014 <- nc_open("tmax.2014.nc")
v1 <- maxtemp2014$var[[1]]


data_array <- ncvar_get(maxtemp2014, v1)

temp <- maxtemp2014$var$tmax$dim[[3]]$vals
lats <- maxtemp2014$var$tmax$dim[[2]]$vals
lons <- maxtemp2014$var$tmax$dim[[1]]$vals

#library(plot3D)
S <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S[ , 1] > 180)
S[keep, 1] <- -(360 - S[keep, 1])

#library(plot3D)
#scatter2D(x = S[ , 1], y = S[ ,2], colvar = c(data_array[ , , 200]))

library(maps)

cc <- map.where(x = S[ , 1], y = S[ , 2])

keep2 <- which(cc == "USA")

S.US <- S[keep2, ]
#Y.US <- c(data_array[ , , 360])[keep2]

#dat3 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
#attach(dat3)
#dat3[which(lat == 40.25), which(long == -114.25),]
#dat3 %>% 
#  filter(lat == 40.25,long == -114.25)

#library(plot3D)
#scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)



############# making 2015 dataframe ############


maxtemp2015 <- nc_open("tmax.2015.nc")
v1 <- maxtemp2015$var[[1]]


data_array_2015 <- ncvar_get(maxtemp2015, v1)

temp <- maxtemp2015$var$tmax$dim[[3]]$vals
lats <- maxtemp2015$var$tmax$dim[[2]]$vals
lons <- maxtemp2015$var$tmax$dim[[1]]$vals

#library(plot3D)
S_2015 <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S_2015[ , 1] > 180)
S_2015[keep, 1] <- -(360 - S_2015[keep, 1])

library(maps)

cc <- map.where(x = S_2015[ , 1], y = S_2015[ , 2])

keep2 <- which(cc == "USA")

S.US <- S_2015[keep2, ]
#Y.US <- c(data_array_2015[ , , 360])[keep2]

#dat6 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
#attach(dat6)
#dat3[which(lat == 40.25), which(long == -114.25),]
#dat6 %>% 
#  filter(lat == 40.25,long == -114.25)

#library(plot3D)
#scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)


############# making 2016 dataframe ############

library(ncdf4)
maxtemp2016 <- nc_open("tmax.2016.nc")
v1 <- maxtemp2016$var[[1]]


data_array_2016 <- ncvar_get(maxtemp2016, v1)

temp <- maxtemp2016$var$tmax$dim[[3]]$vals
lats <- maxtemp2016$var$tmax$dim[[2]]$vals
lons <- maxtemp2016$var$tmax$dim[[1]]$vals

#library(plot3D)
S_2016 <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S_2016[ , 1] > 180)
S_2016[keep, 1] <- -(360 - S_2016[keep, 1])

library(maps)

cc <- map.where(x = S_2016[ , 1], y = S_2016[ , 2])

keep2 <- which(cc == "USA")

S.US <- S_2016[keep2, ]
#Y.US <- c(data_array_2016[ , , 360])[keep2]

#dat4 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
attach(dat4)
#dat3[which(lat == 40.25), which(long == -114.25),]
#dat4 %>% 
#  filter(lat == 40.25,long == -114.25)

#library(plot3D)
#scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)





############# making 2017 dataframe ############

library(ncdf4)
maxtemp2017 <- nc_open("tmax.2017.nc")
v1 <- maxtemp2017$var[[1]]


data_array_2017 <- ncvar_get(maxtemp2017, v1)

temp <- maxtemp2017$var$tmax$dim[[3]]$vals
lats <- maxtemp2017$var$tmax$dim[[2]]$vals
lons <- maxtemp2017$var$tmax$dim[[1]]$vals

#library(plot3D)
S_2017 <- as.matrix(expand.grid(x = lons, y = lats))

keep <- which(S_2017[ , 1] > 180)
S_2017[keep, 1] <- -(360 - S_2017[keep, 1])

library(maps)

cc <- map.where(x = S_2017[ , 1], y = S_2017[ , 2])

keep2 <- which(cc == "USA")

S.US <- S_2017[keep2, ]
#Y.US <- c(data_array_2017[ , , 360])[keep2]

#dat5 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
library(dplyr)
attach(dat5)
#dat3[which(lat == 40.25), which(long == -114.25),]
#dat5 %>% 
#  filter(lat == 40.25,long == -114.25)

#library(plot3D)
#scatter2D(x = S.US[ , 1], y = S.US[ ,2], colvar = Y.US, pch = 15)





########################### for loop ###############################
library(glmnet)
library(dplyr)
library(maps)
library(plot3D)

k = 1

#forecasted_y_one_day <- array(0)

# 948, 1048, 1049, 1050, 1051, 1145, 1146, 1148, 1149, 1150, 1151
# 1247, 1248, 1249, 1250, 1251, 1253, 1355, 1356, 1357, 1358, 1359
# 1360, 1457, 1459, 1460, 1461, 1462

for(k in 1706:dim(S.US)[1]){     ## loop for k starts

print(paste("Grid started",k))

temp_2014 <- array(0)
for (i in 1: 365){
  
  Y.US <- c(data_array[ , , i])[keep2]
  
  dat3 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat3 %>% 
    filter(lat == S.US[k,2],long == S.US[k,1])
  temp_2014 <- c(temp_2014, foo$temp)
  #print(paste(i))
  
}

temp_2014 <- temp_2014[-1]

dat_temp_2014 <- data.frame(max_temp = temp_2014)
#save(dat_temp_2014, file = "dat_2014.Rdata")




temp_2015 <- array(0)
for (i in 1: 365){
  
  Y.US <- c(data_array_2015[ , , i])[keep2]
  
  dat6 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat6 %>% 
    filter(lat == S.US[k,2],long == S.US[k,1])
  temp_2015 <- c(temp_2015, foo$temp)
  #print(paste(i))
  
}

temp_2015 <- temp_2015[-1]

dat_temp_2015 <- data.frame(max_temp = temp_2015)
#save(dat_temp_2015, file = "dat_2015.Rdata")


temp_2016 <- array(0)
for (i in 1: 366){
  
  Y.US <- c(data_array_2016[ , , i])[keep2]
  
  dat4 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat4 %>% 
    filter(lat == S.US[k,2],long == S.US[k,1])
  temp_2016 <- c(temp_2016, foo$temp)
 # print(paste(i))
  
}

temp_2016 <- temp_2016[-1]

dat_temp_2016 <- data.frame(max_temp = temp_2016)
#save(dat_temp_2016, file = "dat_2016.Rdata")



temp_2017 <- array(0)
for (i in 1: 365){
  
  Y.US <- c(data_array_2017[ , , i])[keep2]
  
  dat5 <- data.frame(lat = S.US[,2] , long = S.US[,1],temp = Y.US)
  foo <- dat5 %>% 
    filter(lat == S.US[k,2],long == S.US[k,1])
  temp_2017 <- c(temp_2017, foo$temp)
 # print(paste(i))
  
}

temp_2017 <- temp_2017[-1]

dat_temp_2017 <- data.frame(max_temp = temp_2017)
#save(dat_temp_2017, file = "dat_2017.Rdata")


print(paste("Data formation done for this grid"))

########### modelling script starts ############

dat_temp_2014_clean <- dat_temp_2014$max_temp[-seq(1,181,1)]
dat_temp_2017_clean <- dat_temp_2017$max_temp[seq(1,244,1)]

Response_clean <- c(dat_temp_2014_clean,dat_temp_2015$max_temp,dat_temp_2016$max_temp,
                    dat_temp_2017_clean) 
x_predictor <- as.matrix(fdata[,2:114])

############## train test split #############33

x_predictor_modelling <- as.matrix(fdata[1:1158,2:114])
Response_modelling <- Response_clean[1:1158]



################## cross validation checking #################
set.seed(9)
cvfit <- cv.glmnet(x = x_predictor_modelling, y = Response_modelling, alpha = 1)  # alpha = 1 for Lasso

lambda_optimal <- cvfit$lambda.min

# Fit the final Lasso model with the optimal lambda
lasso_model <- glmnet(x = x_predictor_modelling, y = Response_modelling, alpha = 1, lambda = lambda_optimal)

coef <- lasso_model$beta

restricted_coef <- which(abs(coef[,1]) != 0)
features <- rownames(coef)[restricted_coef]


restricted_coef.values <- coef[restricted_coef]


########################## prediction ##########################
print(paste("Prediction starts"))


col_ind <- array(0)
for (i in 1:length(features)) {
  temp <- which(features[i] == colnames(fdata)[2:114]) + 1
  col_ind <- c(col_ind , temp)
}
col_ind <- col_ind[-1]

########## test data ############
x_new <- as.matrix(fdata[1159,col_ind])
y_new <- Response_clean[1159]


y_fitted <- numeric(length = 1)
for (i in 1:length(features)) {
  y_fitted <- y_fitted + (restricted_coef.values[i] * x_new[,i])
}


forecasted_y_one_day <- c(forecasted_y_one_day, y_fitted)

print(paste("Grid ended",k))


}   ## loop for k ends

y_fitted <- 0


forecasted_y_one_day
length(forecasted_y_one_day)


forecasted_y_one_day[1560]
forecasted_y_one_day[1561] <- forecasted_y_one_day[1560]

which(forecasted_y_one_day == 0)

save(forecasted_y_one_day, file = "forecasted_1st_sept.Rdata")




