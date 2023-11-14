fort_waine_distance <- vector(length = 113)
for (i in 1:length(locations$AirPtCd)) {
  fort_waine_distance[i] <- sqrt((40.75 - locations$latitude[i])^2 + 
    (-85.25 - locations$longitude[i])^2)
  
}

distance <- data.frame(Airports = locations$AirPtCd, Eu_dist = fort_waine_distance,
                       City = locations$city)
min_dist <- min(distance$Eu_dist)
min_dist_airport <- distance$Airports[which(distance$Eu_dist == min_dist)]
min_dist_city <- distance$City[which(distance$Eu_dist == min_dist)]

max_dist <- max(distance$Eu_dist)
max_dist_airport <- distance$Airports[which(distance$Eu_dist == max_dist)]
max_dist_city <- distance$City[which(distance$Eu_dist == max_dist)]

dist <- 15
attach(distance)
loc_set_airport <- distance %>% filter(Eu_dist <= dist)
loc_set_airport$Airports


#sec_min_dist <- unique(sort(distance$Eu_dist))[2]

j <- 1
loc_set_list <- list()
loc_value_list <- list()
for (dist in seq(2,35,1)) {
  print(paste(j))
  loc_set_airport <- distance %>% filter(Eu_dist <= dist)
  col_ind <- array(0)
  for (i in 1:length(loc_set_airport$Airports)) {
    temp <- which(loc_set_airport$Airports[i] == colnames(fdata)[2:114]) + 1
    col_ind <- c(col_ind , temp)
  }
  col_ind <- col_ind[-1]
  x_pred <- as.matrix(fdata[1:1129,col_ind])
  y_new <- Response_clean[1:1129]
  set.seed(9)
  # Fit Lasso model with cross-validation
  cvfit <- cv.glmnet(x = x_pred, y = y_new, alpha = 1)  # alpha = 1 for Lasso
  lambda_optimal <- cvfit$lambda.min
  
  # Fit the final Lasso model with the optimal lambda
  lasso_model <- glmnet(x = x_pred, y = y_new, alpha = 1, lambda = lambda_optimal)
  coef <- coef(lasso_model, s = "lambda.min")
  restricted_coef <- which(abs(coef[,1]) > 0.02)
  features <- rownames(coef)[restricted_coef]
  loc_set_list[[dist]] <- features
  loc_value_list[[dist]] <- coef[restricted_coef]
  j <- j+1
}

forecasted_data <- rep(0,30)
for(i in 2:length(loc_set_list)){
  features <- loc_set_list[[i]][-1]
  restricted_coef.values <- loc_value_list[[i]][-1]
  intercept <- loc_value_list[[i]][1]
  col_ind <- array(0)
  for (j in 1:length(features)) {
    temp <- which(features[j] == colnames(fdata)[2:114])+1
    col_ind <- c(col_ind , temp)
  }
  col_ind <- col_ind[-1]
  x_test <- as.matrix(fdata[1130:1159,col_ind])
  y_test <- Response_clean[1130:1159]
  
  y_fitted <- rep(intercept,30)
  for (k in 1:length(col_ind)) {
    y_fitted <- y_fitted + (restricted_coef.values[k] * x_test[,k])
  }
  forecasted_data <- cbind(forecasted_data,y_fitted)
  
}
forecasted_data <- forecasted_data[,-1]
forecasted_data <- as.data.frame(forecasted_data)
forecasted_data <- data.frame(y_test,forecasted_data)

save(forecasted_data, file = "Forecasted_30_days")

rmse <- vector(length = 34)
for (j in 2:(dim(forecasted_data)[2])) {
  rmse[j] <- sqrt(sum((forecasted_data[,1] - forecasted_data[,j])^2))
}
plot(1:length(rmse), rmse, type = "o",col = "cadetblue",lwd = 2,ylab = "RMSE",xlab = "Radius in Distance",
     main = "Plot of Radius Distance Vs. RMSE ")

###################### Accuracy plot #############

plot(1:length(forecasted_data$y_test), forecasted_data$y_test, type = "o", col = "red", lwd = 2, ylim = c(15,40),
     ylab = "Maximum Temperature",xlab = "Day Numbers",main = "Plot of Forecasted Temperature from 03/08/2017 to 01/09/2017 by Lasso and Bubble Method")
lines(accurecy$y_forecasted, col = "blue", type = "o", lwd = 2)
lines(forecasted_data$y_fitted, col = "green", type = "o", lwd = 2)
legend("topleft",legend = c("Actual","Forecasted_lasso","Forecasted_bubble"),
       col = c("red", "blue","green"), pch  = c(19,19), cex =c(1.2,1.2))

######################## influencing airport plot ##################

library(ggplot2)
features <- loc_set_list[[2]][-1]
locations_sub_1 <- cbind(locations %>% filter(AirPtCd %in% features),coef = loc_value_list[[2]][-1])

ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = temp), size = 2.3) + 
  labs(title = "Plot of Maximum Temperature of USA on 1st September", x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.key.size = unit(0.4, "in")) + 
  scale_color_viridis(name = "Maximum Temperture", option = "inferno")+
  geom_point(x = -85.25, y = 40.75, color = "red",size = 1.5)+
  geom_point(data = locations_sub_1, aes(x = longitude, y = latitude),
             color = "cyan", size = 1.5)



