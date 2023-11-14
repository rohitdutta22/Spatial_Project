
dat_temp_2014_clean <- dat_temp_2014$max_temp[-seq(1,181,1)]
dat_temp_2017_clean <- dat_temp_2017$max_temp[seq(1,244,1)]


########### Run from here from the next time ###############
## Load fdata.rdata, response.rdata  #############

Response_clean <- c(dat_temp_2014_clean,dat_temp_2015$max_temp,dat_temp_2016$max_temp,
                    dat_temp_2017_clean) 
x_predictor <- as.matrix(fdata[,2:114])

set.seed(9)
library(glmnet)
# Fit Lasso model with cross-validation
cvfit <- cv.glmnet(x = x_predictor, y = Response_clean, alpha = 1)  # alpha = 1 for Lasso

# Plot the cross-validated error as a function of log(lambda)
plot(cvfit)


lambda_optimal <- cvfit$lambda.min

# Fit the final Lasso model with the optimal lambda
lasso_model <- glmnet(x = x_predictor, y = Response_clean, alpha = 1, lambda = lambda_optimal)

coef <- coef(lasso_model, s = "lambda.min")

nonzero_coef <- which(abs(coef[,1]) > 0.02)
features <- rownames(coef)[nonzero_coef]
features <- features[-1]



########### mapping part #######

locations <- read.csv("locations.csv")
attach(locations)
#final_loc <- locations %>% filter(AirPtCd == features)

loc_index <- numeric(length = length(features))
locations_sub <- cbind(locations %>% filter(AirPtCd %in% features),coef = coef[nonzero_coef])

save(locations_sub,file = "fort_waine_lasso_final_coef.Rdata")

################## Drawing the US map #######################
points(x = -114.25, y = 40.25, pch = 19)

new_loc = locations[-c(112,113),]
library(ggplot2)
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
  geom_point(data = locations_sub, aes(x = longitude, y = latitude),
             color = "cyan", size = 1.5)

 

