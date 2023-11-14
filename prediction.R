x_predictor_modelling <- as.matrix(fdata[1:1129,2:114])
Response_modelling <- Response_clean[1:1129]


set.seed(9)
cvfit <- cv.glmnet(x = x_predictor_modelling, y = Response_modelling, alpha = 1)  # alpha = 1 for Lasso

# Plot the cross-validated error as a function of log(lambda)
plot(cvfit)


lambda_optimal <- cvfit$lambda.min

# Fit the final Lasso model with the optimal lambda
lasso_model <- glmnet(x = x_predictor_modelling, y = Response_modelling, alpha = 1, lambda = lambda_optimal)

coef <- coef(lasso_model, s = "lambda.min")

restricted_coef <- which(abs(coef[,1]) > 0.02)
features <- rownames(coef)[restricted_coef]
features <- features[-1]
restricted_coef.values <- coef[restricted_coef]
intercept <- restricted_coef.values[1]
restricted_coef.values <- restricted_coef.values[-1]

se <- sqrt(diag(cvfit$cvm))
print(se)

############# plot part ############

locations_sub_1 <- cbind(locations %>% filter(AirPtCd %in% features),coef = restricted_coef.values)

save(locations_sub_1,file = "fort_waine_lasso_forecast_coef.Rdata")


ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = temp), size = 2.3) + 
  labs(title = "Plot of Maximum Temperature of USA on 1st September,2017", x = "Longitude", y = "Latitude") + 
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




############### prediction part ######################


col_ind <- array(0)
for (i in 1:length(features)) {
  temp <- which(features[i] == colnames(fdata)[2:114]) + 1
  col_ind <- c(col_ind , temp)
}
col_ind <- col_ind[-1]
x_new <- as.matrix(fdata[1130:1159,col_ind])
y_new <- Response_clean[1130:1159]

y_fitted <- rep(intercept,30)
for (i in 1:length(features)) {
  y_fitted <- y_fitted + (restricted_coef.values[i] * x_new[,i])
}


#predictions <- predict(lasso_model, newx = x_new)

accurecy <- data.frame(y_new, y_forecasted = y_fitted)
save(accurecy, file = "Accurecy.Rdata")


########### line diagram oart ##########

plot(1:length(accurecy$y_new), accurecy$y_new, type = "o", col = "red", lwd = 2,ylim = c(0,40),
     ylab = "Maximum Tempertaure",xlab = "Number of Days",main = "Line Diagram of Actual Vs Forecasted Maximum Temperature Near Fort Waine")
lines(accurecy$y_forecasted, col = "blue", type = "o", lwd = 2)
legend("bottomright",legend = c("Actual","Forecasted"),
       col = c("red", "blue"), pch  = c(19,19), cex =c(1.2,1.2))


