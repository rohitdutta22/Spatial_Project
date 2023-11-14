library(ggplot2)
library(viridis)
p1 <- ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = temp), size = 2.3) + 
  labs(title = "Plot of Maximum Temperature of USA on 1st September,2017", x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.key.size = unit(0.4, "in")) + 
  scale_color_viridis(name = "Maximum Temperture", option = "inferno")

new_loc = locations[-c(112,113),]


p2 <- ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = temp), size = 2.3) + 
  labs(title = "Plot of Maximum Temperature of USA on 1st September,2017", x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.key.size = unit(0.4, "in")) + 
  scale_color_viridis(name = "Maximum Temperture", option = "inferno")+
  geom_point(data = new_loc, aes(x = longitude, y = latitude),
             color = "cyan", size = 1.5)



p4 <- ggplot(data = dat5, aes(x = long, y = lat))+
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
  geom_point(data = locations_sub, aes(x = longitude, y = latitude),
             color = "cyan", size = 1.5)

p5 <- ggplot(data = dat5, aes(x = long, y = lat))+
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


#p6

plot(1:length(accurecy$y_new), accurecy$y_new, type = "o", col = "red", lwd = 2,ylim = c(0,40),
     ylab = "Maximum Tempertaure",xlab = "Number of Days",main = "Line Diagram of Actual Vs Forecasted Maximum Temperature Near Fort Waine")
lines(accurecy$y_forecasted, col = "blue", type = "o", lwd = 2)
legend("bottomright",legend = c("Actual","Forecasted"),
       col = c("red", "blue"), pch  = c(19,19), cex =c(1.2,1.2))


p8 <- ggplot(data = dat5, aes(x = long, y = lat))+
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




library(patchwork)
p20 <- ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = temp), size = 2.3) + 
  labs(title = "Plot of Maximum Temperature of USA on 1st September,2017", x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.key.size = unit(0.4, "in")) + 
  scale_color_viridis(name = "Maximum Temperture", option = "inferno")

dat5 <- cbind(dat5, temp_forecasted = forecasted_y_one_day)
save(dat5, file = "dat5.Rdata")

p30 <- ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = temp_forecasted), size = 2.3) + 
  labs(title = "Plot of Forecasted Maximum Temperature of USA on 1st September,2017", x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.key.size = unit(0.4, "in")) + 
  scale_color_viridis(name = "Maximum Temperture", option = "inferno")

p10 <- p20/p30

error <- dat5$temp - dat5$temp_forecasted
dat5 <- cbind(dat5,error)

p12 <- ggplot(data = dat5, aes(x = long, y = lat))+
  geom_point(dat5, mapping = aes(x = long, y = lat, color = error), size = 2.3) + 
  labs(title = "Plot of Error of Forecasting Maximum Temperature of USA on 1st September,2017", x = "Longitude", y = "Latitude") + 
  theme(axis.text = element_text(size=12),
        axis.title=element_text(size=12),
        plot.title = element_text(size=12, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12), 
        legend.key.size = unit(0.4, "in")) + 
  scale_color_viridis(name = "Maximum Temperture", option = "inferno")


