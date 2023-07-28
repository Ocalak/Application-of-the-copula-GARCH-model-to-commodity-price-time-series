
#Forecast ARMA-GARCH
spec_gas <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(order_gas[[3]],order_gas[[4]])),
                       mean.model = (list(armaOrder = c(order_gas[[1]],order_gas[[2]]),include.mean=TRUE)),
                       distribution.model = "norm")

mod_gas <- ugarchroll(spec_gas,commodities$NGas,n.ahead = 1,n.start = 2500,refit.every = 1,refit.window = "recursive",calculate.VaR = TRUE,VaR.alpha = 0.05,keep.coef = TRUE)

spec_oil <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(order_oil[[3]],order_oil[[4]])),
                       mean.model = (list(armaOrder = c(order_oil[[1]],order_oil[[2]]),include.mean=TRUE)),
                       distribution.model = "norm")
mod_oil <- ugarchroll(spec_oil,commodities$oil,n.ahead = 1,n.start = 2500,refit.every = 1,refit.window = "recursive",calculate.VaR = TRUE,VaR.alpha = 0.05,keep.coef = TRUE)


spec_coal <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(order_coal[[3]],order_coal[[4]])),
                        mean.model = (list(armaOrder = c(order_coal[[1]],order_coal[[2]]),include.mean=TRUE)),
                        distribution.model = "norm")
mod_coal <- ugarchroll(spec_coal,commodities$coal,n.ahead = 1,n.start = 2500,refit.every = 1,refit.window = "recursive",calculate.VaR = TRUE,VaR.alpha = 0.05,keep.coef = TRUE)






#Plot Froecast
#Create tibble for densities and realizations
densities = as_tibble(data.frame(date = commodities$date[2501:2700], matrix(NA, 200, 12)))
realizations = as_tibble (data.frame(obs = NA, date = NA))
#create an empty list to store forecasts
forecast = list(dens_gas = realizations,dens_oil = realizations,dens_coal = realizations)
#create and empy


densities[,2] = slot(mod_gas,"forecast")$density$Mu
densities[,3] = slot(mod_gas,"forecast")$density$Sigma
densities[,4] = slot(mod_oil,"forecast")$density$Mu
densities[,5] = slot(mod_oil,"forecast")$density$Sigma
densities[,6] = slot(mod_coal,"forecast")$density$Mu
densities[,7] = slot(mod_coal,"forecast")$density$Sigma
for (j in 1:3) {
  for (i in 1:200) {
    forecast[[j]] %<>% add_row(obs = rnorm(1000, mean= as.numeric(densities[i,2*j]),
                                           sd= as.numeric(densities[i,2*j+1])),
                               date=tail(commodities[,1],200)[i]) %>%drop_na()}}
#This is just for natural gas
forecast$dens_gas$date <- date(forecast$dens_gas$date)
plot = ggplot(forecast[[1]],aes(x=obs,y=date, group=date))+
  geom_density_ridges(rel_min_height = 0.001)+
  ggtitle(label="Natural Gas",subtitle = "One Day Ahead P.Forecast")+
  scale_y_date(name = "months in 2020", date_breaks = "1 month", date_labels = "%b")+
  xlab("realizations")
#extract the actual values and dates
plot <- plot+ coord_flip()
# Display the plot
print(plot)


#oil
forecast$dens_oil$date <- date(forecast$dens_oil$date)
plot2 <- ggplot(forecast[[2]],aes(x=obs,y=date, group=date))+
  geom_density_ridges(rel_min_height = 0.01)+
  ggtitle(label="Oil",subtitle = "One Day Ahead P.Forecast")+
  scale_y_date(name = "months in 2020", date_breaks = "1 month", date_labels = "%b")+
  xlab("realizations")
plot2 <- plot +coord_flip()
# Display the plot
print(plot2)

#Coal
forecast$dens_coal$date <- date(forecast$dens_coal$date)
plot3 = ggplot(forecast[[3]],aes(x=obs,y=date, group=date))+
  geom_density_ridges(rel_min_height = 0.01)+
  ggtitle(label="Coal",subtitle = "One Day Ahead P.Forecast")+
  scale_y_date(name = "months in 2020", date_breaks = "1 month", date_labels = "%b")+
  xlab("realizations")
plot3 <- plot3 +coord_flip()
# Display the plot
print(plot3)