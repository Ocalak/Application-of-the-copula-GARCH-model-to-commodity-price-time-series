#What do you need? 
#Fitted copula parameters.
#Mean forecasts and sigma forecasts from ARMA-GARCH
#Actual obs. of time series
sim  <- list()
sim_n  <- list()
sim_i  <- list()
for (i in 1:200) {
  
  simt <- rCopula(1000,fit_t_cop@copula)
  simn <- rCopula(1000,fit_n_cop@copula)
  simi <- rCopula(1000,fit_i_cop@copula)
  
  #quantile transforms of Copula simulation results for each asset
  sim[[i]]<-  qnorm(simt,
                    mean=mean=c(forecast_gas@forecast$density$Mu[i],
                                     forecast_oil@forecast$density$Mu[i],
                                     forecast_coal@forecast$density$Mu[i]),
                    sd=c(forecast_gas@forecast$density$Sigma[i],
                         forecast_oil@forecast$density$Sigma[i],
                         forecast_coal@forecast$density$Sigma[i]))
  sim_n[[i]]<-  qnorm(simn,
                      mean=c(forecast_gas@forecast$density$Mu[i],
                                      forecast_oil@forecast$density$Mu[i],
                                      forecast_coal@forecast$density$Mu[i]),
                      sd=c(forecast_gas@forecast$density$Sigma[i],
                           forecast_oil@forecast$density$Sigma[i],
                           forecast_coal@forecast$density$Sigma[i]))
  
  sim_i[[i]]<-  qnorm(simi,
                      mean=mean=c(forecast_gas@forecast$density$Mu[i],
                                       forecast_oil@forecast$density$Mu[i],
                                       forecast_coal@forecast$density$Mu[i]),
                      sd=c(forecast_gas@forecast$density$Sigma[i],
                           forecast_oil@forecast$density$Sigma[i],
                           forecast_coal@forecast$density$Sigma[i]))
}
est_results<- numeric(200)
esn_results<- numeric(200)
esi_results<- numeric(200)
es_results <- numeric(200)

# Iterate over each realization in the 'realizations' data frame
for (i in 1:200) {
  # Extract the first row of the i-th realization as observed values
  observed_values <- as.numeric(commodities[i, ][,-1 ])
  
  # Extract the i-th element of the simulation results list
  simulations_t <- sim[[i]]# t normal copula results
  simulations_n <- sim_n[[i]]#normal copula
  simulations_i <- sim_i[[i]]#indp. coplua
  # Compute es using the first row as observed values
  es_t <- es_sample(observed_values, t(simulations_t[, 1:3]))
  es_n <- es_sample(observed_values, t(simulations_n[, 1:3]))
  es_i <- es_sample(observed_values, t(simulations_i[, 1:3]))
  
  
  
  # Store the computed es in the results vector
  est_results[i] <- es_t
  esn_results[i] <- es_n
  esi_results[i] <- es_i
}


par(mfrow = c(2, 1), mex = 0.5, cex = 0.5)
plot(est_results,type="l")
lines(esn_results,col="red")
lines(esi_results,col="green")
legend("topleft",c("ES_t_copula","ES_n_copula","ES_i_copula"),lwd=c(1,1,1),col=c("black","red","green"))

plot(esi_results,col="black",type="l",lwd=2)
lines(crps_oil,col="red")
lines(crps_gas,col="green")
lines(crps_coal,col="blue")
legend("top",c("ES_i_copula","crps_oil","crps_gas","crps_coal"),lwd=c(1,1,1,1),col=c("black","red","green","blue"))






