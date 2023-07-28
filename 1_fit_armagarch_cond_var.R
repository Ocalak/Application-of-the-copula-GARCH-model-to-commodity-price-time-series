
#Order of ARMA GARCH function
order_AG <- function(x){
  opt_model <- auto.arima(x[1:2500],max.p = 2,max.q = 2,stationary=TRUE,seasonal=FALSE,ic="aic",allowmean = TRUE)
  #model <- arima(x[1:2500],order=c(opt_model$arma[1],0,opt_model$arma[2]),method="ML")
  para_grid <- expand_grid("ARCH"=1:2,"GARCH"=1:2)
  aic <- c()
  for (i in 1:nrow(para_grid)) {
    spec <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(para_grid[[i,1]],para_grid[[i,2]])),
                       mean.model = (list(armaOrder = c(opt_model$arma[1],opt_model$arma[2]),include.mean=TRUE)),
                       distribution.model = "norm")
    modelgarch <- ugarchfit(spec,x[1:2500],optim=2500)
    #SAve the AIC of each armagarch.
    aic[i] <- infocriteria(modelgarch)[1]
    
  }
  #get smallest AICscore and respectvly the oreders
  opt_parameters <- c(opt_model$arma[1],opt_model$arma[2],para_grid[which.min(aic),][1],para_grid[which.min(aic),][2])
  return(opt_parameters)
}
#fit ARMA-GARCH function
fit_models <- function(x,order){
  spec <- ugarchspec(variance.model = list(model="sGARCH",garchOrder=c(order[[3]],order[[4]])),
                     mean.model = (list(armaOrder = c(order[[1]],order[[2]]),include.mean=TRUE)),
                     distribution.model = "norm")
  fit<- ugarchfit(spec,x[1:2500],optim=2500)
  return(fit)
}


#Get the orders
order_gas <- order_AG(commodities$NGas)
order_oil <- order_AG(commodities$oil)
order_coal <- order_AG(commodities$coal)

#Fit ARMA-GARCH
fit_gas <- fit_models(commodities$NGas,order_gas)
fit_oil <- fit_models(commodities$oil,order_oil)
fit_coal <- fit_models(commodities$coal,order_coal)



#Extract Coeffcieints
fit_gas@fit$matcoef
#Extract Coeffcieints
fit_oil@fit$matcoef
#Extract Coeffcieints
fit_coal@fit$matcoef


#Conditional variances
cv_gas <- fit_gas@fit$sigma^2
cv_oil <- fit_oil@fit$sigma^2
cv_coal <- fit_coal@fit$sigma^2

plot(commodities$NGas[1:2500],type="l")
lines(cv_gas,col="red")

plot(commodities$oil[1:2500],type="l")
lines(cv_oil,col="red")

plot(commodities$coal[1:2500],type="l")
lines(cv_coal,col="red")