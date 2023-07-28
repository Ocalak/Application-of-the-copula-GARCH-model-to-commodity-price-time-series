#Ar Model Forecast
pred_ar_gas <- c()
var_ar_gas <- c()
pred_ar_oil <- c()
var_ar_oil <- c()
pred_ar_coal <- c()
var_ar_coal <- c()
for (i in 0:199) {
  ar_model_coal <- arima(commodities$coal[1:(2500+i)],order=c(1,0,0),method = "ML")
  pred_ar_coal[i+1] <- ar_model_coal$coef[1]*commodities$coal[2500+i] +ar_model_coal$coef[2]
  var_ar_coal[i+1] <- ar_model_coal$sigma2
  ar_model_oil <- arima(commodities$oil[1:(2500+i)],order=c(1,0,0),method = "ML")
  pred_ar_oil[i+1] <- ar_model_oil$coef[1]*commodities$oil[2500+i] +ar_model_oil$coef[2]
  var_ar_oil[i+1] <- ar_model_oil$sigma2
  ar_model_gas <- arima(commodities$gas[1:(2500+i)],order=c(1,0,0),method = "ML")
  pred_ar_gas[i+1] <- ar_model_gas$coef[1]*commodities$gas[2500+i] +ar_model_gas$coef[2]
  var_ar_gas[i+1] <- ar_model_gas$sigma2
}

##CRPS Function
man_crps <- function(mu,sigma,x){
  x_1 = (x-mu)/sigma
  x_2 = sigma*(1/sqrt(pi)-2*dnorm(x_1)-x_1*(2*pnorm(x_1)-1))
  return(x_2)
}crps_gas <- c()
crps_argas <- c()
crps_oil <- c()
crps_aroil <- c()
crps_coal <- c()
crps_arcoal <- c()
for (i in 1:200) {
  crps_gas[i] <- -man_crps(mod_gas@forecast$density$Mu[i],mod_gas@forecast$density$Sigma[i],commodities$NGas[2500+i])
  crps_argas[i] <- -man_crps(pred_ar_gas[i],sqrt(var_ar_gas[i]),commodities$NGas[2500+i])
  crps_oil[i] <- -man_crps(mod_oil@forecast$density$Mu[i],mod_oil@forecast$density$Sigma[i],commodities$oil[2500+i])
  crps_aroil[i] <- -man_crps(pred_ar_oil[i],sqrt(var_ar_oil[i]),commodities$oil[2500+i])
  crps_coal[i] <- -man_crps(mod_coal@forecast$density$Mu[i],mod_coal@forecast$density$Sigma[i],commodities$coal[2500+i])
  crps_arcoal[i] <- -man_crps(pred_ar_coal[i],sqrt(var_ar_coal[i]),commodities$coal[2500+i])
}



par(mfrow = c(3, 1), mex = 0.5, cex = 0.5)
ts.plot(ts.gas[2501:2700],type="l",ylim=c(-1.5,2),lwd=1,main="Natural Gas",ylab="Natural Gas")
lines(crps_gas,col="red")
lines(crps_argas,col="green")
legend("topleft",c("Actual N.Gas","CRPS of ARMA-GARCH","CRPS of AR(1)"),lty=c(1,1,1),col=c("black","red","green"),cex=0.6)
#lines(crps,col="blue")
mean(crps_gas)
mean(crps_argas)
##oil
ts.plot(ts.oil[2501:2700],type="l",ylim=c(-12,12),lwd=1,main="Oil",ylab="Oil")
lines(crps_oil,col="red")
lines(crps_aroil,col="green")
legend("top",c("Actual Oil","CRPS of ARMA-GARCH","CRPS of AR(1)"),lty=c(1,1,1),col=c("black","red","green"),cex=0.6)

mean(crps_coal)
mean(crps_arcoal)
##Coal
ts.plot(ts.coal[2501:2700],type="l",ylim=c(-6,6),lwd=1,main="coal",ylab="Coal")
lines(crps_coal,col="red")
lines(crps_arcoal,col="green")
legend("bottomright",c("Actual Coal","CRPS of ARMA-GARCH","CRPS of AR(1)"),lty=c(1,1,1),col=c("black","red","green"),cex=0.6)

