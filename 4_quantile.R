#Extract Quantiels
qq_g <- as.matrix(rugarch::quantile(mod_gas)$`q[0.05]`)
qq_o <- as.matrix(rugarch::quantile(mod_oil)$`q[0.05]`)
qq_c <- as.matrix(rugarch::quantile(mod_coal)$`q[0.05]`)
qng <- c()
qno <- c()
qnc <- c()
for (i in 1:200) {
  qng[i] <- qq_g[i,1]
  qno[i] <- qq_o[i,1]
  qnc[i] <- qq_c[i,1]
}


#FCalc the actual obs that fall below quantile
pro_below_qua <- function(x,quantiles){
  below_quantile <- sum(x[2501:2700] < quantiles)
  proportion_below_quantile <- below_quantile / length(x[2501:2700])
  return(proportion_below_quantile)
}

rate_gas <- pro_below_qua(commodities$NGas,qng)
rate_oil <- pro_below_qua(commodities$oil,qno)
rate_coal <- pro_below_qua(commodities$coal,qnc)


par(mfrow = c(3, 1), mex = 0.4, cex = 0.5)

plot(commodities$NGas[2501:2700],type="l",ylim=c(-1.5,2),ylab="Natural Gas",main="Natural Gas")
lines(qng,col="red")
legend("topleft",c("Actual Natural Gas","0.05 Quantiles of Forecasts"),lty=c(1,1),col=c("black","red"),cex=0.7)
plot(commodities$oil[2501:2700],type="l",ylim=c(-1.5,2),ylab="Oil",main="Oil")
lines(qno,col="red")
legend("topleft",c("Actual Oil","0.05 Quantiles of Forecasts"),lty=c(1,1),col=c("black","red"),cex=0.7)
plot(commodities$coal[2501:2700],type="l",ylim=c(-1.5,2),ylab="Coal",main="Coal")
lines(qnc,col="red")
legend("topleft",c("Actual Coal","0.05 Quantiles of Forecasts"),lty=c(1,1),col=c("black","red"),cex=0.7)