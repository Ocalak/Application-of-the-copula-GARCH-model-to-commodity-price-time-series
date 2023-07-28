#standardized residuals
res <- list((fit_gas@fit$residuals-mean(fit_gas@fit$residuals))/sd(fit_gas@fit$residuals),
            (fit_oil@fit$residuals-mean(fit_oil@fit$residuals))/sd(fit_oil@fit$residuals),
            (fit_coal@fit$residuals-mean(fit_coal@fit$residuals))/sd(fit_coal@fit$residuals))


#Pit Transformation to standardized residuals
pit_trs <- sapply(res, pobs)
