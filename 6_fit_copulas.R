######Copula fit
#tcopula
fit_t_cop <- fitCopula(tCopula(dim=3,dispstr = "un"), data = pit_trs, method = "ml")
#Normal copula
fit_n_cop <- fitCopula(normalCopula(dim=3,dispstr = "un"), data = pit_trs, method = "ml")
#ind.copula
fit_i_cop <- fitCopula(gumbelCopula(dim=3,use.indepC =c(TRUE,TRUE,TRUE)),data=pit_trs,method = "ml")


