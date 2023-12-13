###################################################################################
### Create useful parameters, exchange risks and odds, and discounting function ###
###################################################################################


##### Compute useful parameters depending on the sample of scenarios #####

gen_useful_params <- function(sampgen, sampc){
  
  params <- as.data.table(lapply(cbind(sampgen,sampc),as.numeric))
  
  # Parameters under SoC conditions
  params[,CFR_Corti_SoC := CFR_SoC / (1 - CFR_SoC) * OR_death_corti_SoC / (1+CFR_SoC / (1 - CFR_SoC) * OR_death_corti_SoC)]
  
  params[,need_MV_Corti_SoC := need_MV_SoC / (1 - need_MV_SoC) * OR_MV_corti_SoC / (1+ need_MV_SoC / (1 - need_MV_SoC) * OR_MV_corti_SoC)]
  
  params[,CFR_IL6_Corti_SoC := CFR_SoC / (1 - CFR_SoC) * OR_death_corti_SoC * OR_death_cortiIL_vs_corti_SoC / (1+ CFR_SoC / (1 - CFR_SoC) * OR_death_corti_SoC * OR_death_cortiIL_vs_corti_SoC)]
  
  params[,need_MV_IL6_Corti_SoC := need_MV_SoC / (1 - need_MV_SoC) * OR_MV_corti_SoC * OR_MV_cortiIL_vs_corti_SoC / (1+ need_MV_SoC / (1 - need_MV_SoC) * OR_MV_corti_SoC * OR_MV_cortiIL_vs_corti_SoC)]
  
  # Parameters for when only conventional oxygen is available
  params[,CFR_ConvOxy := (CFR_SoC  - CFR_MV_SoC * need_MV_SoC) + need_MV_SoC * CFR_MV_ConvOxy]
  
  params[,CFR_Corti_ConvOxy := CFR_ConvOxy / (1- CFR_ConvOxy) * OR_death_corti_SoC / (1 + CFR_ConvOxy / (1- CFR_ConvOxy) * OR_death_corti_SoC)]
  
  # Parameters for people that are not hospitalized and do not receive any supplemental oxygen
  params[,CFR_noHosp := CFR_Oxy_noHosp * (1-need_MV_SoC)+ CFR_MV_noHosp * need_MV_SoC]
  
  return(params)
  
}

#########################################################

##### Functions exchanging risks and odds #####

r2o <- function(risk){
  odds <- risk/(1-risk)
  return(odds)}

o2r <- function(odds){
  risk <- odds/(1+odds)
  return(risk)}


#########################################################

##### Discounting function #####

gen_disc_YLL <- function(Y,d){
  YLL <- ifelse(d>0,(1-exp(-d*Y))/d,Y)
  return(YLL)
}