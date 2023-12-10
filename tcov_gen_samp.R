#####################################################################
### Generate samples of generic (not country-specific) parameters ###
#####################################################################

# Function generating a vector of values given the characteristics of a distribution

gen_one_param_value <- function(distr, d1, d2, lat,k){
  
  if(distr=="Fixed"){valpar <- rep(d1,ncol(lat))} 
  else if(distr=="Uniform") {valpar <- qunif(lat[k,],d1,d2)}
  else if(distr=="Beta") {valpar <- qbeta(lat[k,],d1,d2)}
  else if(distr=="Lognormal") {valpar <- qlnorm(lat[k,],d1,d2)}
  else if(distr=="Normal") {valpar <- qnorm(lat[k,],d1,d2)}
  else if(distr=="Gamma") {valpar <- qgamma(lat[k,],d1,d2)}
  else if(distr=="Weibull") {valpar <- qweibull(lat[k,],d2,d1)}
  else {print("Unknown distribution type")}
  return(valpar)
}

# Creation of the sample of generic values

gen_sample <- function(Nparam, Nsample,general_param,lat_samp){
  
  names_params <- colnames(general_param)
  scen_params <- as.data.table(matrix(nrow=Nsample,ncol=Nparam))
  colnames(scen_params) <- names_params
  
  for(k in 1:Nparam){
    distr <- general_param[2,k]
    d1 <- as.numeric(general_param[3,k])
    d2 <- as.numeric(general_param[4,k])
    scen_params[,k]<- gen_one_param_value(distr,d1,d2,lat_samp,k)
    
  }
  
  return(scen_params)
  
}