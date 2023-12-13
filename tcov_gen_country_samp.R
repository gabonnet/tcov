#######################################################
### Generate samples of country-specific parameters ###
#######################################################

gen_country_sample <- function(Nparam, Nsample,c_par,lat_samp,countryC,cost_side,samp_gen)
{
  
  lc <- as.data.table(c_par[c_par$Code == countryC,]) # select country line in input dataset
  
  df <- data.table(matrix(nrow=Nsample))
  
  # generate random samples for the specified country
  
  df[,Income_level := lc[["Num_Income"]] ]
  
  df[,Cost_PCR_samp := qlnorm(lat_samp[Nparam+1,],log(lc[["Cost_PCR_samp"]])-lc[["Cost_PCR_samp_sdl"]]^2/2,lc[["Cost_PCR_samp_sdl"]])] 
  # Note: the difference between the average and median in lognormal distribution is SD^2/2
  
  df[,Cost_RDT_samp := qlnorm(lat_samp[Nparam+2,],log(lc[["Cost_RDT_samp"]])-lc[["Cost_RDT_samp_sdl"]]^2/2,lc[["Cost_RDT_samp_sdl"]])]
  
  df[,Cost_screen := qlnorm(lat_samp[Nparam+3,],log(lc[["Cost_screen"]])-lc[["Cost_screen_sdl"]]^2/2,lc[["Cost_screen_sdl"]])]
  
  df[,Cost_bed := qlnorm(lat_samp[Nparam+4,],log(lc[["Cost_bed"]])-lc[["Cost_bed_sdl"]]^2/2,lc[["Cost_bed_sdl"]])]
  
  df[,Cost_ICU := qlnorm(lat_samp[Nparam+5,],log(lc[["Cost_ICU"]])-lc[["Cost_ICU_sdl"]]^2/2,lc[["Cost_ICU_sdl"]])]
  
  df[,CFR_sepsis := qlnorm(lat_samp[Nparam+11,],log(lc[["CFR_sepsis"]])-lc[["CFR_sepsis_sdl"]]^2/2,lc[["CFR_sepsis_sdl"]])]
  
  df[,CFR_TB := qnorm(lat_samp[Nparam+12,],lc[["CFR_TB"]]-lc[["CFR_TB_sd"]]^2/2,lc[["CFR_TB_sd"]])]
  
  df[,YLL_Cov := gen_disc_YLL(qnorm(lat_samp[Nparam+6,],lc[["YLL_Cov"]]-lc[["YLL_Cov_sd"]]^2/2,lc[["YLL_Cov_sd"]]),samp_gen[,Health_discount])] 
  
  df[,YLL_notCov := gen_disc_YLL(qlnorm(lat_samp[Nparam+7,],log(lc[["YLL_notCov"]])-lc[["YLL_notCov_sdl"]]^2/2,lc[["YLL_notCov_sdl"]]),samp_gen[,Health_discount])]
  
  df[,YLL_HF_cortico:=gen_disc_YLL(qlnorm(lat_samp[Nparam+8,],log(lc[["YLL_HF_cortico"]])-lc[["YLL_HF_sdl"]]^2/2,lc[["YLL_HF_sdl"]]),samp_gen[,Health_discount])]
  
  df[,Ochalek := qunif(lat_samp[Nparam+9,],lc[["Ochalek_low"]],lc[["Ochalek_high"]])]
  
  df[,Cost_TB_month := qlnorm(lat_samp[Nparam+10,],log(lc[["Cost_TB_month"]])-lc[["Cost_TB_month_sdl"]]^2/2,lc[["Cost_TB_month_sdl"]])]
  
  ifelse(cost_side=="low",df[,Cost_sepsis:=lc[["Cost_sepsis_low"]]],ifelse(cost_side=="high",df[,Cost_sepsis:=lc[["Cost_sepsis_high"]]],df[,Cost_sepsis:=lc[["Cost_sepsis"]]]))
  
  ifelse(cost_side=="low",df[,Cost_fract:=lc[["Cost_fract_low"]]],ifelse(cost_side=="high",df[,Cost_fract:=lc[["Cost_fract_high"]]],df[,Cost_fract:=lc[["Cost_fract"]]]))
  
  ifelse(cost_side=="low",df[,Cost_Gibleed:=lc[["Cost_Gibleed_low"]]],ifelse(cost_side=="high",df[,Cost_Gibleed:=lc[["Cost_Gibleed_high"]]],df[,Cost_Gibleed:=lc[["Cost_Gibleed"]]]))
  
  ifelse(cost_side=="low",df[,Cost_VT:=lc[["Cost_VT_low"]]],ifelse(cost_side=="high",df[,Cost_VT:=lc[["Cost_VT_high"]]],df[,Cost_VT:=lc[["Cost_VT"]]]))
  
  ifelse(cost_side=="low",df[,Cost_HF:=lc[["Cost_HF_low"]]],ifelse(cost_side=="high",df[,Cost_HF:=lc[["Cost_HF_high"]]],df[,Cost_HF:=lc[["Cost_HF"]]]))
  
  return(df)
  
}