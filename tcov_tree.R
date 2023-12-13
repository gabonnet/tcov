###################################### 
###   Populate the decision tree   ###
######################################


gen_tree <- function(tree){
  
  tr <- as.data.table(tree)
  
  ## Probability of diagnostic
  tr[,diag := ifelse(Result_PCR==999,ifelse(Result_RDT==999,ifelse(as.numeric(Testing_scen)==1,1,0),Result_RDT),Result_PCR)] 
  
  ## Population in the branch
  tr[,pop_branch := (Has_COVID * pcov + (1-Has_COVID)*(1-pcov)) * # probability of health status
       (ifelse(Result_RDT==999,1,Has_COVID * Result_RDT * RDT_sensi + (1 - Has_COVID) * (1 - Result_RDT) * RDT_speci+ Has_COVID * (1 - Result_RDT) * (1-RDT_sensi) + (1-Has_COVID) * Result_RDT * (1-RDT_speci))) * # probability of RDT test result
       (ifelse(Result_PCR==999,1,Has_COVID * Result_PCR * PCR_sensi +  (1 - Has_COVID) * (1 - Result_PCR) * PCR_speci+ Has_COVID * (1 - Result_PCR) * (1-PCR_sensi) + (1-Has_COVID) * Result_PCR * (1-PCR_speci))) * # probability of PCR test result
       (Treat_hosp * (1-Treat_refusal_severe)+ (1-Treat_hosp) * Treat_refusal_severe)] # probability of treatment choice
  
  ## Population treated with TCZ (for calculations of maximum TCZ cost to ensure cost-effectiveness)
  tr[,IL6Treated_pop := pop_branch * diag * Treat_hosp * IL6_iftreat]
  
  ## Deaths from COVID within the branch (multiplied by branch population)
  tr[,death_Cov := pop_branch * Has_COVID * Treat_hosp * ( diag * ((1 - MV_iftreat) * ((1 - Corti_iftreat) * CFR_ConvOxy + Corti_iftreat * CFR_Corti_ConvOxy) + MV_iftreat * ((1 - Corti_iftreat) * CFR_SoC + Corti_iftreat * (IL6_iftreat * CFR_IL6_Corti_SoC + (1-IL6_iftreat) * CFR_Corti_SoC)))+ (1-diag) * ((1-MV_iftreat) *CFR_ConvOxy + MV_iftreat * CFR_SoC))+ pop_branch * Has_COVID * (1-Treat_hosp) * (CFR_Oxy_noHosp * (1-need_MV_SoC) + need_MV_SoC * CFR_MV_noHosp)] 
  
  ## Deaths from treatment side effects in the branch (multiplied by branch population)
  tr[,deathside := pop_branch *  severe_effects * Treat_hosp * diag * (Corti_iftreat * (Risk_fracture_cortico * CFR_fracture+ Risk_GI_bleed_cortico * CFR_GI_bleed+ Risk_VT_cortico * CFR_VT + Risk_sepsis_cortico * (CFR_sepsis + CFR_sepsis_1year) + Risk_HF_cortico * CFR_HF + Disease_spec_mort_risk_cortico * (1-Has_COVID)) + IL6_iftreat * TB_risk_IL6_10days * CFR_TB)] 
  
  ## DALYs from treatment side effects in the branch (multiplied by branch population)
  tr[,DALY_side := pop_branch * severe_effects * Treat_hosp * diag * Corti_iftreat * ( (Has_COVID * YLL_Cov + (1- Has_COVID) * YLL_notCov) * (Risk_fracture_cortico * CFR_fracture+ Risk_GI_bleed_cortico * CFR_GI_bleed+ Risk_VT_cortico * CFR_VT + Risk_sepsis_cortico * (CFR_sepsis+CFR_sepsis_1year) + (1-Has_COVID) * Disease_spec_mort_risk_cortico) + Risk_HF_cortico * YLL_HF_cortico) - pop_branch * severe_effects * Treat_hosp * diag * Corti_iftreat *Risk_sepsis_cortico * CFR_sepsis_1year * Delayed_sepsis_time + Treat_hosp * diag * IL6_iftreat * TB_risk_IL6_10days * CFR_TB * pop_branch * (Has_COVID * YLL_Cov + (1- Has_COVID) * YLL_notCov) + # Years of life lost from side-effects
       pop_branch * Treat_hosp * diag * ( Corti_iftreat * (DALY_minor_cortico_10d * Risk_minor_cortico * mild_effects + severe_effects * (Risk_GI_bleed_cortico * YLD_GI_bleed +Risk_HF_cortico * Life_Exp_HF * DW_HF) + severe_effects * (Risk_fracture_cortico * CFR_fracture * (1/YLLtoDALY_fracture-1)+ Risk_VT_cortico * DW_VT * (1-CFR_VT) + Corti_iftreat * Risk_sepsis_cortico * DW_sepsis * (1-CFR_sepsis-CFR_sepsis_1year)) * ((1-Has_COVID) * YLL_notCov+ Has_COVID * YLL_Cov)) + Risk_sepsis_cortico * DW_sepsis * CFR_sepsis_1year * Delayed_sepsis_time + IL6_iftreat * (DALY_gastro_10d * Risk_gastro_IL6 * mild_effects + DALY_URTI_85d * Risk_URTI_IL6 * mild_effects + TB_risk_IL6_10days * severe_effects * DALY_weight_TB * Time_TB_Disease_years))] # YLDs from treatment side effects 
  
  ## Time in hospital with COVID (multiplied by branch population)
  tr[,Time_hosp := pop_branch * Treat_hosp * Has_COVID * (MV_iftreat * Hospt_Cov_SoC + (1- MV_iftreat) * Hospt_Cov_noMV + diag * Corti_iftreat * (MV_iftreat * (Hospt_Cov_corti + IL6_iftreat * Hospt_Cov_IL6) + (1-MV_iftreat) * Hospt_Cov_corti_noMV))]
  
  ## Time needing ICU from COVID (multiplied by branch population) for hospitalized patients, including for cases where it is not available
  tr[,Time_need_ICU := Treat_hosp * pop_branch * Has_COVID * ((1- diag * Corti_iftreat) * need_MV_SoC * ICUt_Cov_SoC+ diag * Corti_iftreat * ( (1-IL6_iftreat) * need_MV_Corti_SoC + IL6_iftreat * need_MV_IL6_Corti_SoC) *  (ICUt_Cov_SoC + ICUt_Cov_corti + IL6_iftreat *  ICUt_Cov_IL6))] 
  
  ## DALYs from COVID
  tr[,DALY_Covid := death_Cov * YLL_Cov + ## Years of life lost
       (Time_hosp - Time_need_ICU) * DALY_hosp + Time_need_ICU * DALY_ICU + (1-Treat_hosp) * pop_branch * Time_sick_home * DALY_home_Covid * Has_COVID + pop_branch * Has_COVID * DALYs_postCov_p1000 * 0.001 / (1+Treated_in_Bowe* DALYs_postCov_Treat_Impact) * (1+ DALYs_postCov_Treat_Impact * diag * Treat_hosp * Corti_iftreat)] # YLDs
  
  
  ## Costs of screening and testing
  tr[,Cost_tests_screening := pop_branch * (ifelse(Result_PCR==999,0,Cost_PCR_testkit + Cost_PCR_samp)+ifelse(Result_RDT==999,0,Cost_RDT_testkit + Cost_RDT_samp)) + # Testing costs
       pop_branch * ifelse(Code=="NoTreat",0,screencost * (Screen_sensi -pcov*(Screen_sensi + Screen_speci - 1))/Screen_sensi / (1-Screen_speci)/(1-Test_refusal_severe) * Cost_screen)] # Screening costs
  
  ## Costs of managing/treating COVID and its consequences  
  tr[,Cost_Covid := pop_branch * diag * Treat_hosp * (Corti_iftreat * (Cost_corti_base + Cost_corti_benefit) + IL6_iftreat * Cost_IL6) + # treatment costs
       (Time_hosp - Time_need_ICU * MV_iftreat) *Cost_bed + Time_need_ICU * MV_iftreat * Cost_ICU + # hospitalization and oxygen or MV costs
       Has_COVID * Costs_postCov * (1+ DALYs_postCov_Treat_Impact * diag * Treat_hosp * Corti_iftreat) + # Costs associated with long-COVID (assumes impact of treatment is the same on costs and DALYs)
       pop_branch * Cost_AM * (Add_use_AM_negno * (1-diag) + Use_AM_pos)+ # Costs of antibiotics
       death_Cov * Cost_BodyBag] # Cost of body bags
  
  ## Costs of the side-effects of corticosteroids and IL-6 receptor blockers
  tr[,Cost_side := pop_branch * Treat_hosp * diag * (Corti_iftreat * Cost_mild_effects_corti * Risk_minor_cortico + IL6_iftreat * Cost_mild_effects_IL6 *(Risk_gastro_IL6 + Risk_URTI_IL6)) + # costs of minor treatment side-effects
       pop_branch *  Treat_hosp * diag * (Corti_iftreat * (Risk_fracture_cortico * Cost_fract+ Risk_GI_bleed_cortico * Cost_Gibleed+ Risk_VT_cortico * Cost_VT + Risk_sepsis_cortico * Cost_sepsis + Risk_HF_cortico * Cost_HF) + IL6_iftreat * TB_risk_IL6_10days * Duration_TB_Treat_years * Cost_TB_month * 12) + # costs of treating severe side-effects of treatment 
       deathside * Cost_BodyBag] # Cost of body bags for side-effects-related deaths
  
  ##################################### 
  ### Total deaths, DALYs and costs ###
  #####################################
  
  tr[,total_deaths := death_Cov + deathside]
  tr[,total_DALYs := DALY_side + DALY_Covid]
  tr[,total_Costs := Cost_tests_screening + Cost_Covid + Cost_side]
  tr[,Net_Benefit := total_DALYs * Ochalek + total_Costs]
  
  
  #### Compute the probability of each scenario being the most cost-effective 
  a <- list()
  
  a[[1]] <- tr[, lapply(.SD, sum, na.rm=TRUE), by=.(Scen_Nb,VentiIL6_scen, Treat_scen,Scen_Nam,pcov,country,Income_level),.SDcols =c("total_deaths","total_DALYs","total_Costs","Net_Benefit","IL6Treated_pop") ]
  
  ## If we include scenarios in which mechanical ventilation is not available
  if(NoVenti==TRUE)
  { tr <- tr[, lapply(.SD, sum, na.rm=TRUE), by=.(Scen_Nb,VentiIL6_scen, Treat_scen,Scen_Nam,pcov,s,country,Income_level),.SDcols =c("total_deaths","total_DALYs","total_Costs","Net_Benefit") ][
    VentiIL6_scen==1 , Optscen_VentiIL6 := .SD[which.min(Net_Benefit)], by=.(pcov,s,country,Income_level)][
      Treat_scen==3, Optscen_Venti := .SD[which.min(Net_Benefit)], by = .(pcov,s,country,Income_level)][ 
        Treat_scen==2, Optscen_Oxy := .SD[which.min(Net_Benefit)], by = .(pcov,s,country,Income_level)]
  
  a2 = tr[, lapply(.SD, mean, na.rm=TRUE), by=.(VentiIL6_scen, Treat_scen,pcov,s,country,Income_level),.SDcols =c("Optscen_VentiIL6","Optscen_Venti","Optscen_Oxy") ][, lapply(.SD, mean, na.rm=TRUE), by= .(pcov,s,country)]
  a[[2]] <- a2[ , .(perIL6=.N/Nsamples) , by = .(Optscen_VentiIL6 , pcov,country,Income_level)]
  a[[3]] <- a2[ , .(perVenti=.N/Nsamples) , by = .(Optscen_Venti , pcov,country,Income_level)] 
  a[[4]] <- a2[ , .(perOxy=.N/Nsamples) , by = .(Optscen_Oxy , pcov,country,Income_level)]
  }  
  ## Computation excluding scenarios in which mechanical ventilation is unavailable
  else{ tr <- tr[, lapply(.SD, sum, na.rm=TRUE), by=.(Scen_Nb,VentiIL6_scen, Treat_scen,Scen_Nam,pcov,s,country,Income_level),.SDcols =c("total_deaths","total_DALYs","total_Costs","Net_Benefit") ][
    VentiIL6_scen==1 , Optscen_VentiIL6 := .SD[which.min(Net_Benefit)], by=.(pcov,s,country,Income_level)][
      Treat_scen==3, Optscen_Venti := .SD[which.min(Net_Benefit)], by = .(pcov,s,country,Income_level)]
  
  a2 = tr[, lapply(.SD, mean, na.rm=TRUE), by=.(VentiIL6_scen, Treat_scen,pcov,s,country,Income_level),.SDcols =c("Optscen_VentiIL6","Optscen_Venti") ][, lapply(.SD, mean, na.rm=TRUE), by= .(pcov,s,country)]
  a[[2]] <- a2[ , .(perIL6=.N/Nsamples) , by = .(Optscen_VentiIL6 , pcov,country,Income_level)]
  a[[3]] <- a2[ , .(perVenti=.N/Nsamples) , by = .(Optscen_Venti , pcov,country,Income_level)] 
  }
  
  return(a)
}
