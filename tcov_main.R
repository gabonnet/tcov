#############################################################################
### Generate most cost-effective testing option under different scenarios ###
#############################################################################


set.seed(1876543)

alea = TRUE

Nsamples <- 1000

cost_side <- "median" ## Choose scenario for the cost of side-effects
# cost_side <- "low"
# cost_side <- "high"

screencost = FALSE # Include/exclude screening cost differential.
self_testing = FALSE # Estimates (or not) outcomes if patients can self-test at care centers.
severe_effects = TRUE # Include severe corticosteroid side-effects.
mild_effects = TRUE # Include mild corticosteroid side-effects.
PCRavailable = FALSE ## Choose TRUE to compare all options, including the use of PCR alone or as a means of confirming an RDT test results.
NoVenti = TRUE ## Include scenarios in which mechanical ventilation is unavailable if true. This will include those scenarios for all options considered, including in deterministic sensitivity analysis. 

path.io <- "./Outputs/"
path.dat <- "./data/"  

###################################################################

source("tcov_gen_samp.R") ### generates generic parameters
source("tcov_gen_country_samp.R") ### generates country-specific parameters
source("tcov_gen_useful_params.R") ### computes useful parameters
source("tcov_tree.R") ### generates the decision tree

require("data.table") 
require("tidyverse")
require("ggplot2")
require("readxl")
require("lhs")

##################################################################

start_time <- Sys.time()

### Importing list of generic and country parameters and distributions
library(readxl)
general_params <- read_excel("./Data/Parameters.xlsx", sheet = "General parameters")
country_params <- as.data.table(read_excel("./Data/Parameters.xlsx", sheet = "Country parameters"))

ifelse(PCRavailable==TRUE,
       ifelse(NoVenti==TRUE,
              tcov_trees <- as.data.table(read_excel("./Data/Parameters.xlsx", sheet = "Branches")),
              tcov_trees <- as.data.table(read_excel("./Data/Parameters.xlsx", sheet = "BranchesNoVenti"))),
       ifelse(NoVenti==TRUE,
              tcov_trees <- as.data.table(read_excel("./Data/Parameters.xlsx", sheet = "BranchesRDTvsdonothing")),
              tcov_trees <- as.data.table(read_excel("./Data/Parameters.xlsx", sheet = "BranchesNoPCRorVenti"))))

Nparams <- ncol(general_params)
Ncountries <- nrow(country_params)
Nrandparcountry <- 13

### Generating parameter scenarios for selected country

require("lhs")
A <- randomLHS(Nparams+Nrandparcountry,Nsamples)


samp_gen <- gen_sample(Nparams, Nsamples,general_params,A) # Create a sample of general parameters
samp_gen[,Life_Exp_HF:=gen_disc_YLL(Life_Exp_HF,Health_discount)][,Delayed_sepsis_time:=gen_disc_YLL(Delayed_sepsis_time,Health_discount)][,Time_TB_Disease_years:=gen_disc_YLL(Time_TB_Disease_years,Health_discount)]

prevalences <- data.table(pcov=seq(0,0.3, by = 0.01))

## Sensitivity analysis (k = 1 if none)

for(k in 1:1){
  
  # Different options for the cost of generic side-effects
  if(k==2){cost_side = "low"} else if(k==3) {cost_side = "high"} else{cost_side = "median"}
  # Integrating disease-specific corticosteroid side-effects
  if(k==4){samp_gen[,Disease_spec_mort_risk_cortico:=as.numeric(general_params$Disease_spec_mort_risk_cortico[5])]} else{samp_gen[,Disease_spec_mort_risk_cortico:=as.numeric(general_params$Disease_spec_mort_risk_cortico[1])]}
  # Explore other RDT sensitivity
  if(k==5){samp_gen[,RDT_sensi:=as.numeric(general_params$RDT_sensi[5])]} else if(k==6){samp_gen[,RDT_sensi:=as.numeric(general_params$RDT_sensi[6])]} else{samp_gen[,RDT_sensi:=as.numeric(general_params$RDT_sensi[1])]}
  # Explore treatment refusal
  if(k==7){samp_gen[,Treat_refusal_severe:=as.numeric(general_params$Treat_refusal_severe[5])]} else if(k==8){samp_gen[,Treat_refusal_severe:=as.numeric(general_params$Treat_refusal_severe[6])]} else{samp_gen[,Treat_refusal_severe:=as.numeric(general_params$Treat_refusal_severe[1])]}
  # Explore different costs for IL6 receptor blockers
  if(k==9){samp_gen[,Cost_IL6:=as.numeric(general_params$Cost_IL6[5])]} else if(k==10){samp_gen[,Cost_IL6:=as.numeric(general_params$Cost_IL6[6])]} else{samp_gen[,Cost_IL6:=as.numeric(general_params$Cost_IL6[1])]}
  # With health discounting
  if(k==11){samp_gen[,Health_discount:=as.numeric(general_params$Health_discount[5])]} else{samp_gen[,Health_discount:=as.numeric(general_params$Health_discount[1])]}
  # if screening costs were ignored
  if(k==12){screencost=TRUE} else{screencost=FALSE}
  # if RDT test kits were cheaper or more expensive
  if(k==13){samp_gen[,Cost_RDT_testkit:=as.numeric(general_params$Cost_RDT_testkit[5])]} else if(k==14){samp_gen[,Cost_RDT_testkit:=as.numeric(general_params$Cost_RDT_testkit[6])]} else{samp_gen[,Cost_IL6:=as.numeric(general_params$Cost_RDT_testkit[1])]}
  # Assuming treatment affects the likelihood of post-COVID  
  if(k==15){samp_gen[,DALYs_postCov_Treat_Impact:=as.numeric(general_params$DALYs_postCov_Treat_Impact[5])]} else
  if(k==16){samp_gen[,DALYs_postCov_Treat_Impact:=as.numeric(general_params$DALYs_postCov_Treat_Impact[6])]}
  else{samp_gen[,DALYs_postCov_Treat_Impact:=as.numeric(general_params$DALYs_postCov_Treat_Impact[1])]}
# Scenario without side-effects 
  if(k==17){severe_effects=FALSE 
  mild_effects=FALSE} else {severe_effects=TRUE
  mild_effects=TRUE}
  
  #######################################################################
  
  Synthesis_total <- list()
  
  country_code_list = country_params[,1][[1]]
  
  ## Create a decision tree containing all parameter sets and countries
  Tree_countryIL6 <- list()
  Tree_countryVenti <- list()
  Tree_countryOxy <- list()
  i=0
  for(countryCode in country_code_list){
    i=i+1
    income <- country_params[,.SD[which(Code==countryCode)]]$Num_Income
    samp_c <- gen_country_sample(Nparams, Nsamples,country_params,A,countryCode,cost_side,samp_gen) # Creates a sample of country-specific parameters
    paramite <- as.data.table(gen_useful_params(samp_gen,samp_c))[,country:=countryCode][,Income_level:=income][,s:=1:Nsamples]
    Tree_1country <- setkey(paramite[,c(k=1,.SD)],k)[tcov_trees[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
    Tree_1country <- setkey(Tree_1country[,c(k=1,.SD)],k)[prevalences[,c(k=1,.SD)],allow.cartesian=TRUE][,k:=NULL]
    Tree_countryIL6[[i]] <- gen_tree(Tree_1country)[[1]]
    Tree_countryVenti[[i]] <- gen_tree(Tree_1country)[[2]]
    if(NoVenti==TRUE){ 
      Tree_countryOxy[[i]] <- gen_tree(Tree_1country)[[3]]
    }
  }
  Tree_countriesIL6 <- rbindlist(Tree_countryIL6)
  Tree_countriesVenti <- rbindlist(Tree_countryVenti)
  if(NoVenti==TRUE){ 
    Tree_countriesOxy <- rbindlist(Tree_countryOxy)
  }
  
  k_list <- list()
  
  k_list[[1]] <- dcast( Tree_countriesIL6[, .SD[which.max(perIL6)], by=.(country,Income_level,pcov)][,.(per=.N),by=.(pcov,Income_level,Optscen_VentiIL6)], Income_level + Optscen_VentiIL6 ~ pcov,value.var="per")
  k_list[[2]] <- dcast( Tree_countriesVenti[, .SD[which.max(perVenti)], by=.(country,Income_level,pcov)][,.(per=.N),by=.(pcov,Income_level,Optscen_Venti)], Income_level + Optscen_Venti ~ pcov,value.var="per")
  if(NoVenti==TRUE){ 
  k_list[[3]] <- dcast( Tree_countriesOxy[, .SD[which.max(perOxy)], by=.(country,Income_level,pcov)][,.(per=.N),by=.(pcov,Income_level,Optscen_Oxy)], Income_level + Optscen_Oxy ~ pcov,value.var="per")
  }
  
  Synthesis_total[[k]] <- k_list
}
library("writexl")
# write_xlsx(Synthesis_total[[1]][[1]],".\\tcov_synthNumeric.xlsx")
# write_xlsx(Synthesis_total[[1]][[2]],".\\tcov_synthgraphsIL6.xlsx")
# write_xlsx(Synthesis_total[[1]][[3]],".\\tcov_synthgraphsVenti.xlsx")
# write_xlsx(Synthesis_total[[1]][[4]],".\\tcov_synthgraphsOxy.xlsx")
write_xlsx(Synthesis_total[[1]][[1]],".\\tcov_synthIL6.xlsx")
write_xlsx(Synthesis_total[[1]][[2]],".\\tcov_synthVenti.xlsx")
# write_xlsx(Synthesis_total[[1]][[1]],".\\tcov_selfIL6.xlsx")
# write_xlsx(Synthesis_total[[1]][[2]],".\\tcov_selfVenti.xlsx")
# write_xlsx(Synthesis_total[[2]][[1]],".\\tcov_lowsideIL6.xlsx")
# write_xlsx(Synthesis_total[[2]][[2]],".\\tcov_lowsideVenti.xlsx")
# write_xlsx(Synthesis_total[[3]][[1]],".\\tcov_highsideIL6.xlsx")
# write_xlsx(Synthesis_total[[3]][[2]],".\\tcov_highsideVenti.xlsx")
# write_xlsx(Synthesis_total[[4]][[1]],".\\tcov_dispecsideIL6.xlsx")
# write_xlsx(Synthesis_total[[4]][[2]],".\\tcov_dispecsideVenti.xlsx")
# write_xlsx(Synthesis_total[[5]][[1]],".\\tcov_RDTsensi60IL6.xlsx")
# write_xlsx(Synthesis_total[[5]][[2]],".\\tcov_RDTsensi60Venti.xlsx")
# write_xlsx(Synthesis_total[[6]][[1]],".\\tcov_RDTsensi40IL6.xlsx")
# write_xlsx(Synthesis_total[[6]][[2]],".\\tcov_RDTsensi40Venti.xlsx")
# write_xlsx(Synthesis_total[[7]][[1]],".\\tcov_Treatrefus30IL6.xlsx")
# write_xlsx(Synthesis_total[[7]][[2]],".\\tcov_Treatrefus30Venti.xlsx")
# write_xlsx(Synthesis_total[[8]][[1]],".\\tcov_Treatrefus60IL6.xlsx")
# write_xlsx(Synthesis_total[[8]][[2]],".\\tcov_Treatrefus60Venti.xlsx")
# write_xlsx(Synthesis_total[[9]][[1]],".\\tcov_CostIL6lowIL6.xlsx")
# write_xlsx(Synthesis_total[[9]][[2]],".\\tcov_CostIL6lowVenti.xlsx")
# write_xlsx(Synthesis_total[[10]][[1]],".\\tcov_CostIL6highIL6.xlsx")
# write_xlsx(Synthesis_total[[10]][[2]],".\\tcov_CostIL6highVenti.xlsx")
# write_xlsx(Synthesis_total[[11]][[1]],".\\tcov_HdiscountIL6.xlsx")
# write_xlsx(Synthesis_total[[11]][[2]],".\\tcov_HdiscountVenti.xlsx")
# write_xlsx(Synthesis_total[[12]][[1]],".\\tcov_noscreenIL6.xlsx")
# write_xlsx(Synthesis_total[[12]][[2]],".\\tcov_noscreenVenti.xlsx")
# write_xlsx(Synthesis_total[[13]][[1]],".\\tcov_lowRDTkitCostIL6.xlsx")
# write_xlsx(Synthesis_total[[13]][[2]],".\\tcov_lowRDTkitCostVenti.xlsx")
# write_xlsx(Synthesis_total[[14]][[1]],".\\tcov_highRDTkitCostIL6.xlsx")
# write_xlsx(Synthesis_total[[14]][[2]],".\\tcov_highRDTkitCostVenti.xlsx")
# write_xlsx(Synthesis_total[[15]][[1]],".\\tcov_treat_reduces_postCovIL6.xlsx")
# write_xlsx(Synthesis_total[[15]][[2]],".\\tcov_treat_reduces_postCovVenti.xlsx")
# write_xlsx(Synthesis_total[[16]][[1]],".\\tcov_treat_increases_postCovIL6.xlsx")
# write_xlsx(Synthesis_total[[16]][[2]],".\\tcov_treat_increass_postCovVenti.xlsx")
# write_xlsx(Synthesis_total[[17]][[1]],".\\tcov_nosideIL6.xlsx")
# write_xlsx(Synthesis_total[[17]][[2]],".\\tcov_nosideVenti.xlsx")
# 
if(NoVenti==TRUE){ 
  write_xlsx(Synthesis_total[[1]][[3]],".\\tcov_synthOxy.xlsx")
  # write_xlsx(Synthesis_total[[1]][[3]],".\\tcov_selfOxy.xlsx")
  # write_xlsx(Synthesis_total[[2]][[3]],".\\tcov_lowsideOxy.xlsx")
  # write_xlsx(Synthesis_total[[3]][[3]],".\\tcov_highsideOxy.xlsx")
  # write_xlsx(Synthesis_total[[4]][[3]],".\\tcov_disspecsideOxy.xlsx")
  # write_xlsx(Synthesis_total[[5]][[3]],".\\tcov_RDTsensi60Oxy.xlsx")
  # write_xlsx(Synthesis_total[[6]][[3]],".\\tcov_RDTsensi40Oxy.xlsx")
  # write_xlsx(Synthesis_total[[7]][[3]],".\\tcov_Treatrefus30Oxy.xlsx")
  # write_xlsx(Synthesis_total[[8]][[3]],".\\tcov_Treatrefus60Oxy.xlsx")
  # write_xlsx(Synthesis_total[[9]][[3]],".\\tcov_CostIL6lowOxy.xlsx")
  # write_xlsx(Synthesis_total[[10]][[3]],".\\tcov_CostIL6highOxy.xlsx")
  # write_xlsx(Synthesis_total[[11]][[3]],".\\tcov_HdiscountOxy.xlsx")
  # write_xlsx(Synthesis_total[[12]][[3]],".\\tcov_noscreenOxy.xlsx")
  # write_xlsx(Synthesis_total[[13]][[3]],".\\tcov_lowRDTkitCostOxy.xlsx")
  # write_xlsx(Synthesis_total[[14]][[3]],".\\tcov_highRDTkitCostOxy.xlsx")
  # write_xlsx(Synthesis_total[[15]][[3]],".\\tcov_treat_reduces_postCovOxy.xlsx")
  # write_xlsx(Synthesis_total[[16]][[3]],".\\tcov_treat_increases_postCovOxy.xlsx")
  # write_xlsx(Synthesis_total[[17]][[3]],".\\tcov_nosideOxy.xlsx")
}

end_time <- Sys.time()
end_time - start_time