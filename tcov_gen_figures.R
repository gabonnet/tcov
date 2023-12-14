################################
######## CREATE FIGURES ########
################################

library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)


#######################################################

### Create Figure 2, most cost-effective option at different income levels and for different costs of corticosteroid side-effects


tcov_VentiA <- tcov_lowsideVenti
tcov_VentiB <- tcov_synthVenti13nov
tcov_VentiC <- tcov_highsideVenti

tcov2_synthVentiA <- as.data.frame(tcov_VentiA) %>% mutate(code = Income_level*100+ Optscen_Venti)
colnamestcovA <- tcov2_synthVentiA$code
tcovVentiA <- t(tcov2_synthVentiA[3:34])
tcovVentiA[is.na(tcovVentiA)] <- 0
colnames(tcovVentiA) = colnamestcovA
tcovVentiA <- tcovVentiA[1:31,]
tcov2_synthVentiB <- as.data.frame(tcov_VentiB) %>% mutate(code = Income_level*100+ Optscen_Venti)
colnamestcovB <- tcov2_synthVentiB$code
tcovVentiB <- t(tcov2_synthVentiB[3:34])
tcovVentiB[is.na(tcovVentiB)] <- 0
colnames(tcovVentiB) = colnamestcovB
tcovVentiB <- tcovVentiB[1:31,]
tcov2_synthVentiC <- as.data.frame(tcov_VentiC) %>% mutate(code = Income_level*100+ Optscen_Venti)
colnamestcovC <- tcov2_synthVentiC$code
tcovVentiC <- t(tcov2_synthVentiC[3:34])
colnames(tcovVentiC) = colnamestcovC
tcovVentiC[is.na(tcovVentiC)] <- 0
tcovVentiC <- tcovVentiC[1:31,]


d2a <- melt(tcovVentiA, id.vars=rownames(tcovVentiA))
df2a <- d2a %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df2a <- df2a %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df2a <- df2a %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df2a <- df2a %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))

df2a$Scenario <- factor(df2a$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ"))

VentiplotA <- ggplot(data =df2a, aes(x=Var1, y=value, group=Scen))  + theme(legend.position="bottom") +geom_line(aes(linetype=Scenario, color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1))  + scale_color_manual(values=c( "#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries \n in which scenario \n is cost-effective", x = element_blank()) + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98")) + theme(plot.margin = margin(0,0,0,0, 'cm')) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)

d2b <- melt(tcovVentiB, id.vars=rownames(tcovVentiB))
df2b <- d2b %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df2b <- df2b %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df2b <- df2b %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df2b <- df2b %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))

df2b$Scenario <- factor(df2b$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ"))

VentiplotB <- ggplot(data =df2b, aes(x=Var1, y=value, group=Scen))  + theme(legend.position="bottom") +geom_line(aes(linetype=Scenario, color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1))  + scale_color_manual(values=c( "#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries \n in which scenario \n is cost-effective", x = element_blank()) + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98")) + theme(plot.margin = margin(0,0,0,0, 'cm')) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)

d2c <- melt(tcovVentiC, id.vars=rownames(tcovVentiC))
df2c <- d2c %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df2c <- df2c %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df2c <- df2c %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df2c <- df2c %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))

df2c$Scenario <- factor(df2c$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ"))

VentiplotC <- ggplot(data =df2c, aes(x=Var1, y=value, group=Scen))  + theme(legend.position="bottom") +geom_line(aes(linetype=Scenario, color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1))  + scale_color_manual(values=c( "#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries \n in which scenario \n is cost-effective", x = "COVID-19 prevalence among severe suspected COVID cases") + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98")) + theme(plot.margin = margin(0,0,0,0, 'cm')) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)


library(ggpubr)
figure2 <- ggarrange(VentiplotA, VentiplotB, VentiplotC, labels = c("A", "B", "C"),   ncol = 1, nrow = 3,   common.legend = TRUE, legend = "bottom")

figure2

###########################################################################################

### Create figure 3: impact of disease-specific corticosteroid side-effects


tcov_Venti <- tcov_dispecsideVenti


tcov2_synthVenti <- as.data.frame(tcov_Venti) %>% mutate(code = Income_level*100+ Optscen_Venti)
colnamestcov <- tcov2_synthVenti$code
tcovVenti <- t(tcov2_synthVenti[3:34])
tcovVenti[is.na(tcovVenti)] <- 0
colnames(tcovVenti) = colnamestcov
tcovVenti <- tcovVenti[1:31,]


d2 <- melt(tcovVenti, id.vars=rownames(tcovVenti))
df2 <- d2 %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df2 <- df2 %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df2 <- df2 %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df2 <- df2 %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))

df2$Scenario <- factor(df2$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ"))

Figure3 <- ggplot(data =df2, aes(x=Var1, y=value, group=Scen))  + theme(legend.position="bottom") +geom_line(aes(linetype=Scenario, color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1))  + scale_color_manual(values=c( "#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries in which scenario is cost-effective", x = "COVID-19 prevalence among severe suspected COVID cases") + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98"))  + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)
Figure3

###########################################################################################

### Create figure 5: cost-effectiveness of different options for different income levels and TCZ costs
tcov_IL6a <- tcov_CostIL6lowIL6
tcov_IL6b <- tcov_synthIL613nov
tcov_IL6c <- tcov_CostIL6highIL6

tcov2_synthIL6a <- as.data.frame(tcov_IL6a) %>% mutate(code = Income_level*100+ Optscen_VentiIL6)
colnamestcova <- tcov2_synthIL6a$code
tcovIL6a <- t(tcov2_synthIL6a[3:34])
tcovIL6a[is.na(tcovIL6a)] <- 0
colnames(tcovIL6a) = colnamestcova
tcovIL6a <- tcovIL6a[1:31,]

tcov2_synthIL6b <- as.data.frame(tcov_IL6b) %>% mutate(code = Income_level*100+ Optscen_VentiIL6)
colnamestcovb <- tcov2_synthIL6b$code
tcovIL6b <- t(tcov2_synthIL6b[3:34])
tcovIL6b[is.na(tcovIL6b)] <- 0
colnames(tcovIL6b) = colnamestcovb
tcovIL6b <- tcovIL6b[1:31,]

tcov2_synthIL6c <- as.data.frame(tcov_IL6c) %>% mutate(code = Income_level*100+ Optscen_VentiIL6)
colnamestcovc <- tcov2_synthIL6c$code
tcovIL6c <- t(tcov2_synthIL6c[3:34])
tcovIL6c[is.na(tcovIL6c)] <- 0
colnames(tcovIL6c) = colnamestcovc
tcovIL6c <- tcovIL6c[1:31,]

d1a <- melt(tcovIL6a, id.vars=rownames(tcovIL6a))
df1a <- d1a %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df1a <- df1a %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df1a <- df1a %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df1a <- df1a %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))
df1a$Scenario <- factor(df1a$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ", "Test none, treat all, TCZ", "RDT, TCZ", "RDT, check negatives with PCR, TCZ","RDT, check positives with PCR, TCZ", "PCR, TCZ"))

d1b <- melt(tcovIL6b, id.vars=rownames(tcovIL6b))
df1b <- d1b %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df1b <- df1b %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df1b <- df1b %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df1b <- df1b %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))
df1b$Scenario <- factor(df1b$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ", "Test none, treat all, TCZ", "RDT, TCZ", "RDT, check negatives with PCR, TCZ","RDT, check positives with PCR, TCZ", "PCR, TCZ"))

d1c <- melt(tcovIL6c, id.vars=rownames(tcovIL6c))
df1c <- d1c %>% mutate(Income = ifelse(Var2>300,"UMIC",ifelse(Var2>200,"LMIC","LIC")))
df1c <- df1c %>% mutate(Scen = ifelse(Var2>300,Var2-300,ifelse(Var2>200,Var2-200,Var2-100)))
df1c <- df1c %>% mutate("Scenario" = ifelse(Scen==30,"Test and treat none",ifelse(Scen==31,"Test none, treat all, no TCZ",ifelse(Scen==32,"RDTs, no TCZ",ifelse(Scen==33,"PCR, no TCZ",ifelse(Scen==34,"RDT, check positives with PCR, no TCZ", ifelse(Scen==35,"RDT, check negatives with PCR, no TCZ",ifelse(Scen==41,"Test none, treat all, TCZ",ifelse(Scen==42,"RDT, TCZ",ifelse(Scen==43,"PCR, TCZ",ifelse(Scen==44,"RDT, check positives with PCR, TCZ", "RDT, check negatives with PCR, TCZ")))))))))))
df1c <- df1c %>% mutate(value = ifelse(Var2>300,value/48,ifelse(Var2>200,value/54,value/27)))
df1c$Scenario <- factor(df1c$Scenario, levels = c("Test and treat none", "Test none, treat all, no TCZ", "RDTs, no TCZ","RDT, check negatives with PCR, no TCZ", "RDT, check positives with PCR, no TCZ", "PCR, no TCZ", "Test none, treat all, TCZ", "RDT, TCZ", "RDT, check negatives with PCR, TCZ","RDT, check positives with PCR, TCZ", "PCR, TCZ"))


IL6plotA <- ggplot(data =df1a, aes(x=Var1, y=value, group=Scen))  +  theme(legend.position="bottom") + geom_line(aes(linetype=Scenario,color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1,5,5,5,5,5)) + scale_color_manual(values=c("#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E","#E7298A","#D95F02", "#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries \n in which scenario \n is cost-effective", x = element_blank()) + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98")) + theme(plot.margin = margin(0,0,0,0, 'cm')) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)

IL6plotB <- ggplot(data =df1b, aes(x=Var1, y=value, group=Scen))  +  theme(legend.position="bottom") + geom_line(aes(linetype=Scenario,color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1,5,5,5,5,5)) + scale_color_manual(values=c("#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E","#E7298A","#D95F02", "#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries \n in which scenario \n is cost-effective", x = element_blank()) + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98")) + theme(plot.margin = margin(0,0,0,0, 'cm')) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)

IL6plotC <- ggplot(data =df1c, aes(x=Var1, y=value, group=Scen))  +  theme(legend.position="bottom") + geom_line(aes(linetype=Scenario,color=Scenario)) + scale_linetype_manual(values=c(1,1,1,1,1,1,5,5,5,5,5)) + scale_color_manual(values=c("#7570B3","#E7298A","#D95F02","#DFC27D","#80CDC1","#01665E","#E7298A","#D95F02", "#DFC27D","#80CDC1","#01665E")) +  labs(y = "% of countries \n in which scenario \n is cost-effective", x = "COVID-19 prevalence among severe suspected COVID cases") + facet_wrap(~Income) + theme(panel.background = element_rect(fill = 'gray98')) + theme(legend.key = element_rect(fill = "gray98")) + theme(plot.margin = margin(0,0,0,0, 'cm')) + scale_y_continuous(labels = scales::percent) + scale_x_continuous(labels = scales::percent)

figure5 <- ggarrange(IL6plotA, IL6plotB, IL6plotC, labels = c("A", "B", "C"),   ncol = 1, nrow = 3,   common.legend = TRUE, legend = "bottom")

figure5

#### Export all plots

ggsave(
   paste0(path.io,"fig2.eps"), 
   figure2, width = 7, height = 5, units = "in", dpi = 300
 )

ggsave(
   paste0(path.io,"fig3.eps"), 
   figure3, width = 7, height = 5, units = "in", dpi = 300
 )

ggsave(
   paste0(path.io,"fig5.eps"), 
   figure5, width = 7, height = 5, units = "in", dpi = 300
 )
