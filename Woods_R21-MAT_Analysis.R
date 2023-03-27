library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)

setwd("C:/Users/Phil/Dropbox/MBL/Tension + AaBbCc/R21-MAT")

my_data <- read_excel("R21-Tension+AkBbCc__PW_R_3-27-23.xlsx",
                      sheet = "Final - Run only")

table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""),
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
  
}
### MHC I: Age x Weight --------------------------------------------------
my_data_I <- my_data %>% 
  filter(FiberType == "I")

I_Force_AxW_mdl <- lmer(Force ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_Force_AxW_emm <- emmeans(I_Force_mdl, specs = "AgexWeight")
if ((a <- anova(I_Force_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Force_AxW_hoc <- summary(glht(I_Force_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_CSA_AxW_mdl <- lmer(CSA ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_CSA_AxW_emm <- emmeans(I_CSA_AxW_mdl, specs = "AgexWeight")
if ((b <- anova(I_CSA_AxW_mdl)$`Pr(>F)`) <0.05) {  I_CSA_AxW_hoc <- summary(glht(I_CSA_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_Tension_AxW_mdl <- lmer(Tension ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_Tension_AxW_emm <- emmeans(I_Tension_AxW_mdl, specs = "AgexWeight")
if ((c <- anova(I_Tension_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Tension_AxW_hoc <- summary(glht(I_Tension_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_A_AxW_mdl <- lmer(A ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_A_AxW_emm <- emmeans(I_A_AxW_mdl, specs = "AgexWeight")
if ((d <- anova(I_A_AxW_mdl)$`Pr(>F)`) <0.05) {  I_A_AxW_hoc <- summary(glht(I_A_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_k_AxW_mdl <- lmer(k ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_k_AxW_emm <- emmeans(I_k_AxW_mdl, specs = "AgexWeight")
if ((e <- anova(I_k_AxW_mdl)$`Pr(>F)`) <0.05) {  I_k_AxW_hoc <- summary(glht(I_k_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_B_AxW_mdl <- lmer(B ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_B_AxW_emm <- emmeans(I_B_AxW_mdl, specs = "AgexWeight")
if ((f <- anova(I_B_AxW_mdl)$`Pr(>F)`) <0.05) {  I_B_AxW_hoc <- summary(glht(I_B_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_ton_AxW_mdl <- lmer(ton ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_ton_AxW_emm <- emmeans(I_ton_AxW_mdl, specs = "AgexWeight")
if ((g <- anova(I_ton_AxW_mdl)$`Pr(>F)`) <0.05) {  I_ton_AxW_hoc <- summary(glht(I_ton_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_twopib_AxW_mdl <- lmer(twopib ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_twopib_AxW_emm <- emmeans(I_twopib_AxW_mdl, specs = "AgexWeight")
if ((h <- anova(I_twopib_AxW_mdl)$`Pr(>F)`) <0.05) {  I_twopib_AxW_hoc <- summary(glht(I_twopib_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_Aelastic_AxW_mdl <- lmer(Aelastic ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_Aelastic_AxW_emm <- emmeans(I_Aelastic_AxW_mdl, specs = "AgexWeight")
if ((i <- anova(I_Aelastic_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Aelastic_AxW_hoc <- summary(glht(I_Aelastic_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_Aviscous_AxW_mdl <- lmer(Aviscous ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_Aviscous_AxW_emm <- emmeans(I_Aviscous_AxW_mdl, specs = "AgexWeight")
if ((j <- anova(I_Aviscous_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Aviscous_AxW_hoc <- summary(glht(I_Aviscous_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}
