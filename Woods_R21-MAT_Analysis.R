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

my_data_I <- my_data %>% 
  filter(FiberType == "I") 
my_data_IIA <- my_data %>% 
  filter(FiberType == "IIA") %>% 
  mutate(AgexWeightxSex = as.factor(AgexWeightxSex))
my_data_IIAX <- my_data %>% 
  filter(FiberType == "IIAX") %>% 
  mutate(AgexWeightxSex = as.factor(AgexWeightxSex))
### MHC I: Age x Weight --------------------------------------------------
I_Force_AxW_mdl <- lmer(Force ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_Force_AxW_emm <- emmeans(I_Force_AxW_mdl, specs = "AgexWeight")
if ((a <- anova(I_Force_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Force_AxW_hoc <- summary(glht(I_Force_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_CSA_AxW_mdl <- lmer(CSA ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_I)
I_CSA_AxW_emm <- emmeans(I_CSA_AxW_mdl, specs = "AgexWeight")
if ((b <- anova(I_CSA_AxW_mdl)$`Pr(>F)`) <0.05) {  I_CSA_AxW_hoc <- summary(glht(I_CSA_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

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

### MHC I: Age x Weight x Sex --------------------------------------------------------------------------------------------------------
I_Force_AxWxS_mdl <- lmer(Force ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_Force_AxWxS_emm <- emmeans(I_Force_AxWxS_mdl, specs = "AgexWeightxSex")
if ((k <- anova(I_Force_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Force_AxWxS_hoc <- summary(glht(I_Force_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_CSA_AxWxS_mdl <- lmer(CSA ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_CSA_AxWxS_emm <- emmeans(I_CSA_AxWxS_mdl, specs = "AgexWeightxSex")
if ((l <- anova(I_CSA_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_CSA_AxWxS_hoc <- summary(glht(I_CSA_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_Tension_AxWxS_mdl <- lmer(Tension ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_Tension_AxWxS_emm <- emmeans(I_Tension_AxWxS_mdl, specs = "AgexWeightxSex")
if ((m <- anova(I_Tension_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Tension_AxWxS_hoc <- summary(glht(I_Tension_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_A_AxWxS_mdl <- lmer(A ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_A_AxWxS_emm <- emmeans(I_A_AxWxS_mdl, specs = "AgexWeightxSex")
if ((n <- anova(I_A_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_A_AxWxS_hoc <- summary(glht(I_A_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_k_AxWxS_mdl <- lmer(k ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_k_AxWxS_emm <- emmeans(I_k_AxWxS_mdl, specs = "AgexWeightxSex")
if ((o <- anova(I_k_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_k_AxWxS_hoc <- summary(glht(I_k_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_B_AxWxS_mdl <- lmer(B ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_B_AxWxS_emm <- emmeans(I_B_AxWxS_mdl, specs = "AgexWeightxSex")
if ((p <- anova(I_B_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_B_AxWxS_hoc <- summary(glht(I_B_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_ton_AxWxS_mdl <- lmer(ton ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_ton_AxWxS_emm <- emmeans(I_ton_AxWxS_mdl, specs = "AgexWeightxSex")
if ((q <- anova(I_ton_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_ton_AxWxS_hoc <- summary(glht(I_ton_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_twopib_AxWxS_mdl <- lmer(twopib ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_twopib_AxWxS_emm <- emmeans(I_twopib_AxWxS_mdl, specs = "AgexWeightxSex")
if ((r <- anova(I_twopib_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_twopib_AxWxS_hoc <- summary(glht(I_twopib_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_Aelastic_AxWxS_mdl <- lmer(Aelastic ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_Aelastic_AxWxS_emm <- emmeans(I_Aelastic_AxWxS_mdl, specs = "AgexWeightxSex")
if ((s <- anova(I_Aelastic_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Aelastic_AxWxS_hoc <- summary(glht(I_Aelastic_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

I_Aviscous_AxWxS_mdl <- lmer(Aviscous ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_I)
I_Aviscous_AxWxS_emm <- emmeans(I_Aviscous_AxWxS_mdl, specs = "AgexWeightxSex")
if ((t <- anova(I_Aviscous_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Aviscous_AxWxS_hoc <- summary(glht(I_Aviscous_AxWxS_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

### MHC IIA : Age x Weight-------------------------------------------------------------------------------------------------------
IIA_Force_AxW_mdl <- lmer(Force ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_Force_AxW_emm <- emmeans(IIA_Force_AxW_mdl, specs = "AgexWeight")
if ((a <- anova(IIA_Force_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Force_AxW_hoc <- summary(glht(IIA_Force_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_CSA_AxW_mdl <- lmer(CSA ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_CSA_AxW_emm <- emmeans(IIA_CSA_AxW_mdl, specs = "AgexWeight")
if ((b <- anova(IIA_CSA_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_CSA_AxW_hoc <- summary(glht(IIA_CSA_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_Tension_AxW_mdl <- lmer(Tension ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_Tension_AxW_emm <- emmeans(IIA_Tension_AxW_mdl, specs = "AgexWeight")
if ((c <- anova(IIA_Tension_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Tension_AxW_hoc <- summary(glht(IIA_Tension_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_A_AxW_mdl <- lmer(A ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_A_AxW_emm <- emmeans(IIA_A_AxW_mdl, specs = "AgexWeight")
if ((d <- anova(IIA_A_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_A_AxW_hoc <- summary(glht(IIA_A_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_k_AxW_mdl <- lmer(k ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_k_AxW_emm <- emmeans(IIA_k_AxW_mdl, specs = "AgexWeight")
if ((e <- anova(IIA_k_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_k_AxW_hoc <- summary(glht(IIA_k_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_B_AxW_mdl <- lmer(B ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_B_AxW_emm <- emmeans(IIA_B_AxW_mdl, specs = "AgexWeight")
if ((f <- anova(IIA_B_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_B_AxW_hoc <- summary(glht(IIA_B_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_ton_AxW_mdl <- lmer(ton ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_ton_AxW_emm <- emmeans(IIA_ton_AxW_mdl, specs = "AgexWeight")
if ((g <- anova(IIA_ton_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_ton_AxW_hoc <- summary(glht(IIA_ton_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_twopib_AxW_mdl <- lmer(twopib ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_twopib_AxW_emm <- emmeans(IIA_twopib_AxW_mdl, specs = "AgexWeight")
if ((h <- anova(IIA_twopib_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_twopib_AxW_hoc <- summary(glht(IIA_twopib_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_Aelastic_AxW_mdl <- lmer(Aelastic ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_Aelastic_AxW_emm <- emmeans(IIA_Aelastic_AxW_mdl, specs = "AgexWeight")
if ((i <- anova(IIA_Aelastic_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Aelastic_AxW_hoc <- summary(glht(IIA_Aelastic_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

IIA_Aviscous_AxW_mdl <- lmer(Aviscous ~ as.factor(AgexWeight) + (1| SubjNum), data = my_data_IIA)
IIA_Aviscous_AxW_emm <- emmeans(IIA_Aviscous_AxW_mdl, specs = "AgexWeight")
if ((j <- anova(IIA_Aviscous_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Aviscous_AxW_hoc <- summary(glht(IIA_Aviscous_AxW_mdl, linfct = mcp(ExpCond = "Tukey")))}else{NA}

### MHC IIA: Age x Weight x Sex ------------------------------------------------------------------------------------------
IIA_Force_AxWxS_mdl <- lmer(Force ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
IIA_Force_AxWxS_emm <- emmeans(IIA_Force_AxWxS_mdl, specs = "AgexWeightxSex")
if ((u <- anova(IIA_Force_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Force_AxWxS_hoc <- summary(glht(IIA_Force_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}


# having difficulty with the posthoc test.

# IIA_CSA_AxWxS_mdl <- lmer(CSA ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_CSA_AxWxS_emm <- emmeans(IIA_CSA_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((v <- anova(IIA_CSA_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_CSA_AxWxS_hoc <- summary(glht(IIA_CSA_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_Tension_AxWxS_mdl <- lmer(Tension ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_Tension_AxWxS_emm <- emmeans(IIA_Tension_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((w <- anova(IIA_Tension_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Tension_AxWxS_hoc <- summary(glht(IIA_Tension_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_A_AxWxS_mdl <- lmer(A ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_A_AxWxS_emm <- emmeans(IIA_A_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((x <- anova(IIA_A_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_A_AxWxS_hoc <- summary(glht(IIA_A_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_k_AxWxS_mdl <- lmer(k ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_k_AxWxS_emm <- emmeans(IIA_k_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((y <- anova(IIA_k_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_k_AxWxS_hoc <- summary(glht(IIA_k_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_B_AxWxS_mdl <- lmer(B ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_B_AxWxS_emm <- emmeans(IIA_B_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((z <- anova(IIA_B_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_B_AxWxS_hoc <- summary(glht(IIA_B_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_ton_AxWxS_mdl <- lmer(ton ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_ton_AxWxS_emm <- emmeans(IIA_ton_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((zz <- anova(IIA_ton_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_ton_AxWxS_hoc <- summary(glht(IIA_ton_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_twopib_AxWxS_mdl <- lmer(twopib ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_twopib_AxWxS_emm <- emmeans(IIA_twopib_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((zzz <- anova(IIA_twopib_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_twopib_AxWxS_hoc <- summary(glht(IIA_twopib_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_Aelastic_AxWxS_mdl <- lmer(Aelastic ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_Aelastic_AxWxS_emm <- emmeans(IIA_Aelastic_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((zzzz <- anova(IIA_Aelastic_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Aelastic_AxWxS_hoc <- summary(glht(IIA_Aelastic_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
# IIA_Aviscous_AxWxS_mdl <- lmer(Aviscous ~ as.factor(AgexWeightxSex) + (1| SubjNum), data = my_data_IIA)
# IIA_Aviscous_AxWxS_emm <- emmeans(IIA_Aviscous_AxWxS_mdl, specs = "AgexWeightxSex")
# if ((zzzzz <- anova(IIA_Aviscous_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Aviscous_AxWxS_hoc <- summary(glht(IIA_Aviscous_AxWxS_mdl, linfct = mcp(as.factor(AgexWeightxSex) = "Tukey")))}else{NA}
# 
