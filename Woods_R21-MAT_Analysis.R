library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)

# setwd("C:/Users/Phil/Dropbox/MBL/Tension + AaBbCc/R21-MAT")
setwd("C:/Users/pcw00/Dropbox/MBL/Tension + AaBbCc/R21-MAT")

my_data <- read_excel("R21-Tension+AkBbCc__PW_R_3-27-23.xlsx",
                      sheet = "Final - Run only") %>% 
  mutate(AgexWeight = as.factor(AgexWeight)) %>% 
  mutate(AgexWeightxSex = as.factor(AgexWeightxSex))

my_data_I <- my_data %>% 
  filter(FiberType == "I") 
my_data_IIA <- my_data %>% 
  filter(FiberType == "IIA")
my_data_IIAX <- my_data %>% 
  filter(FiberType == "IIAX") 

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
I_Force_AxW_mdl <- lmer(Force ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_Force_AxW_emm <- data.frame(emmeans(I_Force_AxW_mdl, specs = "AgexWeight"))
if ((a <- anova(I_Force_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Force_AxW_hoc <- summary(glht(I_Force_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_CSA_AxW_mdl <- lmer(CSA ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_CSA_AxW_emm <- data.frame(emmeans(I_CSA_AxW_mdl, specs = "AgexWeight"))
if ((b <- anova(I_CSA_AxW_mdl)$`Pr(>F)`) <0.05) {  I_CSA_AxW_hoc <- summary(glht(I_CSA_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_Tension_AxW_mdl <- lmer(Tension ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_Tension_AxW_emm <- data.frame(emmeans(I_Tension_AxW_mdl, specs = "AgexWeight"))
if ((c <- anova(I_Tension_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Tension_AxW_hoc <- summary(glht(I_Tension_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_A_AxW_mdl <- lmer(A ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_A_AxW_emm <- data.frame(emmeans(I_A_AxW_mdl, specs = "AgexWeight"))
if ((d <- anova(I_A_AxW_mdl)$`Pr(>F)`) <0.05) {  I_A_AxW_hoc <- summary(glht(I_A_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_k_AxW_mdl <- lmer(k ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_k_AxW_emm <- data.frame(emmeans(I_k_AxW_mdl, specs = "AgexWeight"))
if ((e <- anova(I_k_AxW_mdl)$`Pr(>F)`) <0.05) {  I_k_AxW_hoc <- summary(glht(I_k_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_B_AxW_mdl <- lmer(B ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_B_AxW_emm <- data.frame(emmeans(I_B_AxW_mdl, specs = "AgexWeight"))
if ((f <- anova(I_B_AxW_mdl)$`Pr(>F)`) <0.05) {  I_B_AxW_hoc <- summary(glht(I_B_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_ton_AxW_mdl <- lmer(ton ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_ton_AxW_emm <- data.frame(emmeans(I_ton_AxW_mdl, specs = "AgexWeight"))
if ((g <- anova(I_ton_AxW_mdl)$`Pr(>F)`) <0.05) {  I_ton_AxW_hoc <- summary(glht(I_ton_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_twopib_AxW_mdl <- lmer(twopib ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_twopib_AxW_emm <- data.frame(emmeans(I_twopib_AxW_mdl, specs = "AgexWeight"))
if ((h <- anova(I_twopib_AxW_mdl)$`Pr(>F)`) <0.05) {  I_twopib_AxW_hoc <- summary(glht(I_twopib_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_Aelastic_AxW_mdl <- lmer(Aelastic ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_Aelastic_AxW_emm <- data.frame(emmeans(I_Aelastic_AxW_mdl, specs = "AgexWeight"))
if ((i <- anova(I_Aelastic_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Aelastic_AxW_hoc <- summary(glht(I_Aelastic_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

I_Aviscous_AxW_mdl <- lmer(Aviscous ~ AgexWeight + (1| SubjNum), data = my_data_I)
I_Aviscous_AxW_emm <- data.frame(emmeans(I_Aviscous_AxW_mdl, specs = "AgexWeight"))
if ((j <- anova(I_Aviscous_AxW_mdl)$`Pr(>F)`) <0.05) {  I_Aviscous_AxW_hoc <- summary(glht(I_Aviscous_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

### MHC I: Age x Weight x Sex --------------------------------------------------------------------------------------------------------
I_Force_AxWxS_mdl <- lmer(Force ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_Force_AxWxS_emm <- data.frame(emmeans(I_Force_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((k <- anova(I_Force_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Force_AxWxS_hoc <- summary(glht(I_Force_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_CSA_AxWxS_mdl <- lmer(CSA ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_CSA_AxWxS_emm <- data.frame(emmeans(I_CSA_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((l <- anova(I_CSA_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_CSA_AxWxS_hoc <- summary(glht(I_CSA_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_Tension_AxWxS_mdl <- lmer(Tension ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_Tension_AxWxS_emm <- data.frame(emmeans(I_Tension_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((m <- anova(I_Tension_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Tension_AxWxS_hoc <- summary(glht(I_Tension_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_A_AxWxS_mdl <- lmer(A ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_A_AxWxS_emm <- data.frame(emmeans(I_A_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((n <- anova(I_A_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_A_AxWxS_hoc <- summary(glht(I_A_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_k_AxWxS_mdl <- lmer(k ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_k_AxWxS_emm <- data.frame(emmeans(I_k_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((o <- anova(I_k_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_k_AxWxS_hoc <- summary(glht(I_k_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_B_AxWxS_mdl <- lmer(B ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_B_AxWxS_emm <- data.frame(emmeans(I_B_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((p <- anova(I_B_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_B_AxWxS_hoc <- summary(glht(I_B_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_ton_AxWxS_mdl <- lmer(ton ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_ton_AxWxS_emm <- data.frame(emmeans(I_ton_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((q <- anova(I_ton_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_ton_AxWxS_hoc <- summary(glht(I_ton_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_twopib_AxWxS_mdl <- lmer(twopib ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_twopib_AxWxS_emm <- data.frame(emmeans(I_twopib_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((r <- anova(I_twopib_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_twopib_AxWxS_hoc <- summary(glht(I_twopib_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_Aelastic_AxWxS_mdl <- lmer(Aelastic ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_Aelastic_AxWxS_emm <- data.frame(emmeans(I_Aelastic_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((s <- anova(I_Aelastic_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Aelastic_AxWxS_hoc <- summary(glht(I_Aelastic_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

I_Aviscous_AxWxS_mdl <- lmer(Aviscous ~ AgexWeightxSex + (1| SubjNum), data = my_data_I)
I_Aviscous_AxWxS_emm <- data.frame(emmeans(I_Aviscous_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((t <- anova(I_Aviscous_AxWxS_mdl)$`Pr(>F)`) <0.05) {  I_Aviscous_AxWxS_hoc <- summary(glht(I_Aviscous_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

### MHC IIA : Age x Weight-------------------------------------------------------------------------------------------------------
IIA_Force_AxW_mdl <- lmer(Force ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_Force_AxW_emm <- data.frame(emmeans(IIA_Force_AxW_mdl, specs = "AgexWeight"))
if ((a1 <- anova(IIA_Force_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Force_AxW_hoc <- summary(glht(IIA_Force_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_CSA_AxW_mdl <- lmer(CSA ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_CSA_AxW_emm <- data.frame(emmeans(IIA_CSA_AxW_mdl, specs = "AgexWeight"))
if ((b1 <- anova(IIA_CSA_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_CSA_AxW_hoc <- summary(glht(IIA_CSA_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_Tension_AxW_mdl <- lmer(Tension ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_Tension_AxW_emm <- data.frame(emmeans(IIA_Tension_AxW_mdl, specs = "AgexWeight"))
if ((c1 <- anova(IIA_Tension_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Tension_AxW_hoc <- summary(glht(IIA_Tension_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_A_AxW_mdl <- lmer(A ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_A_AxW_emm <- data.frame(emmeans(IIA_A_AxW_mdl, specs = "AgexWeight"))
if ((d1 <- anova(IIA_A_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_A_AxW_hoc <- summary(glht(IIA_A_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_k_AxW_mdl <- lmer(k ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_k_AxW_emm <- data.frame(emmeans(IIA_k_AxW_mdl, specs = "AgexWeight"))
if ((e1 <- anova(IIA_k_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_k_AxW_hoc <- summary(glht(IIA_k_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_B_AxW_mdl <- lmer(B ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_B_AxW_emm <- data.frame(emmeans(IIA_B_AxW_mdl, specs = "AgexWeight"))
if ((f1 <- anova(IIA_B_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_B_AxW_hoc <- summary(glht(IIA_B_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_ton_AxW_mdl <- lmer(ton ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_ton_AxW_emm <- data.frame(emmeans(IIA_ton_AxW_mdl, specs = "AgexWeight"))
if ((g1 <- anova(IIA_ton_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_ton_AxW_hoc <- summary(glht(IIA_ton_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_twopib_AxW_mdl <- lmer(twopib ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_twopib_AxW_emm <- data.frame(emmeans(IIA_twopib_AxW_mdl, specs = "AgexWeight"))
if ((h1 <- anova(IIA_twopib_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_twopib_AxW_hoc <- summary(glht(IIA_twopib_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_Aelastic_AxW_mdl <- lmer(Aelastic ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_Aelastic_AxW_emm <- data.frame(emmeans(IIA_Aelastic_AxW_mdl, specs = "AgexWeight"))
if ((i1 <- anova(IIA_Aelastic_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Aelastic_AxW_hoc <- summary(glht(IIA_Aelastic_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIA_Aviscous_AxW_mdl <- lmer(Aviscous ~ AgexWeight + (1| SubjNum), data = my_data_IIA)
IIA_Aviscous_AxW_emm <- data.frame(emmeans(IIA_Aviscous_AxW_mdl, specs = "AgexWeight"))
if ((j1 <- anova(IIA_Aviscous_AxW_mdl)$`Pr(>F)`) <0.05) {  IIA_Aviscous_AxW_hoc <- summary(glht(IIA_Aviscous_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

### MHC IIA: Age x Weight x Sex ------------------------------------------------------------------------------------------
IIA_Force_AxWxS_mdl <- lmer(Force ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_Force_AxWxS_emm <- data.frame(emmeans(IIA_Force_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((k1 <- anova(IIA_Force_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Force_AxWxS_hoc <- summary(glht(IIA_Force_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_CSA_AxWxS_mdl <- lmer(CSA ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_CSA_AxWxS_emm <- data.frame(emmeans(IIA_CSA_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((l1 <- anova(IIA_CSA_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_CSA_AxWxS_hoc <- summary(glht(IIA_CSA_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_Tension_AxWxS_mdl <- lmer(Tension ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_Tension_AxWxS_emm <- data.frame(emmeans(IIA_Tension_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((m1 <- anova(IIA_Tension_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Tension_AxWxS_hoc <- summary(glht(IIA_Tension_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_A_AxWxS_mdl <- lmer(A ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_A_AxWxS_emm <- data.frame(emmeans(IIA_A_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((n1 <- anova(IIA_A_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_A_AxWxS_hoc <- summary(glht(IIA_A_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_k_AxWxS_mdl <- lmer(k ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_k_AxWxS_emm <- data.frame(emmeans(IIA_k_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((o1 <- anova(IIA_k_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_k_AxWxS_hoc <- summary(glht(IIA_k_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_B_AxWxS_mdl <- lmer(B ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_B_AxWxS_emm <- data.frame(emmeans(IIA_B_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((p1 <- anova(IIA_B_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_B_AxWxS_hoc <- summary(glht(IIA_B_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_ton_AxWxS_mdl <- lmer(ton ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_ton_AxWxS_emm <- data.frame(emmeans(IIA_ton_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((q1 <- anova(IIA_ton_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_ton_AxWxS_hoc <- summary(glht(IIA_ton_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_twopib_AxWxS_mdl <- lmer(twopib ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_twopib_AxWxS_emm <- data.frame(emmeans(IIA_twopib_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((r1 <- anova(IIA_twopib_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_twopib_AxWxS_hoc <- summary(glht(IIA_twopib_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_Aelastic_AxWxS_mdl <- lmer(Aelastic ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_Aelastic_AxWxS_emm <- data.frame(emmeans(IIA_Aelastic_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((s1 <- anova(IIA_Aelastic_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Aelastic_AxWxS_hoc <- summary(glht(IIA_Aelastic_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIA_Aviscous_AxWxS_mdl <- lmer(Aviscous ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIA)
IIA_Aviscous_AxWxS_emm <- data.frame(emmeans(IIA_Aviscous_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((t1 <- anova(IIA_Aviscous_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIA_Aviscous_AxWxS_hoc <- summary(glht(IIA_Aviscous_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

### MHC IIAX: Age x Weight --------------------------------------------------------------------------------------------
IIAX_Force_AxW_mdl <- lmer(Force ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_Force_AxW_emm <- data.frame(emmeans(IIAX_Force_AxW_mdl, specs = "AgexWeight"))
if ((a2 <- anova(IIAX_Force_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_Force_AxW_hoc <- summary(glht(IIAX_Force_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_CSA_AxW_mdl <- lmer(CSA ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_CSA_AxW_emm <- data.frame(emmeans(IIAX_CSA_AxW_mdl, specs = "AgexWeight"))
if ((b2 <- anova(IIAX_CSA_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_CSA_AxW_hoc <- summary(glht(IIAX_CSA_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_Tension_AxW_mdl <- lmer(Tension ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_Tension_AxW_emm <- data.frame(emmeans(IIAX_Tension_AxW_mdl, specs = "AgexWeight"))
if ((c2 <- anova(IIAX_Tension_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_Tension_AxW_hoc <- summary(glht(IIAX_Tension_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_A_AxW_mdl <- lmer(A ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_A_AxW_emm <- data.frame(emmeans(IIAX_A_AxW_mdl, specs = "AgexWeight"))
if ((d2 <- anova(IIAX_A_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_A_AxW_hoc <- summary(glht(IIAX_A_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_k_AxW_mdl <- lmer(k ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_k_AxW_emm <- data.frame(emmeans(IIAX_k_AxW_mdl, specs = "AgexWeight"))
if ((e2 <- anova(IIAX_k_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_k_AxW_hoc <- summary(glht(IIAX_k_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_B_AxW_mdl <- lmer(B ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_B_AxW_emm <- data.frame(emmeans(IIAX_B_AxW_mdl, specs = "AgexWeight"))
if ((f2 <- anova(IIAX_B_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_B_AxW_hoc <- summary(glht(IIAX_B_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_ton_AxW_mdl <- lmer(ton ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_ton_AxW_emm <- data.frame(emmeans(IIAX_ton_AxW_mdl, specs = "AgexWeight"))
if ((g2 <- anova(IIAX_ton_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_ton_AxW_hoc <- summary(glht(IIAX_ton_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_twopib_AxW_mdl <- lmer(twopib ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_twopib_AxW_emm <- data.frame(emmeans(IIAX_twopib_AxW_mdl, specs = "AgexWeight"))
if ((h2 <- anova(IIAX_twopib_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_twopib_AxW_hoc <- summary(glht(IIAX_twopib_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_Aelastic_AxW_mdl <- lmer(Aelastic ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_Aelastic_AxW_emm <- data.frame(emmeans(IIAX_Aelastic_AxW_mdl, specs = "AgexWeight"))
if ((i2 <- anova(IIAX_Aelastic_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_Aelastic_AxW_hoc <- summary(glht(IIAX_Aelastic_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

IIAX_Aviscous_AxW_mdl <- lmer(Aviscous ~ AgexWeight + (1| SubjNum), data = my_data_IIAX)
IIAX_Aviscous_AxW_emm <- data.frame(emmeans(IIAX_Aviscous_AxW_mdl, specs = "AgexWeight"))
if ((j2 <- anova(IIAX_Aviscous_AxW_mdl)$`Pr(>F)`) <0.05) {  IIAX_Aviscous_AxW_hoc <- summary(glht(IIAX_Aviscous_AxW_mdl, linfct = mcp(AgexWeight = "Tukey")))}else{NA}

### MHC IIAX: Age x Weight x Sex ---------------------------------------------------------------
IIAX_Force_AxWxS_mdl <- lmer(Force ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_Force_AxWxS_emm <- data.frame(emmeans(IIAX_Force_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((k2 <- anova(IIAX_Force_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_Force_AxWxS_hoc <- summary(glht(IIAX_Force_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_CSA_AxWxS_mdl <- lmer(CSA ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_CSA_AxWxS_emm <- data.frame(emmeans(IIAX_CSA_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((l2 <- anova(IIAX_CSA_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_CSA_AxWxS_hoc <- summary(glht(IIAX_CSA_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_Tension_AxWxS_mdl <- lmer(Tension ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_Tension_AxWxS_emm <- data.frame(emmeans(IIAX_Tension_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((m2 <- anova(IIAX_Tension_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_Tension_AxWxS_hoc <- summary(glht(IIAX_Tension_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_A_AxWxS_mdl <- lmer(A ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_A_AxWxS_emm <- data.frame(emmeans(IIAX_A_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((n2 <- anova(IIAX_A_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_A_AxWxS_hoc <- summary(glht(IIAX_A_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_k_AxWxS_mdl <- lmer(k ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_k_AxWxS_emm <- data.frame(emmeans(IIAX_k_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((o2 <- anova(IIAX_k_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_k_AxWxS_hoc <- summary(glht(IIAX_k_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_B_AxWxS_mdl <- lmer(B ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_B_AxWxS_emm <- data.frame(emmeans(IIAX_B_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((p2 <- anova(IIAX_B_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_B_AxWxS_hoc <- summary(glht(IIAX_B_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_ton_AxWxS_mdl <- lmer(ton ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_ton_AxWxS_emm <- data.frame(emmeans(IIAX_ton_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((q2 <- anova(IIAX_ton_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_ton_AxWxS_hoc <- summary(glht(IIAX_ton_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_twopib_AxWxS_mdl <- lmer(twopib ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_twopib_AxWxS_emm <- data.frame(emmeans(IIAX_twopib_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((r2 <- anova(IIAX_twopib_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_twopib_AxWxS_hoc <- summary(glht(IIAX_twopib_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_Aelastic_AxWxS_mdl <- lmer(Aelastic ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_Aelastic_AxWxS_emm <- data.frame(emmeans(IIAX_Aelastic_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((s2 <- anova(IIAX_Aelastic_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_Aelastic_AxWxS_hoc <- summary(glht(IIAX_Aelastic_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}

IIAX_Aviscous_AxWxS_mdl <- lmer(Aviscous ~ AgexWeightxSex + (1| SubjNum), data = my_data_IIAX)
IIAX_Aviscous_AxWxS_emm <- data.frame(emmeans(IIAX_Aviscous_AxWxS_mdl, specs = "AgexWeightxSex"))
if ((t2 <- anova(IIAX_Aviscous_AxWxS_mdl)$`Pr(>F)`) <0.05) {  IIAX_Aviscous_AxWxS_hoc <- summary(glht(IIAX_Aviscous_AxWxS_mdl, linfct = mcp(AgexWeightxSex = "Tukey")))}else{NA}


### Exporting Data --------------------------------------------------------------------------------------------

mhcI_AxW_emm <- rbind(I_Force_AxW_emm,
                  I_CSA_AxW_emm,
                  I_Tension_AxW_emm,
                  I_A_AxW_emm,
                  I_k_AxW_emm,
                  I_B_AxW_emm,
                  I_ton_AxW_emm,
                  I_twopib_AxW_emm,
                  I_Aelastic_AxW_emm,
                  I_Aviscous_AxW_emm) %>% 
  mutate(Value = c("Force","Force","Force",
                   "CSA","CSA","CSA",
                   "Tension","Tension","Tension",
                   "A","A","A",
                   "k","k","k",
                   "B","B","B",
                   "ton","ton","ton",
                   "2pib","2pib","2pib",
                   "Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous"), .before = emmean) %>% 
  mutate(AgexWeight = c("Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal")) %>% 
  mutate(MHC = "I", .before = emmean)

mhcI_AxWxS_emm <- rbind(I_Force_AxWxS_emm,
                  I_CSA_AxWxS_emm,
                  I_Tension_AxWxS_emm,
                  I_A_AxWxS_emm,
                  I_k_AxWxS_emm,
                  I_B_AxWxS_emm,
                  I_ton_AxWxS_emm,
                  I_twopib_AxWxS_emm,
                  I_Aelastic_AxWxS_emm,
                  I_Aviscous_AxWxS_emm) %>% 
  mutate(Value = c("Force","Force","Force","Force","Force","Force",
                   "CSA","CSA","CSA", "CSA","CSA","CSA",
                   "Tension","Tension","Tension","Tension","Tension","Tension",
                   "A","A","A","A","A","A",
                   "k","k","k", "k","k","k",
                   "B","B","B","B","B","B",
                   "ton","ton","ton","ton","ton","ton",
                   "2pib","2pib","2pib","2pib","2pib","2pib",
                   "Aelastic","Aelastic","Aelastic","Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous"), .before = emmean) %>% 
  mutate(AgexWeightxSex = c("Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                        "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female"))%>% 
  mutate(MHC = "I", .before = emmean)



mhcI_anova <- data.frame(rbind(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t)) %>% 
  mutate(MHC = "I") %>% 
  mutate(Value = c("Force", "CSA", "Tension", "A", "k", "B", "ton", "2pib", "Aelastic", "Aviscous",
                   "Force", "CSA", "Tension", "A", "k", "B", "ton", "2pib", "Aelastic", "Aviscous")) %>% 
  mutate(Condition = c("Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight",
                       "Age x Weight x Sex", "Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex"))
colnames(mhcI_anova) <- c("p-value", "MHC", "Value", "Condition")





mhcIIA_AxW_emm <- rbind(IIA_Force_AxW_emm,
                      IIA_CSA_AxW_emm,
                      IIA_Tension_AxW_emm,
                      IIA_A_AxW_emm,
                      IIA_k_AxW_emm,
                      IIA_B_AxW_emm,
                      IIA_ton_AxW_emm,
                      IIA_twopib_AxW_emm,
                      IIA_Aelastic_AxW_emm,
                      IIA_Aviscous_AxW_emm) %>% 
  mutate(Value = c("Force","Force","Force",
                   "CSA","CSA","CSA",
                   "Tension","Tension","Tension",
                   "A","A","A",
                   "k","k","k",
                   "B","B","B",
                   "ton","ton","ton",
                   "2pib","2pib","2pib",
                   "Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous"), .before = emmean) %>% 
  mutate(AgexWeight = c("Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal")) %>% 
  mutate(MHC = "IIA", .before = emmean)

mhcIIA_AxWxS_emm <- rbind(IIA_Force_AxWxS_emm,
                        IIA_CSA_AxWxS_emm,
                        IIA_Tension_AxWxS_emm,
                        IIA_A_AxWxS_emm,
                        IIA_k_AxWxS_emm,
                        IIA_B_AxWxS_emm,
                        IIA_ton_AxWxS_emm,
                        IIA_twopib_AxWxS_emm,
                        IIA_Aelastic_AxWxS_emm,
                        IIA_Aviscous_AxWxS_emm) %>% 
  mutate(Value = c("Force","Force","Force","Force","Force","Force",
                   "CSA","CSA","CSA", "CSA","CSA","CSA",
                   "Tension","Tension","Tension","Tension","Tension","Tension",
                   "A","A","A","A","A","A",
                   "k","k","k", "k","k","k",
                   "B","B","B","B","B","B",
                   "ton","ton","ton","ton","ton","ton",
                   "2pib","2pib","2pib","2pib","2pib","2pib",
                   "Aelastic","Aelastic","Aelastic","Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous"), .before = emmean) %>% 
  mutate(AgexWeightxSex = c("Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female"))%>% 
  mutate(MHC = "IIA", .before = emmean)

mhcIIA_anova <- data.frame(rbind(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1,o1,p1,q1,r1,s1,t1)) %>% 
  mutate(MHC = "IIA") %>% 
  mutate(Value = c("Force", "CSA", "Tension", "A", "k", "B", "ton", "2pib", "Aelastic", "Aviscous",
                   "Force", "CSA", "Tension", "A", "k", "B", "ton", "2pib", "Aelastic", "Aviscous")) %>% 
  mutate(Condition = c("Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight",
                       "Age x Weight x Sex", "Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex"))
colnames(mhcIIA_anova) <- c("p-value", "MHC", "Value", "Condition")





mhcIIAX_AxW_emm <- rbind(IIAX_Force_AxW_emm,
                        IIAX_CSA_AxW_emm,
                        IIAX_Tension_AxW_emm,
                        IIAX_A_AxW_emm,
                        IIAX_k_AxW_emm,
                        IIAX_B_AxW_emm,
                        IIAX_ton_AxW_emm,
                        IIAX_twopib_AxW_emm,
                        IIAX_Aelastic_AxW_emm,
                        IIAX_Aviscous_AxW_emm) %>% 
  mutate(Value = c("Force","Force","Force",
                   "CSA","CSA","CSA",
                   "Tension","Tension","Tension",
                   "A","A","A",
                   "k","k","k",
                   "B","B","B",
                   "ton","ton","ton",
                   "2pib","2pib","2pib",
                   "Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous"), .before = emmean) %>% 
  mutate(AgexWeight = c("Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal",
                        "Young Normal", "Young Obese", "Older Normal")) %>% 
  mutate(MHC = "IIAX", .before = emmean)

mhcIIAX_AxWxS_emm <- rbind(IIAX_Force_AxWxS_emm,
                          IIAX_CSA_AxWxS_emm,
                          IIAX_Tension_AxWxS_emm,
                          IIAX_A_AxWxS_emm,
                          IIAX_k_AxWxS_emm,
                          IIAX_B_AxWxS_emm,
                          IIAX_ton_AxWxS_emm,
                          IIAX_twopib_AxWxS_emm,
                          IIAX_Aelastic_AxWxS_emm,
                          IIAX_Aviscous_AxWxS_emm) %>% 
  mutate(Value = c("Force","Force","Force","Force","Force","Force",
                   "CSA","CSA","CSA", "CSA","CSA","CSA",
                   "Tension","Tension","Tension","Tension","Tension","Tension",
                   "A","A","A","A","A","A",
                   "k","k","k", "k","k","k",
                   "B","B","B","B","B","B",
                   "ton","ton","ton","ton","ton","ton",
                   "2pib","2pib","2pib","2pib","2pib","2pib",
                   "Aelastic","Aelastic","Aelastic","Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous"), .before = emmean) %>% 
  mutate(AgexWeightxSex = c("Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female",
                            "Young Normal Male","Young Normal Female", "Young Obese Male","Young Obese Female", "Older Normal Male","Older Normal Female"))%>% 
  mutate(MHC = "IIAX", .before = emmean)

mhcIIAX_anova <- data.frame(rbind(a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2,o2,p2,q2,r2,s2,t2)) %>% 
  mutate(MHC = "IIAX") %>% 
  mutate(Value = c("Force", "CSA", "Tension", "A", "k", "B", "ton", "2pib", "Aelastic", "Aviscous",
                   "Force", "CSA", "Tension", "A", "k", "B", "ton", "2pib", "Aelastic", "Aviscous")) %>% 
  mutate(Condition = c("Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight","Age x Weight",
                       "Age x Weight x Sex", "Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex","Age x Weight x Sex"))
colnames(mhcIIAX_anova) <- c("p-value", "MHC", "Value", "Condition")


posthoc <- rbind(table_glht(IIA_Force_AxWxS_hoc),
                 table_glht(IIA_CSA_AxW_emm),
                 table_glht(IIAX_Force_AxW_hoc),
                 table_glht(IIAX_Force_AxWxS_hoc),
                 table_glht(IIAX_CSA_AxW_hoc),
                 table_glht(IIAX_CSA_AxWxS_hoc),
                 table_glht(IIAX_k_AxW_hoc),
                 table_glht(IIAX_B_AxW_hoc))  
  # mutate(FiberType = c("I"))
