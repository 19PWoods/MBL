library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)

setwd("C:/Users/Phil/Dropbox/MBL/Aurora Fatigue Data")


my_data <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) %>% 
  select(SubjectNum, AgeGrp, Sex,Leg, FiberType,Fibertypenum,Fibernum,ExpCond,ExpCondnum,
         CSA,Force,PoControl25C,MSRunNum, AkNm2,k,BkNm2,lbHz,CkNm2,lcHz,ton,twopib)

### MHC I: Control vs Fatigue vs Fatigue dATP-----------------------------------------------------------------

I <- my_data %>% filter(FiberType == "I")

I_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Po_emm <- emmeans(I_Po_lm, specs = "ExpCond")  
if (anova(I_Po_lm)$`Pr(>F)` <0.5) {
 
   I_Po_hoc <- summary(glht(I_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
   } else{
     NA
}

I_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Force_emm <- emmeans(I_Force_lm, specs = "ExpCond")  
if (anova(I_Force_lm)$`Pr(>F)` <0.5) {
  
  I_Force_hoc <- summary(glht(I_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Force_emm <- emmeans(I_Force_lm, specs = "ExpCond")  
if (anova(I_Force_lm)$`Pr(>F)` <0.5) {
  
  I_Force_hoc <- summary(glht(I_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}
