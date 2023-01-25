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
  mutate(Force = CSA*PoControl25C)
  # select(SubjectNum, AgeGrp, Sex,Leg, FiberType,Fibertypenum,Fibernum,ExpCond,ExpCondnum,
  #        CSA,Force,PoControl25C,MSRunNum, AkNm2,k,BkNm2,lbHz,CkNm2,lcHz,ton,twopib)

### Figure 1-----------------------------------------------------------------

I <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 


I_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Po_emm <- data.frame(emmeans(I_Po_lm, specs = "ExpCond"))  
if (anova(I_Po_lm)$`Pr(>F)` <0.05) {
 
   I_Po_hoc <- summary(glht(I_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
   } else{
     NA
}

I_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Force_emm <- data.frame(emmeans(I_Force_lm, specs = "ExpCond"))  
if (anova(I_Force_lm)$`Pr(>F)` <0.05) {
  
  I_Force_hoc <- summary(glht(I_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum == 4) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

I.IIA_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA)
I.IIA_Po_emm <- data.frame(emmeans(I.IIA_Po_lm, specs = "ExpCond"))  
if (anova(I.IIA_Po_lm)$`Pr(>F)` <0.05) {
  
  I.IIA_Po_hoc <- summary(glht(I.IIA_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA)
I.IIA_Force_emm <- data.frame(emmeans(I.IIA_Force_lm, specs = "ExpCond"))  
if (anova(I.IIA_Force_lm)$`Pr(>F)` <0.05) {
  
  I.IIA_Force_hoc <- summary(glht(I.IIA_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

IIA_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA)
IIA_Po_emm <- data.frame(emmeans(IIA_Po_lm, specs = "ExpCond"))  
if (anova(IIA_Po_lm)$`Pr(>F)` <0.05) {
  
  IIA_Po_hoc <- summary(glht(IIA_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA)
IIA_Force_emm <- data.frame(emmeans(IIA_Force_lm, specs = "ExpCond"))  
if (anova(IIA_Force_lm)$`Pr(>F)` <0.05) {
  
  IIA_Force_hoc <- summary(glht(IIA_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum == 5) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

IIAX_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIAX)
IIAX_Po_emm <- data.frame(emmeans(IIAX_Po_lm, specs = "ExpCond"))
if (anova(IIAX_Po_lm)$`Pr(>F)` <0.05) {
  
  IIAX_Po_hoc <- summary(glht(IIAX_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIAX)
IIAX_Force_emm <- data.frame(emmeans(IIAX_Force_lm, specs = "ExpCond"))  
if (anova(IIAX_Force_lm)$`Pr(>F)` <0.05) {
  
  IIAX_Force_hoc <- summary(glht(IIAX_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


Fig_1_emm <- rbind(I_Po_emm,
                        I_Force_emm,
                        I.IIA_Po_emm,
                        I.IIA_Force_emm,
                        IIA_Po_emm,
                        IIA_Force_emm,
                        IIAX_Po_emm,
                        IIAX_Force_emm) %>% 
  mutate(Fiber_Type = c("I","I","I",'I',"I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"), .before = emmean) %>% 
  mutate(Value = c("ST","ST","ST",
                   "Force","Force","Force",
                   "ST","ST","ST",
                   "Force","Force","Force",
                   "ST","ST","ST",
                   "Force","Force","Force",
                   "ST","ST","ST",
                   "Force","Force","Force"), .before = emmean)


















