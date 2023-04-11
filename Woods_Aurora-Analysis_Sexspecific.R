library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)
theme_set(theme_classic())


# setwd("C:/Users/Phil/Dropbox/MBL/Aurora Fatigue Data")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/MBL/Aurora Fatigue Data")

# my_data <- read_excel("Aurora_Masters_CS.xlsx") %>%
#   filter(Fibertypenum %in% c(1,2,4,5)) %>%
#   filter(ExpCondnum %in% c(1:3)) %>%
#   group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
#   mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
#   mutate(Force = CSA*PoControl25C)
#   # select(SubjectNum, AgeGrp, Sex,Leg, FiberType,Fibertypenum,Fibernum,ExpCond,ExpCondnum,
#   #        CSA,Force,PoControl25C,MSRunNum, AkNm2,k,BkNm2,lbHz,CkNm2,lcHz,ton,twopib)

table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""),
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)

}

### Figure 1: Bar Graphs-----------------------------------------------------------------

I <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>% 
  mutate(Force = CSA*PoControl25C) 


I_Po_lm <- lmer(PoControl25C ~ CondxSex + (1 | SubjectNum), data = I)
I_Po_emm <- data.frame(emmeans(I_Po_lm, specs = "CondxSex"))  
if ((a <- anova(I_Po_lm)$`Pr(>F)`) <0.05) {
 
   I_Po_hoc <- summary(glht(I_Po_lm, linfct = mcp(CondxSex = "Tukey")))
  
   } else{
     NA
}

I_Force_lm <- lmer(Force ~ CondxSex + (1  | SubjectNum), data = I)
I_Force_emm <- data.frame(emmeans(I_Force_lm, specs = "CondxSex"))  
if ((b<- anova(I_Force_lm)$`Pr(>F)`) <0.05) {
  
  I_Force_hoc <- summary(glht(I_Force_lm, linfct = mcp(CondxSex = "Tukey")))
  
} else{
  NA
}

I.IIA <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 4) %>% 
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>% 
  mutate(Force = CSA*PoControl25C) 

I.IIA_Po_lm <- lmer(PoControl25C ~ CondxSex + (1 | SubjectNum), data = I.IIA)
I.IIA_Po_emm <- data.frame(emmeans(I.IIA_Po_lm, specs = "CondxSex"))  
if ((c <- anova(I.IIA_Po_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Po_hoc <- summary(glht(I.IIA_Po_lm, linfct = mcp(CondxSex = "Tukey")))
  
} else{
  NA
}

I.IIA_Force_lm <- lmer(Force ~ CondxSex + (1  | SubjectNum), data = I.IIA)
I.IIA_Force_emm <- data.frame(emmeans(I.IIA_Force_lm, specs = "CondxSex"))  
if ((d <- anova(I.IIA_Force_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Force_hoc <- summary(glht(I.IIA_Force_lm, linfct = mcp(CondxSex = "Tukey")))
  
} else{
  NA
}

IIA <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>% 
  mutate(Force = CSA*PoControl25C) 

IIA_Po_lm <- lmer(PoControl25C ~ CondxSex + (1  | SubjectNum), data = IIA)
IIA_Po_emm <- data.frame(emmeans(IIA_Po_lm, specs = "CondxSex"))  
if ((e<- anova(IIA_Po_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Po_hoc <- summary(glht(IIA_Po_lm, linfct = mcp(CondxSex = "Tukey")))
  
} else{
  NA
}

IIA_Force_lm <- lmer(Force ~ CondxSex + (1  | SubjectNum), data = IIA)
IIA_Force_emm <- data.frame(emmeans(IIA_Force_lm, specs = "CondxSex"))  
if ((f <- anova(IIA_Force_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Force_hoc <- summary(glht(IIA_Force_lm, linfct = mcp(CondxSex = "Tukey")))
  
} else{
  NA
}

IIAX <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 5) %>% 
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>% 
  mutate(Force = CSA*PoControl25C) 

IIAX_Po_lm <- lmer(PoControl25C ~ CondxSex + (1 | SubjectNum), data = IIAX)
IIAX_Po_emm <- data.frame(emmeans(IIAX_Po_lm, specs = "CondxSex"))
if (( g<- anova(IIAX_Po_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Po_hoc <- summary(glht(IIAX_Po_lm, linfct = mcp(CondxSex = "Tukey")))
  
} else{
  NA
}

IIAX_Force_lm <- lmer(Force ~ CondxSex + (1 | SubjectNum), data = IIAX)
IIAX_Force_emm <- data.frame(emmeans(IIAX_Force_lm, specs = "CondxSex"))  
if ((h<- anova(IIAX_Force_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Force_hoc <- summary(glht(IIAX_Force_lm, linfct = mcp(CondxSex = "Tukey")))
  
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
  mutate(Fiber_Type = c("I","I","I",'I',"I","I","I","I","I",'I',"I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"), .before = emmean) %>% 
  mutate(Value = c("ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force",
                   "ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force",
                   "ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force",
                   "ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force"), .before = emmean)

Fig_1_anova <- data.frame(rbind(a,b,c,d,e,f,g,h)) %>% 
  mutate(Fiber_Type = c("I","I", "I/IIA","I/IIA", "IIA","IIA", "IIAX","IIAX")) %>% 
  mutate(Value = c("ST", "Force","ST", "Force","ST", "Force","ST", "Force")) 
colnames(Fig_1_anova) <- c("p_value", "Fiber_Type", "Value")

Fig_1_posthoc <- data.frame(rbind(table_glht(I_Po_hoc),
                            table_glht(I_Force_hoc),
                            table_glht(IIA_Po_hoc),
                            table_glht(IIA_Force_hoc),
                            table_glht(IIAX_Po_hoc),
                            table_glht(IIAX_Force_hoc))) %>%   
  mutate(Fiber_Type = c("I","I","I","I","I","I","I","I","I","I","I","I","I","I","I",
                        "I","I","I","I","I","I","I","I","I","I","I","I","I","I","I",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"),
         .before = Estimate) %>% 
  mutate(Value = c("ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force",
                   "ST","ST","ST", "ST","ST","ST","ST","ST","ST", "ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force",
                   "ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST","ST",
                   "Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force","Force"),
         .before = Fiber_Type) %>% 
  mutate(Comparison = c("Control Males v Control Females","Control Males v Fatigue Males","Control Males v Fatigue Females",
                        "Control Males v Fatigue dATP Males","Control Males v Fatigue dATP Females","Control Females v Fatigue Males",
                        "Control Females v Fatigue Females","Control Females v Fatigue dATP Males","Control Females v Fatigue dATP Females",
                        "Fatigue Males v Fatigue Females","Fatigue Males v Fatigue dATP Males","Fatigue Males v Fatigue dATP Females",
                        "Fatigue Females v Fatigue dATP Males","Fatigue Females v Fatigue dATP Males","Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue Males","Control Males v Fatigue Females",
                        "Control Males v Fatigue dATP Males","Control Males v Fatigue dATP Females","Control Females v Fatigue Males",
                        "Control Females v Fatigue Females","Control Females v Fatigue dATP Males","Control Females v Fatigue dATP Females",
                        "Fatigue Males v Fatigue Females","Fatigue Males v Fatigue dATP Males","Fatigue Males v Fatigue dATP Females",
                        "Fatigue Females v Fatigue dATP Males","Fatigue Females v Fatigue dATP Males","Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue Males","Control Males v Fatigue Females",
                        "Control Males v Fatigue dATP Males","Control Males v Fatigue dATP Females","Control Females v Fatigue Males",
                        "Control Females v Fatigue Females","Control Females v Fatigue dATP Males","Control Females v Fatigue dATP Females",
                        "Fatigue Males v Fatigue Females","Fatigue Males v Fatigue dATP Males","Fatigue Males v Fatigue dATP Females",
                        "Fatigue Females v Fatigue dATP Males","Fatigue Females v Fatigue dATP Males","Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue Males","Control Males v Fatigue Females",
                        "Control Males v Fatigue dATP Males","Control Males v Fatigue dATP Females","Control Females v Fatigue Males",
                        "Control Females v Fatigue Females","Control Females v Fatigue dATP Males","Control Females v Fatigue dATP Females",
                        "Fatigue Males v Fatigue Females","Fatigue Males v Fatigue dATP Males","Fatigue Males v Fatigue dATP Females",
                        "Fatigue Females v Fatigue dATP Males","Fatigue Females v Fatigue dATP Males","Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue Males","Control Males v Fatigue Females",
                        "Control Males v Fatigue dATP Males","Control Males v Fatigue dATP Females","Control Females v Fatigue Males",
                        "Control Females v Fatigue Females","Control Females v Fatigue dATP Males","Control Females v Fatigue dATP Females",
                        "Fatigue Males v Fatigue Females","Fatigue Males v Fatigue dATP Males","Fatigue Males v Fatigue dATP Females",
                        "Fatigue Females v Fatigue dATP Males","Fatigue Females v Fatigue dATP Males","Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue Males","Control Males v Fatigue Females",
                        "Control Males v Fatigue dATP Males","Control Males v Fatigue dATP Females","Control Females v Fatigue Males",
                        "Control Females v Fatigue Females","Control Females v Fatigue dATP Males","Control Females v Fatigue dATP Females",
                        "Fatigue Males v Fatigue Females","Fatigue Males v Fatigue dATP Males","Fatigue Males v Fatigue dATP Females",
                        "Fatigue Females v Fatigue dATP Males","Fatigue Females v Fatigue dATP Males","Fatigue dATP Males v Fatigue dATP Females"
                          ),
         .before = Value)


Figure_1_Final <- list(Fig_1_emm,Fig_1_anova,Fig_1_posthoc)

names(Figure_1_Final) <- c("Figure 1-EMM", "Figure 1-Anova", "Figure 1-Posthoc")

writexl::write_xlsx(Figure_1_Final, path = "Woods_AuroraMaster_Figure1_SexSpecific.xlsx")

### Figure 1: Scatter Plots ---------------------------------------------
I <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>%  
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>% 
  mutate(Force = CSA*PoControl25C) 

I_F1_con_males <- I %>% filter(ExpCond == "Control" & CondxSex == 0)
I_F1_con_lm_males <- lm(I_F1_con_males$Force ~ I_F1_con_males$CSA)
I_F1_con_males$mdl <- predict(I_F1_con_lm_males)

I_F1_con_females <- I %>% filter(ExpCond == "Control" & CondxSex == 1)
I_F1_con_lm_females <- lm(I_F1_con_females$Force ~ I_F1_con_females$CSA)
I_F1_con_females$mdl <- predict(I_F1_con_lm_females)



I_F1_fat_males <- I %>% filter(ExpCond == "Fatigue" & CondxSex == 2)
I_F1_fat_lm_males <- lm(I_F1_fat_males$Force ~ I_F1_fat_males$CSA)
I_F1_fat_males$mdl <- predict(I_F1_fat_lm_males)

I_F1_fat_females <- I %>% filter(ExpCond == "Fatigue" & CondxSex == 3)
I_F1_fat_lm_females <- lm(I_F1_fat_females$Force ~ I_F1_fat_females$CSA)
I_F1_fat_females$mdl <- predict(I_F1_fat_lm_females)



I_F1_fatdatp_males <- I %>% filter(ExpCondnum == 3 & CondxSex == 4)
I_F1_fatdatp_lm_males <- lm(I_F1_fatdatp_males$Force ~ I_F1_fatdatp_males$CSA)
I_F1_fatdatp_males$mdl <- predict(I_F1_fatdatp_lm_males)

I_F1_fatdatp_females <- I %>% filter(ExpCondnum == 3 & CondxSex == 5)
I_F1_fatdatp_lm_females <- lm(I_F1_fatdatp_females$Force ~ I_F1_fatdatp_females$CSA)
I_F1_fatdatp_females$mdl <- predict(I_F1_fatdatp_lm_females)


(I_Fig1_scat <- ggplot(data = I,
                      aes(x = CSA, y = Force)) +
  geom_point(aes(shape = ExpCond), size = 1.5) +
  geom_line(data = I_F1_con_males, aes(y = mdl), linetype = "solid",col="red", size = 1) +
  geom_line(data = I_F1_con_females, aes(y = mdl), linetype = "solid",col="blue", size = 1) +
  geom_line(data = I_F1_fat_males, aes(y = mdl), linetype = "longdash",col= "red", size = 1) +
  geom_line(data = I_F1_fat_females, aes(y = mdl), linetype = "longdash",col= "blue", size = 1) +
  geom_line(data = I_F1_fatdatp_males, aes(y = mdl), linetype = "dotted", col = "blue",size = 1) +
  geom_line(data = I_F1_fatdatp_females, aes(y = mdl), linetype = "dotted", col = "red",size = 1)  
)

IIA <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum ==2 ) %>% 
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>%  
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>% 
  mutate(Force = CSA*PoControl25C) 

IIA_F1_con_males <- IIA %>% filter(ExpCond == "Control" & CondxSex == 0)
IIA_F1_con_lm_males <- lm(IIA_F1_con_males$Force ~ IIA_F1_con_males$CSA)
IIA_F1_con_males$mdl <- predict(IIA_F1_con_lm_males)

IIA_F1_con_females <- IIA %>% filter(ExpCond == "Control" & CondxSex == 1)
IIA_F1_con_lm_females <- lm(IIA_F1_con_females$Force ~ IIA_F1_con_females$CSA)
IIA_F1_con_females$mdl <- predict(IIA_F1_con_lm_females)



IIA_F1_fat_males <- IIA %>% filter(ExpCond == "Fatigue" & CondxSex == 2)
IIA_F1_fat_lm_males <- lm(IIA_F1_fat_males$Force ~ IIA_F1_fat_males$CSA)
IIA_F1_fat_males$mdl <- predict(IIA_F1_fat_lm_males)

IIA_F1_fat_females <- IIA %>% filter(ExpCond == "Fatigue" & CondxSex == 3)
IIA_F1_fat_lm_females <- lm(IIA_F1_fat_females$Force ~ IIA_F1_fat_females$CSA)
IIA_F1_fat_females$mdl <- predict(IIA_F1_fat_lm_females)



IIA_F1_fatdatp_males <- IIA %>% filter(ExpCondnum == 3 & CondxSex == 4)
IIA_F1_fatdatp_lm_males <- lm(IIA_F1_fatdatp_males$Force ~ IIA_F1_fatdatp_males$CSA)
IIA_F1_fatdatp_males$mdl <- predict(IIA_F1_fatdatp_lm_males)

IIA_F1_fatdatp_females <- IIA %>% filter(ExpCondnum == 3 & CondxSex == 5)
IIA_F1_fatdatp_lm_females <- lm(IIA_F1_fatdatp_females$Force ~ IIA_F1_fatdatp_females$CSA)
IIA_F1_fatdatp_females$mdl <- predict(IIA_F1_fatdatp_lm_females)



(IIA_Fig1_scat <- ggplot(data = IIA,
                       aes(x = CSA, y = Force)) +
    geom_point(aes(shape = ExpCond), size = 1.5) +
    geom_line(data = IIA_F1_con_males, aes(y = mdl), linetype = "solid",col="red", size = 1) +
    geom_line(data = IIA_F1_con_females, aes(y = mdl), linetype = "solid",col="blue", size = 1) +
    geom_line(data = IIA_F1_fat_males, aes(y = mdl), linetype = "longdash",col= "red", size = 1) +
    geom_line(data = IIA_F1_fat_females, aes(y = mdl), linetype = "longdash",col= "blue", size = 1) +
    geom_line(data = IIA_F1_fatdatp_males, aes(y = mdl), linetype = "dotted", col = "blue",size = 1) +
    geom_line(data = IIA_F1_fatdatp_females, aes(y = mdl), linetype = "dotted", col = "red",size = 1)  
)


## Figure 3: Control + Fatigue  -----------------------------------------------------------------------------------

con_fat_data <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(Grp == 1) %>%
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, CondxSex) %>%
  mutate(Force = CSA*PoControl25C)


## IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
I_cf <- con_fat_data %>% filter(FiberType == "I")

I_Fig3_B_lm <- lmer(BkNm2 ~ CondxSex + (1 | SubjectNum),data = I_cf)
I_Fig3_B_emm <- data.frame(emmeans(I_Fig3_B_lm, specs = "CondxSex"))
if((i <- anova(I_Fig3_B_lm)$`Pr(>F)`) <0.05){
  I_Fig3_B_hoc <- summary(glht(I_Fig3_B_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}


I_Fig3_Ae_lm <- lmer(Aelastic ~ CondxSex + (1  | SubjectNum),data = I_cf)
I_Fig3_Ae_emm <- data.frame(emmeans(I_Fig3_Ae_lm, specs = "CondxSex"))
if((i.1 <- anova(I_Fig3_Ae_lm)$`Pr(>F)`) <0.05){
  I_Fig3_Ae_hoc <- summary(glht(I_Fig3_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I_Fig3_Av_lm <- lmer(Aviscous ~ CondxSex + (1  | SubjectNum),data = I_cf)
I_Fig3_Av_emm <- data.frame(emmeans(I_Fig3_Av_lm, specs = "CondxSex"))
if((i.2 <- anova(I_Fig3_Av_lm)$`Pr(>F)`)<0.05){
  I_Fig3_Av_hoc <- summary(glht(I_Fig3_Av_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}


I_Fig3_2pib_lm <- lmer(twopib ~ CondxSex + (1  | SubjectNum),data = I_cf)
I_Fig3_2pib_emm <- data.frame(emmeans(I_Fig3_2pib_lm, specs = "CondxSex"))
if((j <- anova(I_Fig3_2pib_lm)$`Pr(>F)`)< 0.05) {
  I_Fig3_2pib_hoc <- summary(glht(I_Fig3_2pib_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

I_Fig3_ton_lm <- lmer(ton ~ CondxSex + (1  | SubjectNum),data = I_cf)
I_Fig3_ton_emm <- data.frame(emmeans(I_Fig3_ton_lm, specs = "CondxSex"))
if((k <- anova(I_Fig3_ton_lm)$`Pr(>F)`)<0.05){
  I_Fig3_ton_hoc <- summary(glht(I_Fig3_ton_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 

I.IIA_cf <- con_fat_data %>% filter(Fibertypenum == 4)

I.IIA_Fig3_B_lm <- lmer(BkNm2 ~ CondxSex + (1 | SubjectNum), data = I.IIA_cf)
I.IIA_Fig3_B_emm <- data.frame(emmeans(I.IIA_Fig3_B_lm, specs = "CondxSex"))
if((l <- anova(I.IIA_Fig3_B_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_B_hoc <- summary(glht(I.IIA_Fig3_B_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I.IIA_Fig3_Ae_lm <- lmer(Aelastic ~ CondxSex + (1  | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_Ae_emm <- data.frame(emmeans(I.IIA_Fig3_Ae_lm, specs = "CondxSex"))
if((l.1 <- anova(I.IIA_Fig3_Ae_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_Ae_hoc <- summary(glht(I.IIA_Fig3_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

I.IIA_Fig3_Av_lm <- lmer(Aviscous ~ CondxSex + (1  | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_Av_emm <- data.frame(emmeans(I.IIA_Fig3_Av_lm, specs = "CondxSex"))
if((l.2 <- anova(I.IIA_Fig3_Av_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_Av_hoc <- summary(glht(I.IIA_Fig3_Av_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}


I.IIA_Fig3_2pib_lm <- lmer(twopib ~ CondxSex + (1  | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_2pib_lm, specs = "CondxSex"))
if((m <- anova(I.IIA_Fig3_2pib_lm)$`Pr(>F)`) <0.05){
  I.IIA_Fig3_2pib_hoc <- summary(glht(I.IIA_Fig3_2pib_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

I.IIA_Fig3_ton_lm <- lmer(ton ~ CondxSex + (1  | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_ton_emm <- data.frame(emmeans(I.IIA_Fig3_ton_lm, specs = "CondxSex"))
if((n <- anova(I.IIA_Fig3_ton_lm)$`Pr(>F)`) <0.05) {
  I.IIA_Fig3_ton_hoc <- summary(glht(I.IIA_Fig3_ton_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

# IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA 

IIA_cf <- con_fat_data %>% filter(Fibertypenum == 2)

IIA_Fig3_B_lm <- lmer(BkNm2 ~ CondxSex + (1  | SubjectNum),data = IIA_cf)
IIA_Fig3_B_emm <- data.frame(emmeans(IIA_Fig3_B_lm, specs = "CondxSex"))
if((o <- anova(IIA_Fig3_B_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_B_hoc <- summary(glht(IIA_Fig3_B_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

IIA_Fig3_Ae_lm <- lmer(Aelastic ~ CondxSex + (1  | SubjectNum),data = IIA_cf)
IIA_Fig3_Ae_emm <- data.frame(emmeans(IIA_Fig3_Ae_lm, specs = "CondxSex"))
if((o.1 <- anova(IIA_Fig3_Ae_lm)$`Pr(>F)`) <0.05){
  IIA_Fig3_Ae_hoc <- summary(glht(IIA_Fig3_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

IIA_Fig3_Av_lm <- lmer(Aviscous ~ CondxSex + (1  | SubjectNum),data = IIA_cf)
IIA_Fig3_Av_emm <- data.frame(emmeans(IIA_Fig3_Av_lm, specs = "CondxSex"))
if((o.2 <- anova(IIA_Fig3_Av_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_Av_hoc <- summary(glht(IIA_Fig3_Av_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

IIA_Fig3_2pib_lm <- lmer(twopib ~ CondxSex + (1  | SubjectNum),data = IIA_cf)
IIA_Fig3_2pib_emm <- data.frame(emmeans(IIA_Fig3_2pib_lm, specs = "CondxSex"))
if((p <- anova(IIA_Fig3_2pib_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_2pib_hoc <- summary(glht(IIA_Fig3_2pib_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}
    
IIA_Fig3_ton_lm <- lmer(ton ~ CondxSex + (1  | SubjectNum),data = IIA_cf)
IIA_Fig3_ton_emm <- data.frame(emmeans(IIA_Fig3_ton_lm, specs = "CondxSex"))
if((q <- anova(IIA_Fig3_ton_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_ton_hoc <- summary(glht(IIA_Fig3_ton_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA} 
  

# IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX 

IIAX_cf <- con_fat_data %>% filter(Fibertypenum == 5)


IIAX_Fig3_B_lm <- lmer(BkNm2 ~ CondxSex + (1 | SubjectNum),data = IIAX_cf)
IIAX_Fig3_B_emm <- data.frame(emmeans(IIAX_Fig3_B_lm, specs = "CondxSex"))
if((q.1 <- anova(IIAX_Fig3_B_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_B_hoc <- summary(glht(IIAX_Fig3_B_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

IIAX_Fig3_Ae_lm <- lmer(Aelastic ~ CondxSex + (1  | SubjectNum),data = IIAX_cf)
IIAX_Fig3_Ae_emm <- data.frame(emmeans(IIAX_Fig3_Ae_lm, specs = "CondxSex"))
if((q.2 <- anova(IIAX_Fig3_Ae_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_Ae_hoc <- summary(glht(IIAX_Fig3_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

IIAX_Fig3_Av_lm <- lmer(Aviscous ~ CondxSex + (1  | SubjectNum),data = IIAX_cf)
IIAX_Fig3_Av_emm <- data.frame(emmeans(IIAX_Fig3_Av_lm, specs = "CondxSex"))
if((q.3 <- anova(IIAX_Fig3_Av_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_Av_hoc <- summary(glht(IIAX_Fig3_Av_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

IIAX_Fig3_2pib_lm <- lmer(twopib ~ CondxSex + (1 | SubjectNum),data = IIAX_cf)
IIAX_Fig3_2pib_emm <- data.frame(emmeans(IIAX_Fig3_2pib_lm, specs = "CondxSex"))
if((q.4 <- anova(IIAX_Fig3_2pib_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_2pib_hoc <- summary(glht(IIAX_Fig3_2pib_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA} 

IIAX_Fig3_ton_lm <- lmer(ton ~ CondxSex + (1 | SubjectNum),data = IIAX_cf)
IIAX_Fig3_ton_emm <- data.frame(emmeans(IIAX_Fig3_ton_lm, specs = "CondxSex"))
if((q.5 <- anova(IIAX_Fig3_ton_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_ton_hoc <- summary(glht(IIAX_Fig3_ton_lm, linfct = mcp(CondxSex = "Tukey")))
} else{NA}

Fig_3_cf_sexdiff_emm <- rbind(I_Fig3_B_emm,
                      I_Fig3_Ae_emm,
                      I_Fig3_Av_emm,
                      I_Fig3_2pib_emm,
                      I_Fig3_ton_emm,
                      I.IIA_Fig3_B_emm,
                      I.IIA_Fig3_Ae_emm,
                      I.IIA_Fig3_Av_emm,
                      I.IIA_Fig3_2pib_emm,
                      I.IIA_Fig3_ton_emm,
                      IIA_Fig3_B_emm,
                      IIA_Fig3_Ae_emm,
                      IIA_Fig3_Av_emm,
                      IIA_Fig3_2pib_emm,
                      IIA_Fig3_ton_emm,
                      IIAX_Fig3_B_emm,
                      IIAX_Fig3_Ae_emm,
                      IIAX_Fig3_Av_emm,
                      IIAX_Fig3_2pib_emm,
                      IIAX_Fig3_ton_emm) %>% 
  mutate(Fiber_Type = c("I","I","I",'I',"I","I","I","I","I","I","I","I","I",'I',"I","I","I","I","I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"
                        ), .before = emmean) %>% 
  mutate(Value = c("B","B","B","B", "Aelastic", "Aelastic", "Aelastic", "Aelastic","Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton",
                   "B","B","B","B", "Aelastic", "Aelastic", "Aelastic", "Aelastic","Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton",
                   "B","B","B","B", "Aelastic", "Aelastic", "Aelastic", "Aelastic","Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton",
                   "B","B","B","B", "Aelastic", "Aelastic", "Aelastic", "Aelastic","Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton"
                   ), .before = emmean)

Fig_3_cf_sexdiff_anova <- data.frame(rbind(i,i.1, i.2, j, k, l, l.1, l.2, m,n, o, o.1, o.2, p, q, q.1,q.2,q.3,q.4,q.5)) %>% 
  mutate(Fiber_Type = c("I","I","I","I","I", 
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA", "IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX")) %>%   
  mutate(Value = c("B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton")) 
colnames(Fig_3_cf_sexdiff_anova) <- c("p_value", "Fiber_Type", "Value")

Fig3_cf_sexdiff_posthocs <- data.frame(rbind(table_glht(I_Fig3_Ae_hoc),
                                             table_glht(I_Fig3_Av_hoc),
                                             table_glht(I_Fig3_2pib_hoc),
                                             table_glht(I_Fig3_ton_hoc),
                                             table_glht(I.IIA_Fig3_B_hoc),
                                             table_glht(I.IIA_Fig3_Ae_hoc),
                                             table_glht(IIA_Fig3_B_hoc),
                                             table_glht(IIA_Fig3_Av_hoc),
                                             table_glht(IIA_Fig3_2pib_hoc),
                                             table_glht(IIA_Fig3_ton_hoc),
                                             table_glht(IIAX_Fig3_ton_hoc)
                                             )) %>% 
  mutate(FiberType = c("I","I","I","I","I","I","I","I","I","I","I","I",
                       "I","I","I","I","I","I","I","I","I","I","I","I",
                       "I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA",
                       "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                       "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                       "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"),.before = Estimate) %>% 
  mutate(Value = c("Aelastic","Aelastic","Aelastic","Aelastic","Aelastic","Aelastic",
                   "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous",
                   "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous",
                   "ton","ton","ton","ton","ton","ton",
                   "B","B","B","B","B","B",
                   "Aelastic","Aelastic","Aelastic","Aelastic","Aelastic","Aelastic",
                   "B","B","B","B","B","B",
                   "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous",
                   "2pib","2pib","2pib","2pib","2pib","2pib",
                   "ton", "ton", "ton", "ton", "ton", "ton",
                   "ton", "ton", "ton", "ton", "ton", "ton"
                   ),.before = Estimate) %>% 
  mutate(Comparison = c("Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females",
                        "Control Males v Control Females","Control Males v Fatigue Males", "Control Males v Fatigue Females","Control Females v Fatigue Males","Control Females v Fatigue Females", "Fatigue Males v Fatigue Females"),
         .before = FiberType)


Fig3_cf_sexdiff <- list(Fig_3_cf_sexdiff_emm,Fig_3_cf_sexdiff_anova,Fig3_cf_sexdiff_posthocs)

names(Fig3_cf_sexdiff) <- c("Fig3 CvF Sex Diff -EMM", 
                    "Fig3 CvF Sex Diff -Anova",
                    "Fig3 CvF Sex Diff - Posthocs")

writexl::write_xlsx(Fig3_cf_sexdiff, path = "Woods_AuroraMaster_Figure3_ControlvFatigue_SexDiff.xlsx")

### Figure 3: Control + dATP --------------------------------------------------------------

con_fatdatp_data <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(Grp == 2) %>%
  filter(CondxSex %in% c(0:5)) %>% 
  mutate(CondxSex = as.factor(CondxSex)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum,CondxSex) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)


# I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I 
I_cdatp <- con_fatdatp_data %>% filter(FiberType == "I")

I_Fig3_gr2_B_lm <- lmer(BkNm2 ~ CondxSex + (1 | SubjectNum),data = I_cdatp)
I_Fig3_gr2_B_emm <- data.frame(emmeans(I_Fig3_gr2_B_lm, specs = "CondxSex"))
if((r <- anova(I_Fig3_gr2_B_lm)$`Pr(>F)`)<0.05){
  I_Fig3_gr2_hoc <- summary(glht(I_Fig3_gr2_B_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ CondxSex + (1 | SubjectNum),data = I_cdatp)
I_Fig3_gr2_Ae_emm <- data.frame(emmeans(I_Fig3_gr2_Ae_lm, specs = "CondxSex"))
if((r.1 <- anova(I_Fig3_gr2_Ae_lm)$`Pr(>F)`)<0.05){
  I_Fig3_gr2_hoc <- summary(glht(I_Fig3_gr2_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I_Fig3_gr2_Av_lm <- lmer(Aviscous ~ CondxSex + (1 | SubjectNum),data = I_cdatp)
I_Fig3_gr2_Av_emm <- data.frame(emmeans(I_Fig3_gr2_Av_lm, specs = "CondxSex"))
if((r.2 <- anova(I_Fig3_gr2_Av_lm)$`Pr(>F)`)<0.05){
  I_Fig3_gr2_Av_hoc <- summary(glht(I_Fig3_gr2_Av_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}


I_Fig3_gr2_2pib_lm <- lmer(twopib ~ CondxSex + (1 | SubjectNum),data = I_cdatp)
I_Fig3_gr2_2pib_emm <- data.frame(emmeans(I_Fig3_gr2_2pib_lm, specs = "CondxSex"))
if((s <- anova(I_Fig3_gr2_2pib_lm)$`Pr(>F)`)<0.05){
  I_Fig3_gr2_2pib_hoc <- summary(glht(I_Fig3_gr2_2pib_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I_Fig3_gr2_ton_lm <- lmer(ton ~ CondxSex + (1  | SubjectNum), data = I_cdatp)
I_Fig3_gr2_ton_emm <- data.frame(emmeans(I_Fig3_gr2_ton_lm, specs = "CondxSex"))
if((t <- anova(I_Fig3_gr2_ton_lm)$`Pr(>F)`)<0.05){
  I_Fig3_gr2_ton_hoc <- summary(glht(I_Fig3_gr2_ton_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 
I.IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 4)

I.IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ CondxSex + (1  | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_B_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_B_lm, specs = "CondxSex"))
if((u <- anova(I.IIA_Fig3_gr2_B_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_gr2_B_hoc <- summary(glht(I.IIA_Fig3_gr2_B_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I.IIA_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ CondxSex + (1  | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_Ae_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_Ae_lm, specs = "CondxSex"))
if((u.1 <- anova(I.IIA_Fig3_gr2_Ae_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_gr2_Ae_hoc <- summary(glht(I.IIA_Fig3_gr2_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I.IIA_Fig3_gr2_Av_lm <- lmer(Aviscous ~ CondxSex + (1  | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_Av_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_Av_lm, specs = "CondxSex"))
if((u.2 <- anova(I.IIA_Fig3_gr2_Av_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_gr2_Av_hoc <- summary(glht(I.IIA_Fig3_gr2_Av_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I.IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ CondxSex + (1 | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_2pib_lm, specs = "CondxSex"))
if((v <- anova(I.IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_gr2_A2pib_hoc <- summary(glht(I.IIA_Fig3_gr2_2pib_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

I.IIA_Fig3_gr2_ton_lm <- lmer(ton ~ CondxSex + (1  |SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_ton_lm, specs = "CondxSex"))
if((w <- anova(I.IIA_Fig3_gr2_ton_lm)$`Pr(>F)`)<0.05){
  I.IIA_Fig3_gr2_ton_hoc <- summary(glht(I.IIA_Fig3_gr2_ton_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}


# IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA 
IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 2)

IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ CondxSex + (1  | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_B_emm <- data.frame(emmeans(IIA_Fig3_gr2_B_lm, specs = "CondxSex"))
if((x <- anova(IIA_Fig3_gr2_B_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_B_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

IIA_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ CondxSex + (1 | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIA_Fig3_gr2_Ae_lm, specs = "CondxSex"))
if((x.1 <- anova(IIA_Fig3_gr2_Ae_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_gr2_Ae_hoc <- summary(glht(IIA_Fig3_gr2_Ae_lm, linfct = mcp(CondxSex = "Tukey")))
}else{NA}

IIA_Fig3_gr2_Av_lm <- lmer(Aviscous ~ CondxSex + (1 | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_Av_emm <- data.frame(emmeans(IIA_Fig3_gr2_Av_lm, specs = "CondxSex"))
if((x.2 <- anova(IIA_Fig3_gr2_Av_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_gr2_Av_hoc <- summary(glht(IIA_Fig3_gr2_Av_lm, specs = "CondxSex"))
}else{NA}

IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ CondxSex + (1 | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIA_Fig3_gr2_2pib_lm, specs = "CondxSex"))
if((y <- anova(IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_gr2_2pib_hoc <- summary(glht(IIA_Fig3_gr2_2pib_lm, specs = "CondxSex"))
}else{NA}

IIA_Fig3_gr2_ton_lm <- lmer(ton ~ CondxSex + (1 | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(IIA_Fig3_gr2_ton_lm, specs = "CondxSex"))
if((z <- anova(IIA_Fig3_gr2_ton_lm)$`Pr(>F)`)<0.05){
  IIA_Fig3_gr2_ton_hoc <- summary(glht(IIA_Fig3_gr2_ton_lm, specs = "CondxSex"))
}else{NA}

# IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX
IIAX_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 5)

IIAX_Fig3_gr2_B_lm <- lmer(BkNm2 ~ CondxSex + (1  | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_B_emm <- data.frame(emmeans(IIAX_Fig3_gr2_B_lm, specs = "CondxSex"))
if((z.1 <- anova(IIAX_Fig3_gr2_B_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_B_lm, specs = "CondxSex"))
}else{NA}


IIAX_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ CondxSex + (1 | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIAX_Fig3_gr2_Ae_lm, specs = "CondxSex"))
if((z.2 <- anova(IIAX_Fig3_gr2_Ae_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_gr2_Ae_hoc <- summary(glht(IIAX_Fig3_gr2_Ae_lm, specs = "CondxSex"))
}else{NA}

IIAX_Fig3_gr2_Av_lm <- lmer(Aviscous ~ CondxSex + (1 | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_Av_emm <- data.frame(emmeans(IIAX_Fig3_gr2_Av_lm, specs = "CondxSex"))
if((z.3 <- anova(IIAX_Fig3_gr2_Av_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_gr2_Av_hoc <- summary(glht(IIAX_Fig3_gr2_Av_lm, specs = "CondxSex"))
}else{NA}

IIAX_Fig3_gr2_2pib_lm <- lmer(twopib ~ CondxSex + (1  | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIAX_Fig3_gr2_2pib_lm, specs = "CondxSex"))
if((zz <- anova(IIAX_Fig3_gr2_2pib_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_gr2_2pib_hoc <- summary(glht(IIAX_Fig3_gr2_2pib_lm, specs = "CondxSex"))
}else{NA}


IIAX_Fig3_gr2_ton_lm <- lmer(ton ~ CondxSex + (1  | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_ton_emm <- data.frame(emmeans(IIAX_Fig3_gr2_ton_lm, specs = "CondxSex"))
if((zzz <- anova(IIAX_Fig3_gr2_ton_lm)$`Pr(>F)`)<0.05){
  IIAX_Fig3_gr2_ton_hoc <- summary(glht(IIAX_Fig3_gr2_ton_lm, specs = "CondxSex"))
}else{NA}



Fig_3_cdatp_emm <- rbind(I_Fig3_gr2_B_emm,
                      I_Fig3_gr2_Ae_emm,
                      I_Fig3_gr2_Av_emm,
                      I_Fig3_gr2_2pib_emm,
                      I_Fig3_gr2_ton_emm,
                      I.IIA_Fig3_gr2_B_emm,
                      I.IIA_Fig3_gr2_Ae_emm,
                      I.IIA_Fig3_gr2_Av_emm,
                      I.IIA_Fig3_gr2_2pib_emm,
                      I.IIA_Fig3_gr2_ton_emm,
                      IIA_Fig3_gr2_B_emm,
                      IIA_Fig3_gr2_Ae_emm,
                      IIA_Fig3_gr2_Av_emm,
                      IIA_Fig3_gr2_2pib_emm,
                      IIA_Fig3_gr2_ton_emm,
                      IIAX_Fig3_gr2_B_emm,
                      IIAX_Fig3_gr2_Ae_emm,
                      IIAX_Fig3_gr2_Av_emm,
                      IIAX_Fig3_gr2_2pib_emm,
                      IIAX_Fig3_gr2_ton_emm) %>% 
  mutate(Fiber_Type = c("I","I","I",'I',"I","I","I","I","I","I","I","I","I",'I',"I","I","I","I","I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"
  ), .before = emmean) %>% 
  mutate(Value = c("B","B","B","B", "Aelastic", "Aelastic","Aelastic", "Aelastic", "Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton",
                   "B","B","B","B", "Aelastic", "Aelastic","Aelastic", "Aelastic", "Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton",
                   "B","B","B","B", "Aelastic", "Aelastic","Aelastic", "Aelastic", "Aviscous","Aviscous","Aviscous","Aviscous", "2pib", "2pib","2pib", "2pib", "ton", "ton","ton", "ton",
                   "B","B","Aelastic", "Aelastic", "Aviscous","Aviscous","2pib", "2pib","ton", "ton"
  ), .before = emmean)

Fig_3_cdatp_anova <- data.frame(rbind(r,r.1,r.2,s,t,u,u.1,u.2,v,w,x,x.1,x.2,y,z,z.1,z.2,z.3,zz,zzz)) %>%   
  mutate(Fiber_Type = c("I","I","I","I","I", 
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA", "IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX")) %>%    
  mutate(Value = c("B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton")) 

colnames(Fig_3_cdatp_anova) <- c("p_value", "Fiber_Type", "Value")


Fig3_cdatp_sexdiff_posthocs <- data.frame(rbind(table_glht(I_Fig3_gr2_Av_hoc),
                                             table_glht(I.IIA_Fig3_gr2_ton_hoc),
                                             table_glht(IIA_Fig3_gr2_B_hoc),
                                             table_glht(IIA_Fig3_gr2_2pib_hoc),
                                             table_glht(IIA_Fig3_gr2_ton_hoc))) %>% 
  mutate(FiberType = c("I","I","I","I","I","I",
                       "I.IIA","I.IIA","I.IIA","I.IIA","I.IIA","I.IIA",
                       "IIA","IIA","IIA","IIA","IIA","IIA",
                       "IIA","IIA","IIA","IIA","IIA","IIA",
                       "IIA","IIA","IIA","IIA","IIA","IIA"),.before = Estimate) %>% 
  mutate(Value = c("Aviscous","Aviscous","Aviscous","Aviscous","Aviscous","Aviscous",
                   "ton","ton","ton","ton","ton","ton",
                   "B","B","B","B","B","B",
                   "2pib","2pib","2pib","2pib","2pib","2pib",
                   "ton", "ton", "ton", "ton", "ton", "ton",
  ),.before = Estimate) %>% 
  mutate(Comparison = c("Control Males v Control Females","Control Males v Fatigue dATP Males", "Control Males v Fatigue dATP Females", "Control Females v Fatigue dATP Males", "Control Females v Fatigue dATP Females", "Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue dATP Males", "Control Males v Fatigue dATP Females", "Control Females v Fatigue dATP Males", "Control Females v Fatigue dATP Females", "Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue dATP Males", "Control Males v Fatigue dATP Females", "Control Females v Fatigue dATP Males", "Control Females v Fatigue dATP Females", "Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue dATP Males", "Control Males v Fatigue dATP Females", "Control Females v Fatigue dATP Males", "Control Females v Fatigue dATP Females", "Fatigue dATP Males v Fatigue dATP Females",
                        "Control Males v Control Females","Control Males v Fatigue dATP Males", "Control Males v Fatigue dATP Females", "Control Females v Fatigue dATP Males", "Control Females v Fatigue dATP Females", "Fatigue dATP Males v Fatigue dATP Females"
                        ),.before = FiberType)


Fig3_cdatp <- list(Fig_3_cdatp_emm,Fig_3_cdatp_anova)

names(Fig3_cdatp) <- c("Figure 3 C v dATP -EMM",
                       "Figure 3 C vs dATP - Anova")

writexl::write_xlsx(Fig3_cdatp, path = "Woods_AuroraMaster_Figure3_ControlvFatiguedATP.xlsx")

### Figure 3: Fatigue vs Fatigue dATP -----------------------------------------------------------------------

fat_v_datp <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(ExpCondnum %in% c(2,3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)

# MHC I .................................................................
I_ffdatp <- fat_v_datp %>% filter(FiberType == "I")

I_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_B_emm <- data.frame(emmeans(I_Fig3_fvf_B_lm, specs = "ExpCond"))
aa <- anova(I_Fig3_fvf_B_lm)$`Pr(>F)`

I_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_Ae_emm <- data.frame(emmeans(I_Fig3_fvf_Ae_lm, specs = "ExpCond"))
aa.1 <- anova(I_Fig3_fvf_Ae_lm)$`Pr(>F)`

I_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_Av_emm <- data.frame(emmeans(I_Fig3_fvf_Av_lm, specs = "ExpCond"))
aa.2 <- anova(I_Fig3_fvf_Av_lm)$`Pr(>F)`

I_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_2pib_emm <- data.frame(emmeans(I_Fig3_fvf_2pib_lm, specs = "ExpCond"))
ab <- anova(I_Fig3_fvf_2pib_lm)$`Pr(>F)`

I_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_ton_emm <- data.frame(emmeans(I_Fig3_fvf_ton_lm, specs = "ExpCond"))
ac <- anova(I_Fig3_fvf_ton_lm)$`Pr(>F)`

# MHC I/IIA............................................................................
I.IIA_ffdatp <- fat_v_datp %>% filter(FiberType == "I/IIA")

I.IIA_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum),data = I.IIA_ffdatp)
I.IIA_Fig3_fvf_B_emm <- data.frame(emmeans(I.IIA_Fig3_fvf_B_lm, specs = "ExpCond"))
ad <- anova(I.IIA_Fig3_fvf_B_lm)$`Pr(>F)`

I.IIA_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = I.IIA_ffdatp)
I.IIA_Fig3_fvf_Ae_emm <- data.frame(emmeans(I.IIA_Fig3_fvf_Ae_lm, specs = "ExpCond"))
ad.1 <- anova(I.IIA_Fig3_fvf_Ae_lm)$`Pr(>F)`

I.IIA_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = I.IIA_ffdatp)
I.IIA_Fig3_fvf_Av_emm <- data.frame(emmeans(I.IIA_Fig3_fvf_Av_lm, specs = "ExpCond"))
ad.2 <- anova(I.IIA_Fig3_fvf_Av_lm)$`Pr(>F)`

I.IIA_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = I.IIA_ffdatp)
I.IIA_Fig3_fvf_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_fvf_2pib_lm, specs = "ExpCond"))
ae <- anova(I.IIA_Fig3_fvf_2pib_lm)$`Pr(>F)`

I.IIA_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = I.IIA_ffdatp)
I.IIA_Fig3_fvf_ton_emm <- data.frame(emmeans(I.IIA_Fig3_fvf_ton_lm, specs = "ExpCond"))
af <- anova(I.IIA_Fig3_fvf_ton_lm)$`Pr(>F)`
# MHC IIA ...............................................................................

IIA_ffdatp <- fat_v_datp %>% filter(FiberType == "IIA")

IIA_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = IIA_ffdatp)
IIA_Fig3_fvf_B_emm <- data.frame(emmeans(IIA_Fig3_fvf_B_lm, specs = "ExpCond"))
ag <- anova(IIA_Fig3_fvf_B_lm)$`Pr(>F)`

IIA_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_Ae_emm <- data.frame(emmeans(IIA_Fig3_fvf_Ae_lm, specs = "ExpCond"))
ag.1 <- anova(IIA_Fig3_fvf_Ae_lm)$`Pr(>F)`

IIA_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_Av_emm <- data.frame(emmeans(IIA_Fig3_fvf_Av_lm, specs = "ExpCond"))
ag.2 <- anova(IIA_Fig3_fvf_Av_lm)$`Pr(>F)`

IIA_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_2pib_emm <- data.frame(emmeans(IIA_Fig3_fvf_2pib_lm, specs = "ExpCond"))
ah <- anova(IIA_Fig3_fvf_2pib_lm)$`Pr(>F)`

IIA_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_ton_emm <- data.frame(emmeans(IIA_Fig3_fvf_ton_lm, specs = "ExpCond"))
ai <- anova(IIA_Fig3_fvf_ton_lm)$`Pr(>F)`

# MHC IIAX .......................................................................

IIAX_ffdatp <- fat_v_datp %>% filter(FiberType == "IIA/IIX")

IIAX_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = IIAX_ffdatp)
IIAX_Fig3_fvf_B_emm <- data.frame(emmeans(IIAX_Fig3_fvf_B_lm, specs = "ExpCond"))
aj <- anova(IIAX_Fig3_fvf_B_lm)$`Pr(>F)`

IIAX_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_Ae_emm <- data.frame(emmeans(IIAX_Fig3_fvf_Ae_lm, specs = "ExpCond"))
aj.1 <- anova(IIAX_Fig3_fvf_Ae_lm)$`Pr(>F)`

IIAX_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_Av_emm <- data.frame(emmeans(IIAX_Fig3_fvf_Av_lm, specs = "ExpCond"))
aj.2 <- anova(IIAX_Fig3_fvf_Av_lm)$`Pr(>F)`


IIAX_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_2pib_emm <- data.frame(emmeans(IIAX_Fig3_fvf_2pib_lm, specs = "ExpCond"))
ak <- anova(IIAX_Fig3_fvf_2pib_lm)$`Pr(>F)`

IIAX_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_ton_emm <- data.frame(emmeans(IIAX_Fig3_fvf_ton_lm, specs = "ExpCond"))
al <- anova(IIAX_Fig3_fvf_ton_lm)$`Pr(>F)`



Fig_3_fvf_emm <- rbind(I_Fig3_fvf_B_emm,
                         I_Fig3_fvf_Ae_emm,
                         I_Fig3_fvf_Av_emm,
                         I_Fig3_fvf_2pib_emm,
                         I_Fig3_fvf_ton_emm,
                         I.IIA_Fig3_fvf_B_emm,
                         I.IIA_Fig3_fvf_Ae_emm,
                         I.IIA_Fig3_fvf_Av_emm,
                         I.IIA_Fig3_fvf_2pib_emm,
                         I.IIA_Fig3_fvf_ton_emm,
                         IIA_Fig3_fvf_B_emm,
                         IIA_Fig3_fvf_Ae_emm,
                         IIA_Fig3_fvf_Av_emm,
                         IIA_Fig3_fvf_2pib_emm,
                         IIA_Fig3_fvf_ton_emm,
                         IIAX_Fig3_fvf_B_emm,
                         IIAX_Fig3_fvf_Ae_emm,
                         IIAX_Fig3_fvf_Av_emm,
                         IIAX_Fig3_fvf_2pib_emm,
                         IIAX_Fig3_fvf_ton_emm) %>% 
  mutate(Fiber_Type = c("I","I","I",'I',"I","I","I","I","I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"
  ), .before = emmean) %>% 
  mutate(Value = c("B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton"
  ), .before = emmean)

Fig_3_fvf_anova <- data.frame(rbind(aa,aa.1,aa.2,ab,ac,ad,ad.1,ad.2,ae,af,ag,ag.1,ag.2,ah,ai,aj,aj.1,aj.2,ak,al)) %>%   
  mutate(Fiber_Type = c("I","I","I","I","I", 
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA", "IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX")) %>%    
  mutate(Value = c("B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton")) 

colnames(Fig_3_fvf_anova) <- c("p_value", "Fiber_Type", "Value")

Fig3_fvf <- list(Fig_3_fvf_emm,Fig_3_fvf_anova)

names(Fig3_fvf) <- c("Figure 3 Fat vs dATP - EMM",
                       "Figure 3 Fat vs dATP-Anova")

writexl::write_xlsx(Fig3_fvf, path = "Woods_AuroraMaster_Figure3_FatiguevFatiguedATP.xlsx")

### Figure 3: Fatigue Control vs Fatigue dATP Control ------------------------------------------
control_fat <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(ExpCondnum == 1 & Grp == 1) %>% 
  mutate(ExpCond = "Control_Fat")

control_datp <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(ExpCondnum == 1 & Grp == 2) %>% 
  mutate(ExpCond = "Control_dATP")

control_df <- rbind(control_fat, control_datp) %>% 
  group_by(ExpCondnum, SubjectNum, FiberType, Fibertypenum)

# MHC I .................................................................
I_cc <- control_df %>% filter(FiberType == "I")

I_cc_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = I_cc)
I_cc_B_emm <- data.frame(emmeans(I_cc_B_lm , specs = "ExpCond"))
za <- anova(I_cc_B_lm)$`Pr(>F)`

I_cc_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum), data = I_cc)
I_cc_Ae_emm <- data.frame(emmeans(I_cc_Ae_lm , specs = "ExpCond"))
zb <- anova(I_cc_Ae_lm)$`Pr(>F)`

I_cc_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum), data = I_cc)
I_cc_Av_emm <- data.frame(emmeans(I_cc_Av_lm , specs = "ExpCond"))
zc <- anova(I_cc_Av_lm)$`Pr(>F)`

I_cc_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum), data = I_cc)
I_cc_2pib_emm <- data.frame(emmeans(I_cc_Av_lm , specs = "ExpCond"))
zd <- anova(I_cc_2pib_lm)$`Pr(>F)`

I_cc_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum), data = I_cc)
I_cc_ton_emm <- data.frame(emmeans(I_cc_ton_lm , specs = "ExpCond"))
ze <- anova(I_cc_ton_lm)$`Pr(>F)`


# MHC I/IIA...............................................................
I.IIA_cc <- control_df %>% filter(FiberType == "I/IIA")

I.IIA_cc_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = I.IIA_cc)
I.IIA_cc_B_emm <- data.frame(emmeans(I.IIA_cc_B_lm , specs = "ExpCond"))
zf <- anova(I.IIA_cc_B_lm)$`Pr(>F)`

I.IIA_cc_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum), data = I.IIA_cc)
I.IIA_cc_Ae_emm <- data.frame(emmeans(I.IIA_cc_Ae_lm , specs = "ExpCond"))
zg <- anova(I.IIA_cc_Ae_lm)$`Pr(>F)`

I.IIA_cc_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum), data = I.IIA_cc)
I.IIA_cc_Av_emm <- data.frame(emmeans(I.IIA_cc_Av_lm , specs = "ExpCond"))
zh <- anova(I.IIA_cc_Av_lm)$`Pr(>F)`

I.IIA_cc_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum), data = I.IIA_cc)
I.IIA_cc_2pib_emm <- data.frame(emmeans(I.IIA_cc_Av_lm , specs = "ExpCond"))
zi <- anova(I.IIA_cc_2pib_lm)$`Pr(>F)`

I.IIA_cc_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum), data = I.IIA_cc)
I.IIA_cc_ton_emm <- data.frame(emmeans(I.IIA_cc_ton_lm , specs = "ExpCond"))
zj <- anova(I.IIA_cc_ton_lm)$`Pr(>F)`

# MHC II.........................................................................
IIA_cc <- control_df %>% filter(FiberType == "IIA")

IIA_cc_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = IIA_cc)
IIA_cc_B_emm <- data.frame(emmeans(IIA_cc_B_lm , specs = "ExpCond"))
zk <- anova(IIA_cc_B_lm)$`Pr(>F)`

IIA_cc_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum), data = IIA_cc)
IIA_cc_Ae_emm <- data.frame(emmeans(IIA_cc_Ae_lm , specs = "ExpCond"))
zl <- anova(IIA_cc_Ae_lm)$`Pr(>F)`

IIA_cc_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum), data = IIA_cc)
IIA_cc_Av_emm <- data.frame(emmeans(IIA_cc_Av_lm , specs = "ExpCond"))
zm <- anova(IIA_cc_Av_lm)$`Pr(>F)`

IIA_cc_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum), data = IIA_cc)
IIA_cc_2pib_emm <- data.frame(emmeans(IIA_cc_Av_lm , specs = "ExpCond"))
zn <- anova(IIA_cc_2pib_lm)$`Pr(>F)`

IIA_cc_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum), data = IIA_cc)
IIA_cc_ton_emm <- data.frame(emmeans(IIA_cc_ton_lm , specs = "ExpCond"))
zo <- anova(IIA_cc_ton_lm)$`Pr(>F)`

# MHC IIX ........................................................
IIAX_cc <- control_df %>% filter(FiberType == "IIA/IIX")

IIAX_cc_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = IIAX_cc)
IIAX_cc_B_emm <- data.frame(emmeans(IIAX_cc_B_lm , specs = "ExpCond"))
zp <- anova(IIAX_cc_B_lm)$`Pr(>F)`

IIAX_cc_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum), data = IIAX_cc)
IIAX_cc_Ae_emm <- data.frame(emmeans(IIAX_cc_Ae_lm , specs = "ExpCond"))
zq <- anova(IIAX_cc_Ae_lm)$`Pr(>F)`

IIAX_cc_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum), data = IIAX_cc)
IIAX_cc_Av_emm <- data.frame(emmeans(IIAX_cc_Av_lm , specs = "ExpCond"))
zr <- anova(IIAX_cc_Av_lm)$`Pr(>F)`

IIAX_cc_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum), data = IIAX_cc)
IIAX_cc_2pib_emm <- data.frame(emmeans(IIAX_cc_Av_lm , specs = "ExpCond"))
zs <- anova(IIAX_cc_2pib_lm)$`Pr(>F)`

IIAX_cc_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum), data = IIAX_cc)
IIAX_cc_ton_emm <- data.frame(emmeans(IIAX_cc_ton_lm , specs = "ExpCond"))
zt <- anova(IIAX_cc_ton_lm)$`Pr(>F)`

Fig_3_cc_emm <- rbind(I_cc_B_emm,
                       I_cc_Ae_emm,
                       I_cc_Av_emm,
                       I_cc_2pib_emm,
                       I_cc_ton_emm,
                       I.IIA_cc_B_emm,
                       I.IIA_cc_Ae_emm,
                       I.IIA_cc_Av_emm,
                       I.IIA_cc_2pib_emm,
                       I.IIA_cc_ton_emm,
                       IIA_cc_B_emm,
                       IIA_cc_Ae_emm,
                       IIA_cc_Av_emm,
                       IIA_cc_2pib_emm,
                       IIA_cc_ton_emm,
                       IIAX_cc_B_emm,
                       IIAX_cc_Ae_emm,
                       IIAX_cc_Av_emm,
                       IIAX_cc_2pib_emm,
                       IIAX_cc_ton_emm) %>% 
  mutate(Fiber_Type = c("I","I","I",'I',"I","I","I","I","I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"
  ), .before = emmean) %>% 
  mutate(Value = c("B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton"
  ), .before = emmean)

Fig_3_cc_anova <- data.frame(rbind(za,zb,zc,zd,ze,zf,zg,zh,zi,zj,zk,zl,zm,zn,zo,zp,zq,zr,zs,zt)) %>%   
  mutate(Fiber_Type = c("I","I","I","I","I", 
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA", "IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX")) %>%    
  mutate(Value = c("B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton")) 

colnames(Fig_3_cc_anova) <- c("p_value", "Fiber_Type", "Value")

Fig3_cc <- list(Fig_3_cc_emm,Fig_3_cc_anova)

names(Fig3_cc) <- c("Figure 3 Con vs Con - EMM",
                     "Figure 3 Con vs Con -Anova")

writexl::write_xlsx(Fig3_cc, path = "Woods_AuroraMaster_Figure3_ControlvsControl.xlsx")



### Figure 4: Bar Graphs-----------------------------------------------------------------------

I_Fig4 <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(Force = CSA*PoControl25C) 

I_Fig4_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I_Fig4)
I_Fig4_Po_emm <- data.frame(emmeans(I_Fig4_Po_lm, specs = "ExpCond"))  
am <- anova(I_Fig4_Po_lm)$`Pr(>F)`

I_Fig4_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I_Fig4)
I_Fig4_Force_emm <- data.frame(emmeans(I_Fig4_Force_lm, specs = "ExpCond"))  
an <- anova(I_Fig4_Force_lm)$`Pr(>F)`



IIA_Fig4 <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(Force = CSA*PoControl25C) 

IIA_Fig4_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA_Fig4)
IIA_Fig4_Po_emm <- data.frame(emmeans(IIA_Fig4_Po_lm, specs = "ExpCond"))  
ao <- anova(IIA_Fig4_Po_lm)$`Pr(>F)`

IIA_Fig4_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA_Fig4)
IIA_Fig4_Force_emm <- data.frame(emmeans(IIA_Fig4_Force_lm, specs = "ExpCond"))  
ap <- anova(IIA_Fig4_Force_lm)$`Pr(>F)`

Fig4_emm <- rbind(I_Fig4_Po_emm,
                  I_Fig4_Force_emm,
                  IIA_Fig4_Po_emm,
                  IIA_Fig4_Force_emm) %>%
  mutate(Fiber_Type = c("I", "I","I", "I", "IIA", "IIA","IIA", "IIA"), .before = emmean) %>% 
  mutate(Value = c("ST", "Force","ST", "Force","ST", "Force","ST", "Force"), .before = emmean)

  
 Fig4_anova <- data.frame(rbind(am, an, ao, ap)) %>% 
   mutate(Fiber_Type = c("I", "I", "IIA", "IIA")) %>% 
   mutate(Value = c("ST", "Force", "ST", "Force"))
  
 colnames(Fig4_anova) <- c("p_value", "Fiber_Type", "Value")
 
Fig4 <- list(Fig4_emm, Fig4_anova)
  
names(Fig4) <- c("Figure 4- EMM",
                      "Figure 4-Anova")


writexl::write_xlsx(Fig4, path = "Woods_AuroraMaster_Figure4.xlsx")



### Figure 4: Scatter plots ---------------------------------------------------

I_Fig4 <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(Force = CSA*PoControl25C) 

IIA_Fig4 <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(Force = CSA*PoControl25C) 

I_Fig4_con <- I_Fig4 %>% filter(ExpCondnum == 10)
I_Fig4_con_lm <- lm(I_Fig4_con$Force ~ I_Fig4_con$csa)
I_Fig4_con$mdl <- predict(I_Fig4_con_lm)

I_Fig4_condatp <- I_Fig4 %>% filter(ExpCondnum == 4)
I_Fig4_condatp_lm <- lm(I_Fig4_condatp$Force ~ I_Fig4_condatp$csa)
I_Fig4_condatp$mdl <- predict(I_Fig4_condatp_lm)

(I_Fig4_scat <- ggplot(data = I_Fig4,
                       aes(x = csa, y = Force)) +
    geom_point(aes(shape = ExpCond), size = 1.5) +
    geom_line(data = I_Fig4_con , aes(y = mdl), linetype = "solid", size = 1) +
    geom_line(data = I_Fig4_condatp, aes(y = mdl), linetype = "longdash", size = 1)
)

IIA_Fig4_con <- IIA_Fig4 %>% filter(ExpCondnum == 10)
IIA_Fig4_con_lm <- lm(IIA_Fig4_con$Force ~ IIA_Fig4_con$csa)
IIA_Fig4_con$mdl <- predict(IIA_Fig4_con_lm)

IIA_Fig4_condatp <- IIA_Fig4 %>% filter(ExpCondnum == 4)
IIA_Fig4_condatp_lm <- lm(IIA_Fig4_condatp$Force ~ IIA_Fig4_condatp$csa)
IIA_Fig4_condatp$mdl <- predict(IIA_Fig4_condatp_lm)

(IIA_Fig4_scat <- ggplot(data = IIA_Fig4,
                       aes(x = csa, y = Force)) +
    geom_point(aes(shape = ExpCond), size = 1.5) +
    geom_line(data = IIA_Fig4_con , aes(y = mdl), linetype = "solid", size = 1) +
    geom_line(data = IIA_Fig4_condatp, aes(y = mdl), linetype = "longdash", size = 1)
)

### Figure 5: Bar Plots --------------------------------------------------

df5 <- read_excel("Aurora_Masters_CS_3-1-23.xlsx") %>%
  filter(FiberTypeNum %in% c(1,2)) %>%
  filter(ExpCondNum %in% c(2:4)) %>%
  group_by(SubjectNum, FiberType, FiberTypeNum, ExpCondNum) %>% 
  mutate(Pdiff_Aelastic = Pdiff_Aelastic + 100) %>% 
  mutate(Pdiff_Aviscous = Pdiff_Aviscous + 100) %>% 
  mutate(Pdiff_B = Pdiff_B + 100) %>% 
  mutate(Pdiff_ton = Pdiff_ton + 100) %>% 
  mutate(Pdiff_2pib = Pdiff_2pib + 100)

df6 <- read_excel("Woods_PercDiff_SpecificTension.xlsx") %>% 
  mutate(Pdiff_ST = Pdiff_ST + 100)
df6_I <- df6 %>% filter(FiberType == "I")
df6_IIA <- df6 %>% filter(FiberType == "IIA")

## MHC I.........................................................................
Fig5_I <- df5 %>% filter(FiberType == "I")

Fig5_I_ST_lmer <- lmer(Pdiff_ST  ~ ExpCond + (1 | SubjectNum), data = df6_I)
Fig5_I_ST_emm <- data.frame(emmeans(Fig5_I_ST_lmer, specs = "ExpCond"))
if ((a1.0 <- anova(Fig5_I_ST_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_I_ST_posthoc <- summary(glht(Fig5_I_ST_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_I_Aelastic_lmer <- lmer(Pdiff_Aelastic ~ ExpCond + (1 | SubjectNum),data = Fig5_I)
Fig5_I_Aelastic_emm <- data.frame(emmeans(Fig5_I_Aelastic_lmer, specs = "ExpCond"))
if ((a1 <- anova(Fig5_I_Aelastic_lmer)$`Pr(>F)`) <0.05) {

  Fig5_I_Aelastic_posthoc <- summary(glht(Fig5_I_Aelastic_lmer, linfct = mcp(ExpCond = "Tukey")))

} else{
  NA
}

Fig5_I_Aviscous_lmer <- lmer(Pdiff_Aviscous ~ ExpCond + (1 | SubjectNum),data = Fig5_I)
Fig5_I_Aviscous_emm <- data.frame(emmeans(Fig5_I_Aviscous_lmer, specs = "ExpCond"))
if ((a2 <- anova(Fig5_I_Aviscous_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_I_Aviscous_posthoc <- summary(glht(Fig5_I_Aviscous_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_I_B_lmer <- lmer(Pdiff_B ~ ExpCond + (1 | SubjectNum),data = Fig5_I)
Fig5_I_B_emm <- data.frame(emmeans(Fig5_I_B_lmer, specs = "ExpCond"))
if ((a3 <- anova(Fig5_I_B_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_I_B_posthoc <- summary(glht(Fig5_I_B_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_I_2pib_lmer <- lmer(Pdiff_2pib ~ ExpCond + (1 | SubjectNum),data = Fig5_I)
Fig5_I_2pib_emm <- data.frame(emmeans(Fig5_I_2pib_lmer, specs = "ExpCond"))
if ((a4 <- anova(Fig5_I_2pib_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_I_2pib_posthoc <- summary(glht(Fig5_I_2pib_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_I_ton_lmer <- lmer(Pdiff_ton ~ ExpCond + (1 | SubjectNum),data = Fig5_I)
Fig5_I_ton_emm <- data.frame(emmeans(Fig5_I_ton_lmer, specs = "ExpCond"))
if ((a5 <- anova(Fig5_I_ton_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_I_ton_posthoc <- summary(glht(Fig5_I_ton_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

## MHC IIA ..............................................
Fig5_IIA <- df5 %>% filter(FiberType == "IIA")

Fig5_IIA_ST_lmer <- lmer(Pdiff_ST  ~ ExpCond + (1 | SubjectNum), data = df6_IIA)
Fig5_IIA_ST_emm <- data.frame(emmeans(Fig5_IIA_ST_lmer, specs = "ExpCond"))
if ((a6 <- anova(Fig5_IIA_ST_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_IIA_ST_posthoc <- summary(glht(Fig5_IIA_ST_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_IIA_Aelastic_lmer <- lmer(Pdiff_Aelastic ~ ExpCond + (1 | SubjectNum),data = Fig5_IIA)
Fig5_IIA_Aelastic_emm <- data.frame(emmeans(Fig5_IIA_Aelastic_lmer, specs = "ExpCond"))
if ((a7 <- anova(Fig5_IIA_Aelastic_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_IIA_Aelastic_posthoc <- summary(glht(Fig5_IIA_Aelastic_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_IIA_Aviscous_lmer <- lmer(Pdiff_Aviscous ~ ExpCond + (1 | SubjectNum),data = Fig5_IIA)
Fig5_IIA_Aviscous_emm <- data.frame(emmeans(Fig5_IIA_Aviscous_lmer, specs = "ExpCond"))
if ((a8 <- anova(Fig5_IIA_Aviscous_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_IIA_Aviscous_posthoc <- summary(glht(Fig5_IIA_Aviscous_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_IIA_B_lmer <- lmer(Pdiff_B ~ ExpCond + (1 | SubjectNum),data = Fig5_IIA)
Fig5_IIA_B_emm <- data.frame(emmeans(Fig5_IIA_B_lmer, specs = "ExpCond"))
if ((a9 <- anova(Fig5_IIA_B_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_IIA_B_posthoc <- summary(glht(Fig5_IIA_B_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_IIA_2pib_lmer <- lmer(Pdiff_2pib ~ ExpCond + (1 | SubjectNum),data = Fig5_IIA)
Fig5_IIA_2pib_emm <- data.frame(emmeans(Fig5_IIA_2pib_lmer, specs = "ExpCond"))
if ((a10 <- anova(Fig5_IIA_2pib_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_IIA_2pib_posthoc <- summary(glht(Fig5_IIA_2pib_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_IIA_ton_lmer <- lmer(Pdiff_ton ~ ExpCond + (1 | SubjectNum),data = Fig5_IIA)
Fig5_IIA_ton_emm <- data.frame(emmeans(Fig5_IIA_ton_lmer, specs = "ExpCond"))
if ((a11 <- anova(Fig5_IIA_ton_lmer)$`Pr(>F)`) <0.05) {
  
  Fig5_IIA_ton_posthoc <- summary(glht(Fig5_IIA_ton_lmer, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

Fig5_emm <- rbind(Fig5_I_ST_emm,
                  Fig5_IIA_ST_emm,
                  Fig5_I_Aelastic_emm,
                  Fig5_IIA_Aelastic_emm,
                  Fig5_I_Aviscous_emm,
                  Fig5_IIA_Aviscous_emm,
                  Fig5_I_B_emm,
                  Fig5_IIA_B_emm,
                  Fig5_I_2pib_emm,
                  Fig5_IIA_2pib_emm,
                  Fig5_I_ton_emm,
                  Fig5_IIA_ton_emm) %>% 
  mutate(FiberType = c("I","I","I", "IIA","IIA","IIA",
                       "I","I","I", "IIA","IIA","IIA",
                       "I","I","I", "IIA","IIA","IIA",
                       "I","I","I", "IIA","IIA","IIA",
                       "I","I","I", "IIA","IIA","IIA",
                       "I","I","I", "IIA" ,"IIA","IIA"), .before = emmean) %>% 
  mutate(Variable = c("Specific Tension", "Specific Tension", "Specific Tension", "Specific Tension", "Specific Tension", "Specific Tension", 
                      "Aelastic", "Aelastic", "Aelastic","Aelastic","Aelastic","Aelastic",
                      "Aviscous", "Aviscous","Aviscous","Aviscous","Aviscous","Aviscous",
                      "B", "B", "B","B","B","B",
                      "2pib", "2pib","2pib","2pib","2pib","2pib",
                      "ton","ton","ton","ton","ton", "ton"), .before = ExpCond)

Fig5_anova <- data.frame(rbind(a1.0,a6, a1,a7,a2,a8,a3,a9,a4,a10,a5,a11)) %>% 
  mutate(FiberType = c("I", "IIA","I", "IIA","I", "IIA","I", "IIA","I", "IIA","I", "IIA" )) %>% 
  mutate(Variable = c("Specific Tension", "Specific Tension",
                      "Aleastic", "Aleastic",
                      "Aviscous", "Aviscous",
                      "B", "B",
                      "2pib", "2pib",
                      "ton", "ton"))
colnames(Fig5_anova) <- c("p-value", "Fiber Type", "Variable")

Fig5_posthoc <- data.frame(rbind(table_glht(Fig5_I_ST_posthoc),
                                 table_glht(Fig5_IIA_ST_posthoc),
                                 table_glht(Fig5_IIA_Aviscous_posthoc),
                                 table_glht(Fig5_I_B_posthoc),
                                 table_glht(Fig5_I_2pib_posthoc),
                                 table_glht(Fig5_IIA_2pib_posthoc),
                                 table_glht(Fig5_I_ton_posthoc),
                                 table_glht(Fig5_IIA_ton_posthoc))) %>% 
  mutate(FiberType = c("I","I","I",
                       "IIA","IIA","IIA",
                       "IIA","IIA","IIA",
                       "I","I","I",
                       "I","I","I",
                       "IIA","IIA","IIA",
                       "I","I","I",
                       "IIA","IIA","IIA" ), .before = Estimate) %>% 
  mutate(Variable = c("Specific Tension","Specific Tension","Specific Tension",
                      "Specific Tension","Specific Tension","Specific Tension",
                      "Aviscous","Aviscous","Aviscous",
                      "B","B","B",
                      "2pib","2pib","2pib",
                      "2pib","2pib","2pib",
                      "ton","ton","ton",
                      "ton","ton","ton"), .before = FiberType) %>% 
  mutate(Comparison = c("Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP",
                        "Fatigue vs. ControldATP", "FatiguedATP vs. ControldATP", "Fatigue vs. FatiguedATP"), .after = Variable)

Fig5_analysis <- list(Fig5_emm, Fig5_anova, Fig5_posthoc)
writexl::write_xlsx(Fig5_analysis, "Aurora_Figure5_Analysis_normalized.xlsx")
