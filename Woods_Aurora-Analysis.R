library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)

setwd("C:/Users/Phil/Dropbox/MBL/Aurora Fatigue Data")


my_data <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(ExpCondnum %in% c(1:3)) %>%
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)
  # select(SubjectNum, AgeGrp, Sex,Leg, FiberType,Fibertypenum,Fibernum,ExpCond,ExpCondnum,
  #        CSA,Force,PoControl25C,MSRunNum, AkNm2,k,BkNm2,lbHz,CkNm2,lcHz,ton,twopib)

table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""), 
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""), two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
  
}

### Figure 1-----------------------------------------------------------------

I <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 


I_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Po_emm <- data.frame(emmeans(I_Po_lm, specs = "ExpCond"))  
if ((a <- anova(I_Po_lm)$`Pr(>F)`) <0.05) {
 
   I_Po_hoc <- summary(glht(I_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
   } else{
     NA
}

I_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I)
I_Force_emm <- data.frame(emmeans(I_Force_lm, specs = "ExpCond"))  
if ((b<- anova(I_Force_lm)$`Pr(>F)`) <0.05) {
  
  I_Force_hoc <- summary(glht(I_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 4) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

IIA_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA)
IIA_Po_emm <- data.frame(emmeans(IIA_Po_lm, specs = "ExpCond"))  
if ((c <- anova(IIA_Po_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Po_hoc <- summary(glht(IIA_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA)
IIA_Force_emm <- data.frame(emmeans(IIA_Force_lm, specs = "ExpCond"))  
if ((d <- anova(IIA_Force_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Force_hoc <- summary(glht(IIA_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

IIA_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA)
IIA_Po_emm <- data.frame(emmeans(IIA_Po_lm, specs = "ExpCond"))  
if ((e<- anova(IIA_Po_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Po_hoc <- summary(glht(IIA_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA)
IIA_Force_emm <- data.frame(emmeans(IIA_Force_lm, specs = "ExpCond"))  
if ((f <- anova(IIA_Force_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Force_hoc <- summary(glht(IIA_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 5) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

IIAX_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIAX)
IIAX_Po_emm <- data.frame(emmeans(IIAX_Po_lm, specs = "ExpCond"))
if (( g<- anova(IIAX_Po_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Po_hoc <- summary(glht(IIAX_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIAX)
IIAX_Force_emm <- data.frame(emmeans(IIAX_Force_lm, specs = "ExpCond"))  
if ((h<- anova(IIAX_Force_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Force_hoc <- summary(glht(IIAX_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


Fig_1_emm <- rbind(I_Po_emm,
                        I_Force_emm,
                        IIA_Po_emm,
                        IIA_Force_emm,
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

Fig_1_anova <- data.frame(rbind(a,b,c,d,e,f,g,h)) %>% 
  mutate(Fiber_Type = c("I","I", "I/IIA","I/IIA", "IIA","IIA", "IIAX","IIAX")) %>% 
  mutate(Value = c("ST", "Force","ST", "Force","ST", "Force","ST", "Force")) 
colnames(Fig_1_anova) <- c("p_value", "Fiber_Type", "Value")

Fig_1_posthoc <- data.frame(rbind(table_glht(I_Po_hoc),
                            table_glht(I_Force_hoc),
                            table_glht(IIA_Po_hoc),
                            table_glht(IIA_Po_hoc),
                            table_glht(IIA_Force_hoc),
                            table_glht(IIAX_Po_hoc),
                            table_glht(IIAX_Force_hoc))) %>%   
  mutate(Fiber_Type = c("I","I","I","I","I","I",
                        "I.IIA","I.IIA","I.IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX","IIAX"),
         .before = Estimate) %>% 
  mutate(Value = c("ST","ST","ST",
                   "Force","Force","Force",
                   "ST","ST","ST",
                   "ST","ST","ST",
                   "Force","Force","Force",
                   "ST","ST","ST",
                   "Force","Force","Force"),
         .before = Fiber_Type) %>% 
  mutate(Comparison = c("Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0",
                        "Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0",
                        "Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0",
                        "Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0",
                        "Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0",
                        "Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0",
                        "Fatigue - Control == 0",
                        "Fatigue+dATP - Control == 0",
                        "Fatigue+dATP - Fatigue == 0"),
         .before = Value)


Figure_1_Final <- list(Fig_1_emm,Fig_1_anova,Fig_1_posthoc)

names(Figure_1_Final) <- c("Figure 1-EMM", "Figure 1-Anova", "Figure 1-Posthoc")

writexl::write_xlsx(Figure_1_Final, path = "Woods_AuroraMaster_Figure1.xlsx")

## Figure 3: Control + Fatigue  -----------------------------------------------------------------------------------

con_fat_data <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(Grp == 1) %>%
  filter(ExpCondnum %in% c(1,2)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)


## IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
I_cf <- con_fat_data %>% filter(FiberType == "I")

I_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cf)
I_Fig3_B_emm <- data.frame(emmeans(I_Fig3_B_lm, specs = "ExpCond"))
i <- anova(I_Fig3_B_lm)$`Pr(>F)`


I_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cf)
I_Fig3_Ae_emm <- data.frame(emmeans(I_Fig3_Ae_lm, specs = "ExpCond"))
i.1 <- anova(I_Fig3_Ae_lm)$`Pr(>F)` 

I_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cf)
I_Fig3_Av_emm <- data.frame(emmeans(I_Fig3_Av_lm, specs = "ExpCond"))
i.2 <- anova(I_Fig3_Av_lm)$`Pr(>F)`


I_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cf)
I_Fig3_2pib_emm <- data.frame(emmeans(I_Fig3_2pib_lm, specs = "ExpCond"))
j <- anova(I_Fig3_2pib_lm)$`Pr(>F)` 

I_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cf)
I_Fig3_ton_emm <- data.frame(emmeans(I_Fig3_ton_lm, specs = "ExpCond"))
k <- anova(I_Fig3_ton_lm)$`Pr(>F)` 

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 

I.IIA_cf <- con_fat_data %>% filter(Fibertypenum == 4)

I.IIA_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA_cf)
I.IIA_Fig3_B_emm <- data.frame(emmeans(I.IIA_Fig3_B_lm, specs = "ExpCond"))
l <- anova(I.IIA_Fig3_B_lm)$`Pr(>F)`

I.IIA_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_Ae_emm <- data.frame(emmeans(I.IIA_Fig3_Ae_lm, specs = "ExpCond"))
l.1 <- anova(I.IIA_Fig3_Ae_lm)$`Pr(>F)` 

I.IIA_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_Av_emm <- data.frame(emmeans(I.IIA_Fig3_Av_lm, specs = "ExpCond"))
l.2 <- anova(I.IIA_Fig3_Av_lm)$`Pr(>F)`


I.IIA_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_2pib_lm, specs = "ExpCond"))
m <- anova(I.IIA_Fig3_2pib_lm)$`Pr(>F)` 

I.IIA_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cf)
I.IIA_Fig3_ton_emm <- data.frame(emmeans(I.IIA_Fig3_ton_lm, specs = "ExpCond"))
n <- anova(I.IIA_Fig3_ton_lm)$`Pr(>F)`

# IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA 

IIA_cf <- con_fat_data %>% filter(Fibertypenum == 2)

IIA_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cf)
IIA_Fig3_B_emm <- data.frame(emmeans(IIA_Fig3_B_lm, specs = "ExpCond"))
o <- anova(IIA_Fig3_B_lm)$`Pr(>F)` 

IIA_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cf)
IIA_Fig3_Ae_emm <- data.frame(emmeans(IIA_Fig3_Ae_lm, specs = "ExpCond"))
o.1 <- anova(IIA_Fig3_Ae_lm)$`Pr(>F)`

IIA_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cf)
IIA_Fig3_Av_emm <- data.frame(emmeans(IIA_Fig3_Av_lm, specs = "ExpCond"))
o.2 <- anova(IIA_Fig3_Av_lm)$`Pr(>F)`

IIA_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cf)
IIA_Fig3_2pib_emm <- data.frame(emmeans(IIA_Fig3_2pib_lm, specs = "ExpCond"))
p <- anova(IIA_Fig3_2pib_lm)$`Pr(>F)`
    
IIA_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cf)
IIA_Fig3_ton_emm <- data.frame(emmeans(IIA_Fig3_ton_lm, specs = "ExpCond"))
q <- anova(IIA_Fig3_ton_lm)$`Pr(>F)` 
  

# IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX 

IIAX_cf <- con_fat_data %>% filter(Fibertypenum == 5)


IIAX_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum),data = IIAX_cf)

IIAX_Fig3_B_emm <- data.frame(emmeans(IIAX_Fig3_B_lm, specs = "ExpCond"))
q.1 <- anova(IIAX_Fig3_B_lm)$`Pr(>F)` 

IIAX_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1  | SubjectNum),data = IIAX_cf)
IIAX_Fig3_Ae_emm <- data.frame(emmeans(IIAX_Fig3_Ae_lm, specs = "ExpCond"))
q.2 <- anova(IIAX_Fig3_Ae_lm)$`Pr(>F)`

IIAX_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1  | SubjectNum),data = IIAX_cf)
IIAX_Fig3_Av_emm <- data.frame(emmeans(IIAX_Fig3_Av_lm, specs = "ExpCond"))
q.3 <- anova(IIAX_Fig3_Av_lm)$`Pr(>F)`

IIAX_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = IIAX_cf)
IIAX_Fig3_2pib_emm <- data.frame(emmeans(IIAX_Fig3_2pib_lm, specs = "ExpCond"))
q.4 <- anova(IIAX_Fig3_2pib_lm)$`Pr(>F)` 

IIAX_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = IIAX_cf)
IIAX_Fig3_ton_emm <- data.frame(emmeans(IIAX_Fig3_ton_lm, specs = "ExpCond"))
q.5 <- anova(IIAX_Fig3_ton_lm)$`Pr(>F)`

Fig_3_cf_emm <- rbind(I_Fig3_B_emm,
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

Fig_3_cf_anova <- data.frame(rbind(i,i.1, i.2, j, k, l, l.1, l.2, m,n, o, o.1, o.2, p, q, q.1,q.2,q.3,q.4,q.5)) %>% 
  mutate(Fiber_Type = c("I","I","I","I","I", 
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA", "IIA","IIA","IIA",
                        "IIAX","IIAX","IIAX","IIAX","IIAX")) %>%   
  mutate(Value = c("B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton")) 
colnames(Fig_3_cf_anova) <- c("p_value", "Fiber_Type", "Value")


Fig3_cf <- list(Fig_3_cf_emm,Fig_3_cf_anova)

names(Fig3_cf) <- c("Figure 3 CvF -EMM", 
                    "Figure 3 CvF -Anova")

writexl::write_xlsx(Fig3_cf, path = "Woods_AuroraMaster_Figure3_ControlvFatigue.xlsx")

### Figure 3: Control + dATP --------------------------------------------------------------

con_fatdatp_data <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(Grp == 2) %>%
  filter(ExpCondnum %in% c(1,3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)


# I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I 
I_cdatp <- con_fatdatp_data %>% filter(FiberType == "I")

I_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond)| SubjectNum),data = I_cdatp)
I_Fig3_gr2_B_emm <- data.frame(emmeans(I_Fig3_gr2_B_lm, specs = "ExpCond"))
r <- anova(I_Fig3_gr2_B_lm)$`Pr(>F)`

I_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cdatp)
I_Fig3_gr2_Ae_emm <- data.frame(emmeans(I_Fig3_gr2_Ae_lm, specs = "ExpCond"))
r.1 <- anova(I_Fig3_gr2_Ae_lm)$`Pr(>F)`

I_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cdatp)
I_Fig3_gr2_Av_emm <- data.frame(emmeans(I_Fig3_gr2_Av_lm, specs = "ExpCond"))
r.2 <- anova(I_Fig3_gr2_Av_lm)$`Pr(>F)`


I_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cdatp)
I_Fig3_gr2_2pib_emm <- data.frame(emmeans(I_Fig3_gr2_2pib_lm, specs = "ExpCond"))
s <- anova(I_Fig3_gr2_2pib_lm)$`Pr(>F)`

I_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1  + as.factor(ExpCond) | SubjectNum), data = I_cdatp)
I_Fig3_gr2_ton_emm <- data.frame(emmeans(I_Fig3_gr2_ton_lm, specs = "ExpCond"))
t <- anova(I_Fig3_gr2_ton_lm)$`Pr(>F)`

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 
I.IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 4)

I.IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_B_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_B_lm, specs = "ExpCond"))
u <- anova(I.IIA_Fig3_gr2_B_lm)$`Pr(>F)`

I.IIA_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_Ae_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_Ae_lm, specs = "ExpCond"))
u.1 <- anova(I.IIA_Fig3_gr2_Ae_lm)$`Pr(>F)`

I.IIA_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_Av_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_Av_lm, specs = "ExpCond"))
u.2 <- anova(I.IIA_Fig3_gr2_Av_lm)$`Pr(>F)`

I.IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond)| SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_2pib_lm, specs = "ExpCond"))
v <- anova(I.IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`

I.IIA_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) |SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_ton_lm, specs = "ExpCond"))
w <- anova(I.IIA_Fig3_gr2_ton_lm)$`Pr(>F)`


# IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA 
IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 2)

IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_B_emm <- data.frame(emmeans(IIA_Fig3_gr2_B_lm, specs = "ExpCond"))
x <- anova(IIA_Fig3_gr2_B_lm)$`Pr(>F)`

IIA_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIA_Fig3_gr2_Ae_lm, specs = "ExpCond"))
x.1 <- anova(IIA_Fig3_gr2_Ae_lm)$`Pr(>F)`

IIA_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_Av_emm <- data.frame(emmeans(IIA_Fig3_gr2_Av_lm, specs = "ExpCond"))
x.2 <- anova(IIA_Fig3_gr2_Av_lm)$`Pr(>F)`

IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIA_Fig3_gr2_2pib_lm, specs = "ExpCond"))
y <- anova(IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`

IIA_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(IIA_Fig3_gr2_ton_lm, specs = "ExpCond"))
z <- anova(IIA_Fig3_gr2_ton_lm)$`Pr(>F)`

# IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX
IIAX_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 5)

IIAX_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_B_emm <- data.frame(emmeans(IIAX_Fig3_gr2_B_lm, specs = "ExpCond"))
z.1 <- anova(IIAX_Fig3_gr2_B_lm)$`Pr(>F)`


IIAX_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIAX_Fig3_gr2_Ae_lm, specs = "ExpCond"))
z.2 <- anova(IIAX_Fig3_gr2_Ae_lm)$`Pr(>F)`

IIAX_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_Av_emm <- data.frame(emmeans(IIAX_Fig3_gr2_Av_lm, specs = "ExpCond"))
z.3 <- anova(IIAX_Fig3_gr2_Av_lm)$`Pr(>F)`

IIAX_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIAX_Fig3_gr2_2pib_lm, specs = "ExpCond"))
zz <- anova(IIAX_Fig3_gr2_2pib_lm)$`Pr(>F)`


IIAX_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_ton_emm <- data.frame(emmeans(IIAX_Fig3_gr2_ton_lm, specs = "ExpCond"))
zzz <- anova(IIAX_Fig3_gr2_ton_lm)$`Pr(>F)`

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

Fig3_cdatp <- list(Fig_3_cdatp_emm,Fig_3_cdatp_anova)

names(Fig3_cdatp) <- c("Figure 3 C v dATP -EMM",
                       "Figure 3 C vs dATP - Anova")

writexl::write_xlsx(Fig3_cdatp, path = "Woods_AuroraMaster_Figure3_ControlvFatiguedATP.xlsx")

### Figure 3: Fatigue vs Fatigue dATP -----------------------------------------------------------------------

fat_v_datp <- read_excel("Aurora_Masters_CS.xlsx") %>%
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
control_fat <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(ExpCondnum == 1 & Grp == 1) %>% 
  mutate(ExpCond = "Control_Fat")

control_datp <- read_excel("Aurora_Masters_CS.xlsx") %>%
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












### Figure 4----------------------------------------------------------------------------

I_Fig4 <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

I_Fig4_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I_Fig4)
I_Fig4_Po_emm <- data.frame(emmeans(I_Fig4_Po_lm, specs = "ExpCond"))  
am <- anova(I_Fig4_Po_lm)$`Pr(>F)`

I_Fig4_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I_Fig4)
I_Fig4_Force_emm <- data.frame(emmeans(I_Fig4_Force_lm, specs = "ExpCond"))  
an <- anova(I_Fig4_Force_lm)$`Pr(>F)`



IIA_Fig4 <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
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
  mutate(Value = c("Force", "ST", "Force", "ST","Force", "ST", "Force", "ST"), .before = emmean)

  
 Fig4_anova <- data.frame(rbind(am, an, ao, ap)) %>% 
   mutate(Fiber_Type = c("I", "I", "IIA", "IIA")) %>% 
   mutate(Value = c("Force", "ST", "Force", "ST"))
  
 colnames(Fig4_anova) <- c("p_value", "Fiber_Type", "Value")
 
Fig4 <- list(Fig4_emm, Fig4_anova)
  
names(Fig4) <- c("Figure 4- EMM",
                      "Figure 4-Anova")


writexl::write_xlsx(Fig4, path = "Woods_AuroraMaster_Figure4.xlsx")















