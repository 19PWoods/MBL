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

I <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
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

I.IIA <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum == 4) %>% 
  filter(ExpCondnum %in% c(1:3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

I.IIA_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA)
I.IIA_Po_emm <- data.frame(emmeans(I.IIA_Po_lm, specs = "ExpCond"))  
if ((c <- anova(I.IIA_Po_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Po_hoc <- summary(glht(I.IIA_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I.IIA)
I.IIA_Force_emm <- data.frame(emmeans(I.IIA_Force_lm, specs = "ExpCond"))  
if ((d <- anova(I.IIA_Force_lm)$`Pr(>F)`) <0.05) {
  
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

IIAX <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
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

Fig_1_anova <- data.frame(rbind(a,b,c,d,e,f,g,h)) %>% 
  mutate(Fiber_Type = c("I","I", "I/IIA","I/IIA", "IIA","IIA", "IIAX","IIAX")) %>% 
  mutate(Value = c("ST", "Force","ST", "Force","ST", "Force","ST", "Force")) 
colnames(Fig_1_anova) <- c("p_value", "Fiber_Type", "Value")

Fig_1_posthoc <- data.frame(rbind(table_glht(I_Po_hoc),
                            table_glht(I_Force_hoc),
                            table_glht(I.IIA_Po_hoc),
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

con_fat_data <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(Group == 1) %>%
  filter(ExpCondnum %in% c(1,2)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)


## IIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII
I_cf <- con_fat_data %>% filter(FiberType == "I")

I_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                    data = I_cf)
I_Fig3_B_emm <- data.frame(emmeans(I_Fig3_B_lm, specs = "ExpCond"))
if ((i <- anova(I_Fig3_B_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_B_hoc <- summary(glht(I_Fig3_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                       data = I_cf)
I_Fig3_2pib_emm <- data.frame(emmeans(I_Fig3_2pib_lm, specs = "ExpCond"))
if ((j <- anova(I_Fig3_2pib_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_2pib_hoc <- summary(glht(I_Fig3_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                       data = I_cf)
I_Fig3_ton_emm <- data.frame(emmeans(I_Fig3_ton_lm, specs = "ExpCond"))
if ((k <- anova(I_Fig3_ton_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_ton_hoc <- summary(glht(I_Fig3_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 

I.IIA_cf <- con_fat_data %>% filter(Fibertypenum == 4)

I.IIA_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                    data = I.IIA_cf)
I.IIA_Fig3_B_emm <- data.frame(emmeans(I.IIA_Fig3_B_lm, specs = "ExpCond"))
if ((l <- anova(I.IIA_Fig3_B_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_B_hoc <- summary(glht(I.IIA_Fig3_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                       data = I.IIA_cf)
I.IIA_Fig3_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_2pib_lm, specs = "ExpCond"))
if ((m <- anova(I.IIA_Fig3_2pib_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_2pib_hoc <- summary(glht(I.IIA_Fig3_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                      data = I.IIA_cf)
I.IIA_Fig3_ton_emm <- data.frame(emmeans(I.IIA_Fig3_ton_lm, specs = "ExpCond"))
if ((n <- anova(I.IIA_Fig3_ton_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_ton_hoc <- summary(glht(I.IIA_Fig3_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA 

IIA_cf <- con_fat_data %>% filter(Fibertypenum == 2)

IIA_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                        data = IIA_cf)
IIA_Fig3_B_emm <- data.frame(emmeans(IIA_Fig3_B_lm, specs = "ExpCond"))
if ((o <- anova(IIA_Fig3_B_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_B_hoc <- summary(glht(IIA_Fig3_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                           data = IIA_cf)
IIA_Fig3_2pib_emm <- data.frame(emmeans(IIA_Fig3_2pib_lm, specs = "ExpCond"))
if ((p <- anova(IIA_Fig3_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_2pib_hoc <- summary(glht(IIA_Fig3_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}
IIA_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                          data = IIA_cf)
IIA_Fig3_ton_emm <- data.frame(emmeans(IIA_Fig3_ton_lm, specs = "ExpCond"))
if ((q <- anova(IIA_Fig3_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_ton_hoc <- summary(glht(IIA_Fig3_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX 
## Getting error when running lmer(), which I believe is due to small sample size


# IIAX_cf <- con_fat_data %>% filter(Fibertypenum == 5)
# IIAX_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
#                       data = IIAX_cf)
# IIAX_Fig3_B_emm <- data.frame(emmeans(IIAX_Fig3_B_lm, specs = "ExpCond"))
# if ((o <- anova(IIAX_Fig3_B_lm)$`Pr(>F)`) <0.05) {
#   
#   IIAX_Fig3_B_hoc <- summary(glht(IIAX_Fig3_B_lm, linfct = mcp(ExpCond = "Tukey")))
#   
# } else{
#   NA
# }
# 
# IIAX_Fig3_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
#                          data = IIAX_cf)
# IIAX_Fig3_2pib_emm <- data.frame(emmeans(IIAX_Fig3_2pib_lm, specs = "ExpCond"))
# if ((p <- anova(IIAX_Fig3_2pib_lm)$`Pr(>F)`) <0.05) {
#   
#   IIAX_Fig3_2pib_hoc <- summary(glht(IIAX_Fig3_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
#   
# } else{
#   NA
# }
# IIAX_Fig3_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
#                         data = IIAX_cf)
# IIAX_Fig3_ton_emm <- data.frame(emmeans(IIAX_Fig3_ton_lm, specs = "ExpCond"))
# if ((q <- anova(IIAX_Fig3_ton_lm)$`Pr(>F)`) <0.05) {
#   
#   IIAX_Fig3_ton_hoc <- summary(glht(IIAX_Fig3_ton_lm, linfct = mcp(ExpCond = "Tukey")))
#   
# } else{
#   NA
# }
### Figure 3: Control + dATP --------------------------------------------------------------

con_fatdatp_data <- read_excel("Aurora_Masters_7-25-22.xlsx") %>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(Group == 2) %>%
  filter(ExpCondnum %in% c(1,3)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>%
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C)


# I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I I 
I_cdatp <- con_fatdatp_data %>% filter(FiberType == "I")

I_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond)| SubjectNum),data = I_cdatp)
I_Fig3_gr2_B_emm <- data.frame(emmeans(I_Fig3_gr2_B_lm, specs = "ExpCond"))
if ((r <- anova(I_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_B_hoc <- summary(glht(I_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1+ as.factor(ExpCond) | SubjectNum),data = I_cdatp)
I_Fig3_gr2_2pib_emm <- data.frame(emmeans(I_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((s <- anova(I_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_B_hoc <- summary(glht(I_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 | + as.factor(ExpCond) | SubjectNum),data = I_cdatp)
I_Fig3_gr2_ton_emm <- data.frame(emmeans(I_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((t <- anova(I_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_B_hoc <- summary(glht(I_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 
I.IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 4)

I.IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_B_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_B_lm, specs = "ExpCond"))
if ((u <- anova(I.IIA_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_gr2_B_hoc <- summary(glht(I.IIA_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond)| SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((v <- anova(I.IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_gr2_B_hoc <- summary(glht(I.IIA_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 | + as.factor(ExpCond) |SubjectNum),data = I.IIA_cdatp)
I.IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(I.IIA_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((w <- anova(I.IIA_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_gr2_B_hoc <- summary(glht(I.IIA_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


# IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA IIA 
IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 2)

IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_B_emm <- data.frame(emmeans(IIA_Fig3_gr2_B_lm, specs = "ExpCond"))
if ((x <- anova(IIA_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIA_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((y <- anova(IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(IIA_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((z <- anova(IIA_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX IIAX
IIAX_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 5)

IIAX_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_B_emm <- data.frame(emmeans(IIAX_Fig3_gr2_B_lm, specs = "ExpCond"))
if ((x <- anova(IIAX_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIAX_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((y <- anova(IIAX_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_ton_emm <- data.frame(emmeans(IIAX_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((z <- anova(IIAX_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

