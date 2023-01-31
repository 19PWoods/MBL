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

I_Fig3_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                    data = I_cf)
I_Fig3_B_emm <- data.frame(emmeans(I_Fig3_B_lm, specs = "ExpCond"))
if ((i <- anova(I_Fig3_B_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_B_hoc <- summary(glht(I_Fig3_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                    data = I_cf)
I_Fig3_Ae_emm <- data.frame(emmeans(I_Fig3_Ae_lm, specs = "ExpCond"))
if ((i.1 <- anova(I_Fig3_Ae_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_Ae_hoc <- summary(glht(I_Fig3_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                     data = I_cf)
I_Fig3_Av_emm <- data.frame(emmeans(I_Fig3_Av_lm, specs = "ExpCond"))
if ((i.2 <- anova(I_Fig3_Av_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_Av_hoc <- summary(glht(I_Fig3_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
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

I.IIA_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                     data = I.IIA_cf)
I.IIA_Fig3_Ae_emm <- data.frame(emmeans(I.IIA_Fig3_Ae_lm, specs = "ExpCond"))
if ((l.1 <- anova(I.IIA_Fig3_Ae_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_Ae_hoc <- summary(glht(I.IIA_Fig3_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I.IIA_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                     data = I.IIA_cf)
I.IIA_Fig3_Av_emm <- data.frame(emmeans(I.IIA_Fig3_Av_lm, specs = "ExpCond"))
if ((l.2 <- anova(I.IIA_Fig3_Av_lm)$`Pr(>F)`) <0.05) {
  
  I.IIA_Fig3_Av_hoc <- summary(glht(I.IIA_Fig3_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
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

IIA_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                         data = IIA_cf)
IIA_Fig3_Ae_emm <- data.frame(emmeans(IIA_Fig3_Ae_lm, specs = "ExpCond"))
if ((o.1 <- anova(IIA_Fig3_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_Ae_hoc <- summary(glht(IIA_Fig3_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                         data = IIA_cf)
IIA_Fig3_Av_emm <- data.frame(emmeans(IIA_Fig3_Av_lm, specs = "ExpCond"))
if ((o.2 <- anova(IIA_Fig3_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_Av_hoc <- summary(glht(IIA_Fig3_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
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

# IIAX_Fig3_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
#                        data = IIAX_cf)
# IIAX_Fig3_Ae_emm <- data.frame(emmeans(IIAX_Fig3_Ae_lm, specs = "ExpCond"))
# if ((l.1 <- anova(IIAX_Fig3_Ae_lm)$`Pr(>F)`) <0.05) {
#   
#   IIAX_Fig3_Ae_hoc <- summary(glht(IIAX_Fig3_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
#   
# } else{
#   NA
# }
# 
# IIAX_Fig3_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
#                        data = IIAX_cf)
# IIAX_Fig3_Av_emm <- data.frame(emmeans(IIAX_Fig3_Av_lm, specs = "ExpCond"))
# if ((l.2 <- anova(IIAX_Fig3_Av_lm)$`Pr(>F)`) <0.05) {
#   
#   IIAX_Fig3_Av_hoc <- summary(glht(IIAX_Fig3_Av_lm, linfct = mcp(ExpCond = "Tukey")))
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
                      IIA_Fig3_ton_emm) %>% 
  mutate(Fiber_Type = c("I","I","I",'I',"I","I","I","I","I","I",
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA","IIA"
                        ), .before = emmean) %>% 
  mutate(Value = c("B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton",
                   "B","B", "Aelastic", "Aelastic", "Aviscous","Aviscous", "2pib", "2pib", "ton", "ton"
                   ), .before = emmean)

Fig_3_cf_anova <- data.frame(rbind(i,i.1, i.2, j, k, l, l.1, l.2, m,n, o, o.1, o.2, p, q)) %>% 
  mutate(Fiber_Type = c("I","I","I","I","I", 
                        "I/IIA","I/IIA","I/IIA","I/IIA","I/IIA",
                        "IIA","IIA", "IIA","IIA","IIA")) %>%   
  mutate(Value = c("B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton",
                   "B","Aelastic","Aviscous","2pib","ton")) 
colnames(Fig_3_cf_anova) <- c("p_value", "Fiber_Type", "Value")

Fig_3_cf_posthoc <- data.frame(rbind(table_glht(I_Fig3_Ae_hoc),
                                  table_glht(I_Fig3_2pib_hoc),
                                  table_glht(I_Fig3_ton_hoc),
                                  table_glht(IIA_Fig3_B_hoc),
                                  table_glht(IIA_Fig3_2pib_hoc),
                                  table_glht(IIA_Fig3_ton_hoc)
                                  )) %>% 
  mutate(Fiber_Type = c("I", "I", "I", "IIA", "IIA", "IIA"), .before = Estimate) %>% 
  mutate(Comparison = c("Fatigue - Control == 0","Fatigue - Control == 0","Fatigue - Control == 0",
                        "Fatigue - Control == 0","Fatigue - Control == 0","Fatigue - Control == 0"),
         .before = Fiber_Type)

Fig3_cf <- list(Fig_3_cf_emm,Fig_3_cf_anova,Fig_3_cf_posthoc)

names(Fig3_cf) <- c("Figure 3-EMM", "Figure 3-Anova", "Figure 3-Posthoc")

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
if ((r <- anova(I_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_B_hoc <- summary(glht(I_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                     data = I_cdat)
I_Fig3_gr2_Ae_emm <- data.frame(emmeans(I_Fig3_gr2_Ae_lm, specs = "ExpCond"))
if ((r.1 <- anova(I_Fig3_gr2_Ae_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_Ae_hoc <- summary(glht(I_Fig3_gr2_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                     data = I_cdat)
I_Fig3_Av_emm <- data.frame(emmeans(I_Fig3_gr2_Av_lm, specs = "ExpCond"))
if ((r.2 <- anova(I_Fig3_gr2_Av_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_Av_hoc <- summary(glht(I_Fig3_gr2_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


I_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = I_cdatp)
I_Fig3_gr2_2pib_emm <- data.frame(emmeans(I_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((s <- anova(I_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_B_hoc <- summary(glht(I_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1  + as.factor(ExpCond) | SubjectNum), data = I_cdatp)
I_Fig3_gr2_ton_emm <- data.frame(emmeans(I_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((t <- anova(I_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_gr2_B_hoc <- summary(glht(I_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA I/IIA 
IIA_cdatp <- con_fatdatp_data %>% filter(Fibertypenum == 4)

IIA_Fig3_gr2_B_lm <- lmer(BkNm2 ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_B_emm <- data.frame(emmeans(IIA_Fig3_gr2_B_lm, specs = "ExpCond"))
if ((u <- anova(IIA_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                         data = IIA_cdat)
IIA_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIA_Fig3_gr2_Ae_lm, specs = "ExpCond"))
if ((u.1 <- anova(IIA_Fig3_gr2_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_Ae_hoc <- summary(glht(IIA_Fig3_gr2_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                         data = IIA_cdat)
IIA_Fig3_Av_emm <- data.frame(emmeans(IIA_Fig3_gr2_Av_lm, specs = "ExpCond"))
if ((u.2 <- anova(IIA_Fig3_gr2_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_Av_hoc <- summary(glht(IIA_Fig3_gr2_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond)| SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIA_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((v <- anova(IIA_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) |SubjectNum),data = IIA_cdatp)
IIA_Fig3_gr2_ton_emm <- data.frame(emmeans(IIA_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((w <- anova(IIA_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_B_hoc <- summary(glht(IIA_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
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

IIA_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                             data = IIA_cdat)
IIA_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIA_Fig3_gr2_Ae_lm, specs = "ExpCond"))
if ((x.1 <- anova(IIA_Fig3_gr2_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_Ae_hoc <- summary(glht(IIA_Fig3_gr2_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                             data = IIA_cdat)
IIA_Fig3_Av_emm <- data.frame(emmeans(IIA_Fig3_gr2_Av_lm, specs = "ExpCond"))
if ((x.2 <- anova(IIA_Fig3_gr2_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_gr2_Av_hoc <- summary(glht(IIA_Fig3_gr2_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
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
if ((z <- anova(IIAX_Fig3_gr2_B_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


IIAX_Fig3_gr2_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                             data = IIAX_cdat)
IIAX_Fig3_gr2_Ae_emm <- data.frame(emmeans(IIAX_Fig3_gr2_Ae_lm, specs = "ExpCond"))
if ((z.1 <- anova(IIAX_Fig3_gr2_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_Ae_hoc <- summary(glht(IIAX_Fig3_gr2_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_gr2_Av_lm <- lmer(Aviscous ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), 
                             data = IIAX_cdat)
IIAX_Fig3_Av_emm <- data.frame(emmeans(IIAX_Fig3_gr2_Av_lm, specs = "ExpCond"))
if ((z.2 <- anova(IIAX_Fig3_gr2_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_Av_hoc <- summary(glht(IIAX_Fig3_gr2_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_gr2_2pib_lm <- lmer(twopib ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_2pib_emm <- data.frame(emmeans(IIAX_Fig3_gr2_2pib_lm, specs = "ExpCond"))
if ((zz <- anova(IIAX_Fig3_gr2_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_gr2_ton_lm <- lmer(ton ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum),data = IIAX_cdatp)
IIAX_Fig3_gr2_ton_emm <- data.frame(emmeans(IIAX_Fig3_gr2_ton_lm, specs = "ExpCond"))
if ((zzz <- anova(IIAX_Fig3_gr2_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_gr2_B_hoc <- summary(glht(IIAX_Fig3_gr2_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

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
if ((aa <- anova(I_Fig3_fvf_B_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_fvf_B_hoc <- summary(glht(I_Fig3_fvf_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_Ae_emm <- data.frame(emmeans(I_Fig3_fvf_Ae_lm, specs = "ExpCond"))
if ((aa.1 <- anova(I_Fig3_fvf_Ae_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_fvf_Ae_hoc <- summary(glht(I_Fig3_fvf_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_Av_emm <- data.frame(emmeans(I_Fig3_fvf_Av_lm, specs = "ExpCond"))
if ((aa.2 <- anova(I_Fig3_fvf_Av_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_fvf_Av_hoc <- summary(glht(I_Fig3_fvf_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_2pib_emm <- data.frame(emmeans(I_Fig3_fvf_2pib_lm, specs = "ExpCond"))
if ((ab <- anova(I_Fig3_fvf_2pib_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_fvf_B_hoc <- summary(glht(I_Fig3_fvf_twopib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = I_ffdatp)
I_Fig3_fvf_ton_emm <- data.frame(emmeans(I_Fig3_fvf_ton_lm, specs = "ExpCond"))
if ((ac <- anova(I_Fig3_fvf_ton_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig3_fvf_B_hoc <- summary(glht(I_Fig3_fvf_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# MHC I/IIA............................................................................
IIA_ffdatp <- fat_v_datp %>% filter(FiberType == "I/IIA")

IIA_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_B_emm <- data.frame(emmeans(IIA_Fig3_fvf_B_lm, specs = "ExpCond"))
if ((ad <- anova(IIA_Fig3_fvf_B_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_B_hoc <- summary(glht(IIA_Fig3_fvf_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_Ae_emm <- data.frame(emmeans(IIA_Fig3_fvf_Ae_lm, specs = "ExpCond"))
if ((ad.1 <- anova(IIA_Fig3_fvf_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_Ae_hoc <- summary(glht(IIA_Fig3_fvf_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_Av_emm <- data.frame(emmeans(IIA_Fig3_fvf_Av_lm, specs = "ExpCond"))
if ((ad.2 <- anova(IIA_Fig3_fvf_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_Av_hoc <- summary(glht(IIA_Fig3_fvf_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_2pib_emm <- data.frame(emmeans(IIA_Fig3_fvf_2pib_lm, specs = "ExpCond"))
if ((ae <- anova(IIA_Fig3_fvf_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_B_hoc <- summary(glht(IIA_Fig3_fvf_twopib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_ton_emm <- data.frame(emmeans(IIA_Fig3_fvf_ton_lm, specs = "ExpCond"))
if ((af <- anova(IIA_Fig3_fvf_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_B_hoc <- summary(glht(IIA_Fig3_fvf_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# MHC IIA ...............................................................................

IIA_ffdatp <- fat_v_datp %>% filter(FiberType == "IIA")

IIA_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = IIA_ffdatp)
IIA_Fig3_fvf_B_emm <- data.frame(emmeans(IIA_Fig3_fvf_B_lm, specs = "ExpCond"))
if ((ag <- anova(IIA_Fig3_fvf_B_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_B_hoc <- summary(glht(IIA_Fig3_fvf_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_Ae_emm <- data.frame(emmeans(IIA_Fig3_fvf_Ae_lm, specs = "ExpCond"))
if ((ag.1 <- anova(IIA_Fig3_fvf_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_Ae_hoc <- summary(glht(IIA_Fig3_fvf_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_Av_emm <- data.frame(emmeans(IIA_Fig3_fvf_Av_lm, specs = "ExpCond"))
if ((ag.2 <- anova(IIA_Fig3_fvf_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_Av_hoc <- summary(glht(IIA_Fig3_fvf_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_2pib_emm <- data.frame(emmeans(IIA_Fig3_fvf_2pib_lm, specs = "ExpCond"))
if ((ah <- anova(IIA_Fig3_fvf_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_B_hoc <- summary(glht(IIA_Fig3_fvf_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = IIA_ffdatp)
IIA_Fig3_fvf_ton_emm <- data.frame(emmeans(IIA_Fig3_fvf_ton_lm, specs = "ExpCond"))
if ((ai <- anova(IIA_Fig3_fvf_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig3_fvf_B_hoc <- summary(glht(IIA_Fig3_fvf_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

# MHC IIAX .......................................................................

IIAX_ffdatp <- fat_v_datp %>% filter(FiberType == "IIA/IIX")

IIAX_Fig3_fvf_B_lm <- lmer(BkNm2 ~ ExpCond + (1 | SubjectNum), data = IIAX_ffdatp)
IIAX_Fig3_fvf_B_emm <- data.frame(emmeans(IIAX_Fig3_fvf_B_lm, specs = "ExpCond"))
if ((aj <- anova(IIAX_Fig3_fvf_B_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_fvf_B_hoc <- summary(glht(IIAX_Fig3_fvf_B_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_fvf_Ae_lm <- lmer(Aelastic ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_Ae_emm <- data.frame(emmeans(IIAX_Fig3_fvf_Ae_lm, specs = "ExpCond"))
if ((aj.1 <- anova(IIAX_Fig3_fvf_Ae_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_fvf_Ae_hoc <- summary(glht(IIAX_Fig3_fvf_Ae_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_fvf_Av_lm <- lmer(Aviscous ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_Av_emm <- data.frame(emmeans(IIAX_Fig3_fvf_Av_lm, specs = "ExpCond"))
if ((aj.2 <- anova(IIAX_Fig3_fvf_Av_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_fvf_Av_hoc <- summary(glht(IIAX_Fig3_fvf_Av_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


IIAX_Fig3_fvf_2pib_lm <- lmer(twopib ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_2pib_emm <- data.frame(emmeans(IIAX_Fig3_fvf_2pib_lm, specs = "ExpCond"))
if ((ak <- anova(IIAX_Fig3_fvf_2pib_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_fvf_B_hoc <- summary(glht(IIAX_Fig3_fvf_2pib_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIAX_Fig3_fvf_ton_lm <- lmer(ton ~ ExpCond + (1 | SubjectNum),data = IIAX_ffdatp)
IIAX_Fig3_fvf_ton_emm <- data.frame(emmeans(IIAX_Fig3_fvf_ton_lm, specs = "ExpCond"))
if ((al <- anova(IIAX_Fig3_fvf_ton_lm)$`Pr(>F)`) <0.05) {
  
  IIAX_Fig3_fvf_B_hoc <- summary(glht(IIAX_Fig3_fvf_ton_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}


### Figure 4----------------------------------------------------------------------------

I_Fig4 <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 1) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

I_Fig4_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I_Fig4)
I_Fig4_Po_emm <- data.frame(emmeans(I_Fig4_Po_lm, specs = "ExpCond"))  
if ((am <- anova(I_Fig4_Po_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig4_Po_hoc <- summary(glht(I_Fig4_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

I_Fig4_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = I_Fig4)
I_Fig4_Force_emm <- data.frame(emmeans(I_Fig4_Force_lm, specs = "ExpCond"))  
if ((an <- anova(I_Fig4_Force_lm)$`Pr(>F)`) <0.05) {
  
  I_Fig4_Force_hoc <- summary(glht(I_Fig4_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig4 <- read_excel("Aurora_Masters_CS.xlsx") %>%
  filter(Fibertypenum == 2) %>% 
  filter(ExpCondnum %in% c(10,4)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum) %>% 
  mutate(CSA = (pi*(Topwidthum/2)*(Sidewidthum/2))/(1000*1000)) %>%
  mutate(Force = CSA*PoControl25C) 

IIA_Fig4_Po_lm <- lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA_Fig4)
IIA_Fig4_Po_emm <- data.frame(emmeans(IIA_Fig4_Po_lm, specs = "ExpCond"))  
if ((ao <- anova(IIA_Fig4_Po_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig4_Po_hoc <- summary(glht(IIA_Fig4_Po_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}

IIA_Fig4_Force_lm <- lmer(Force ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = IIA_Fig4)
IIA_Fig4_Force_emm <- data.frame(emmeans(IIA_Fig4_Force_lm, specs = "ExpCond"))  
if ((ap <- anova(IIA_Fig4_Force_lm)$`Pr(>F)`) <0.05) {
  
  IIA_Fig4_Force_hoc <- summary(glht(IIA_Fig4_Force_lm, linfct = mcp(ExpCond = "Tukey")))
  
} else{
  NA
}
