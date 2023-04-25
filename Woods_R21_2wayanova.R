library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)

# setwd("C:/Users/Phil/Dropbox/MBL/Tension + AaBbCc/R21-MAT")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/MBL/Tension + AaBbCc/R21-MAT")

my_data <- read_excel("R21-Tension+AkBbCc__PW_R_4-3-23.xlsx",
                      sheet = "Final - Run only") %>% 
  mutate(AgexWeight = as.factor(AgexWeight)) %>% 
  mutate(AgexWeightxSex = as.factor(AgexWeightxSex))


my_data_I <- my_data %>% 
  filter(FiberType == "I") %>% 
  filter(AgeGrp == "Young")
my_data_IIA <- my_data %>% 
  filter(FiberType == "IIA")%>% 
  filter(AgeGrp == "Young")
my_data_IIAX <- my_data %>% 
  filter(FiberType == "IIAX") %>% 
  filter(AgeGrp == "Young")

### MHC I Analysis--------------------------------------------------------------------------------

I_Ten_aov <- aov(Tension ~ Sex*Wt.Status, data = my_data_I)
summary(I_Ten_aov)
I_Ten2_aov <- aov(Tension ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_Ten2_aov)
I_Ten3_aov <- aov(Tension ~ Wt.Status*Sex, data = my_data_I)
summary(I_Ten3_aov)

I_Force_aov <- aov(Force ~Sex*Wt.Status, data = my_data_I)
summary(I_Force_aov)
I_Force2_aov <- aov(Force ~Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_Force2_aov)

I_CSA_aov <- aov(CSA ~ Sex*Wt.Status, data = my_data_I)
summary(I_CSA_aov)
I_CSA2_aov <- aov(CSA ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_CSA2_aov)

I_A_aov <- aov(A ~ Sex*Wt.Status, data = my_data_I)
summary(I_A_aov)
I_A2_aov <- aov(A ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_A2_aov)

I_k_aov <- aov(k ~ Sex*Wt.Status, data = my_data_I)
summary(I_k_aov)
I_k2_aov <- aov(k ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_k2_aov)

I_B_aov <- aov(B ~ Sex*Wt.Status, data = my_data_I)
summary(I_B_aov)
I_B2_aov <- aov(B ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_B2_aov)

I_C_aov <- aov(C ~ Sex*Wt.Status, data = my_data_I)
summary(I_C_aov)
I_C2_aov <- aov(C ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_C2_aov)

I_lb_aov <- aov(lb ~ Sex*Wt.Status, data = my_data_I)
summary(I_lb_aov)
I_lb2_aov <- aov(lb ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_lb2_aov)

I_2pib_aov <- aov(twopib ~ Sex*Wt.Status, data = my_data_I)
summary(I_2pib_aov)
I_2pib2_aov <- aov(twopib ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_2pib2_aov)

I_lc_aov <- aov(lc ~ Sex*Wt.Status, data = my_data_I)
summary(I_lc_aov)
I_lc2_aov <- aov(lc ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_lc2_aov)

I_ton_aov <- aov(ton ~ Sex*Wt.Status, data = my_data_I)
summary(I_ton_aov)
I_ton2_aov <- aov(ton ~ Sex*Wt.Status + (1|SubjNum), data = my_data_I)
summary(I_ton2_aov)


