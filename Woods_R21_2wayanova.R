library(tidyverse)
library(readxl)


# setwd("C:/Users/Phil/Dropbox/MBL/Tension + AaBbCc/R21-MAT")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/MBL/Tension + AaBbCc/R21-MAT")

my_data <- read_excel("R21-Tension+AkBbCc__PW_R_4-25-23.xlsx",
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

I_Ten_aov <- aov(Tension ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_Ten_aov)

I_Force_aov <- aov(Force ~Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_Force_aov)

I_CSA_aov <- aov(CSA ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_CSA_aov)

I_A_aov <- aov(A ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_A_aov)

I_k_aov <- aov(k ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_k_aov)

I_B_aov <- aov(B ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_B_aov)

I_C_aov <- aov(C ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_C_aov)

I_lb_aov <- aov(lb ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_lb_aov)

I_2pib_aov <- aov(twopib ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_2pib_aov)

I_lc_aov <- aov(lc ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_lc_aov)

I_ton_aov <- aov(ton ~ Sex*Wt.Status + Error(SubjNum), data = my_data_I)
summary(I_ton_aov)

### MHC IIA --------------------------------------------------------------------------------

IIA_Ten_aov <- aov(Tension ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_Ten_aov)

IIA_Force_aov <- aov(Force ~Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_Force_aov)

IIA_CSA_aov <- aov(CSA ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_CSA_aov)

IIA_A_aov <- aov(A ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_A_aov)

IIA_k_aov <- aov(k ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_k_aov)

IIA_B_aov <- aov(B ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_B_aov)

IIA_C_aov <- aov(C ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_C_aov)

IIA_lb_aov <- aov(lb ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_lb_aov)

IIA_2pib_aov <- aov(twopib ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_2pib_aov)

IIA_lc_aov <- aov(lc ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_lc_aov)

IIA_ton_aov <- aov(ton ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIA)
summary(IIA_ton_aov)


### MHCA IIAX ---------------------------------------------------------------------------------

IIAX_Ten_aov <- aov(Tension ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_Ten_aov)

IIAX_Force_aov <- aov(Force ~Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_Force_aov)

IIAX_CSA_aov <- aov(CSA ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_CSA_aov)

IIAX_A_aov <- aov(A ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_A_aov)

IIAX_k_aov <- aov(k ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_k_aov)

IIAX_B_aov <- aov(B ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_B_aov)

IIAX_C_aov <- aov(C ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_C_aov)

IIAX_lb_aov <- aov(lb ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_lb_aov)

IIAX_2pib_aov <- aov(twopib ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_2pib_aov)

IIAX_lc_aov <- aov(lc ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_lc_aov)

IIAX_ton_aov <- aov(ton ~ Sex*Wt.Status + Error(SubjNum), data = my_data_IIAX)
summary(IIAX_ton_aov)
