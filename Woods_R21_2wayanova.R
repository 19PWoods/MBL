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
  filter(FiberType == "I") 
my_data_IIA <- my_data %>% 
  filter(FiberType == "IIA")
my_data_IIAX <- my_data %>% 
  filter(FiberType == "IIAX") 