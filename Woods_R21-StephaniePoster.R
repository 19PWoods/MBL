library(tidyverse)
library(readxl)
library(emmeans)
library(multcomp)
library(lmerTest)

# setwd("C:/Users/Phil/Dropbox/MBL/Tension + AaBbCc/R21-MAT")
setwd("C:/Users/pcw00/Dropbox/MBL/Tension + AaBbCc/R21-MAT")

my_data <- read_excel("Woods_StephaniePosterAnalysis_4-5-23.xlsx")

my_data_I <- my_data %>% filter(MHC == "I")
my_data_IIA <- my_data %>% filter(MHC == "IIA")
my_data_IIAX <- my_data %>% filter(MHC == "IIAX")

mhcI_mdl <- lmer(Percentage ~ AgexWeight + (1 | Subject), data = my_data_I)
