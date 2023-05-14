library(tidyverse)
library(readxl)
theme_set(theme_classic())

# setwd("C:/Users/Phil/Dropbox/MBL/Tension + AaBbCc/R21-MAT")
setwd("C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/MBL/Tension + AaBbCc/R21-MAT")

my_data <- read_excel("R21-Tension+AkBbCc__PW_R_4-25-23.xlsx",
                      sheet = "Final - Run only") %>% 
  mutate(AgexWeight = as.factor(AgexWeight)) %>% 
  mutate(AgexWeightxSex = as.factor(AgexWeightxSex))

## MHC I-------------------------------------
I_data <- my_data %>% filter(FiberType == "I")

I_data_yn <- I_data %>% filter(AgexWeight == 0)
I_data_yh <- I_data %>% filter(AgexWeight ==1)
I_data_on <- I_data %>% filter(AgexWeight ==2)

YN.I.lm <-  lm(Force ~ CSA, data = I_data_yn)
YH.I.lm <- lm(Force ~ CSA, data = I_data_yh)
ON.I.lm <- lm(Force ~ CSA, data = I_data_on)

I_data_yn$mdl <- predict(YN.I.lm)
I_data_yh$mdl <- predict(YH.I.lm)
I_data_on$mdl <- predict(ON.I.lm)

I_data_ynm <- I_data %>% filter(AgexWeightxSex == 0)
I_data_ynf <- I_data %>% filter(AgexWeightxSex == 1)
I_data_yhm <- I_data %>% filter(AgexWeightxSex ==2)
I_data_yhf <- I_data %>% filter(AgexWeightxSex ==3)
I_data_onm <- I_data %>% filter(AgexWeightxSex ==4)
I_data_onf <- I_data %>% filter(AgexWeightxSex ==5)

YNM.I.lm <-  lm(Force ~ CSA, data = I_data_ynm)
YNF.I.lm <-  lm(Force ~ CSA, data = I_data_ynf)
YHM.I.lm <- lm(Force ~ CSA, data = I_data_yhm)
YHF.I.lm <- lm(Force ~ CSA, data = I_data_yhf)
ONM.I.lm <- lm(Force ~ CSA, data = I_data_onm)
ONF.I.lm <- lm(Force ~ CSA, data = I_data_onf)

I_data_ynm$mdl <- predict(YNM.I.lm)
I_data_ynf$mdl <-predict(YNF.I.lm)
I_data_yhm$mdl <- predict(YHM.I.lm)
I_data_yhf$mdl <-predict(YHF.I.lm)
I_data_onm$mdl <- predict(ONM.I.lm)
I_data_onf$mdl <- predict(ONF.I.lm)

(I.axw.all.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
  geom_point(aes(shape = AgexWeight)) +
  geom_line(data = I_data_yn, aes(y = mdl), linetype = "solid") +
  geom_line(data = I_data_yh, aes(y = mdl), linetype = "longdash") +
  geom_line(data = I_data_on, aes(y = mdl), linetype = "dotted")
)
(I.axw.fits.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
    geom_line(data = I_data_yn, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_yh, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_on, aes(y = mdl), linetype = "dotted")
)

(I.axwxs.all.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
    geom_point(aes(shape = AgexWeightxSex)) +
    geom_line(data = I_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_ynf, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_yhf, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = I_data_onf, aes(y = mdl), linetype = "dotted") 
  )

(I.axwxs.fits.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
    geom_line(data = I_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_ynf, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_yhf, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = I_data_onf, aes(y = mdl), linetype = "dotted") 
)
