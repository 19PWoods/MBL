library(tidyverse)
library(readxl)
library(patchwork)
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
  geom_line(data = I_data_on, aes(y = mdl), linetype = "dotted") +
  scale_shape_manual(values = c(1,6,15),
                     labels = c("Young Normal",
                                  "Young High",
                                  "Older Normal")) 
)

(I.axw.fits.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
    geom_line(data = I_data_yn, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_yh, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_on, aes(y = mdl), linetype = "dotted")
)

(I.axwxs.all.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
    geom_point(aes(shape = AgexWeightxSex)) +
    geom_line(data = I_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_ynf, aes(y = mdl), linetype = "solid", alpha = 0.5) +
    geom_line(data = I_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_yhf, aes(y = mdl), linetype = "longdash", alpha = 0.5) +
    geom_line(data = I_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = I_data_onf, aes(y = mdl), linetype = "dotted", alpha = 0.5) +
    scale_shape_manual(values = c(16,1,17,6,15,0),
                         labels = c("Young Normal Males",
                                    "Young Normal Females",
                                    "Young High Males",
                                    "Young High Females",
                                    "Older Normal Males",
                                    "Older Normal Females")) 
  )

(I.axwxs.fits.gg <- ggplot(data = I_data, aes(x = CSA, y = Force)) +
    geom_line(data = I_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = I_data_ynf, aes(y = mdl), linetype = "solid", alpha = 0.5) +
    geom_line(data = I_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = I_data_yhf, aes(y = mdl), linetype = "longdash", alpha = 0.5) +
    geom_line(data = I_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = I_data_onf, aes(y = mdl), linetype = "dotted", alpha = 0.5) 
)

(I_gg <- (I.axw.all.gg | I.axw.fits.gg) / (I.axwxs.all.gg | I.axwxs.fits.gg) +
  plot_annotation(title = "MHC I")
)

ggsave("R21_ForcevCSA_MHCI.tiff",
       I_gg , width = 12, height = 7, units = "in",  dpi = 300)

### MHC IIA-------------------------------------------------------------

IIA_data <- my_data %>% filter(FiberType == "IIA")

IIA_data_yn <- IIA_data %>% filter(AgexWeight == 0)
IIA_data_yh <- IIA_data %>% filter(AgexWeight ==1)
IIA_data_on <- IIA_data %>% filter(AgexWeight ==2)

YN.IIA.lm <-  lm(Force ~ CSA, data = IIA_data_yn)
YH.IIA.lm <- lm(Force ~ CSA, data = IIA_data_yh)
ON.IIA.lm <- lm(Force ~ CSA, data = IIA_data_on)

IIA_data_yn$mdl <- predict(YN.IIA.lm)
IIA_data_yh$mdl <- predict(YH.IIA.lm)
IIA_data_on$mdl <- predict(ON.IIA.lm)

IIA_data_ynm <- IIA_data %>% filter(AgexWeightxSex == 0)
IIA_data_ynf <- IIA_data %>% filter(AgexWeightxSex == 1)
IIA_data_yhm <- IIA_data %>% filter(AgexWeightxSex ==2)
IIA_data_yhf <- IIA_data %>% filter(AgexWeightxSex ==3)
IIA_data_onm <- IIA_data %>% filter(AgexWeightxSex ==4)
IIA_data_onf <- IIA_data %>% filter(AgexWeightxSex ==5)

YNM.IIA.lm <-  lm(Force ~ CSA, data = IIA_data_ynm)
YNF.IIA.lm <-  lm(Force ~ CSA, data = IIA_data_ynf)
YHM.IIA.lm <- lm(Force ~ CSA, data = IIA_data_yhm)
YHF.IIA.lm <- lm(Force ~ CSA, data = IIA_data_yhf)
ONM.IIA.lm <- lm(Force ~ CSA, data = IIA_data_onm)
ONF.IIA.lm <- lm(Force ~ CSA, data = IIA_data_onf)

IIA_data_ynm$mdl <- predict(YNM.IIA.lm)
IIA_data_ynf$mdl <-predict(YNF.IIA.lm)
IIA_data_yhm$mdl <- predict(YHM.IIA.lm)
IIA_data_yhf$mdl <-predict(YHF.IIA.lm)
IIA_data_onm$mdl <- predict(ONM.IIA.lm)
IIA_data_onf$mdl <- predict(ONF.IIA.lm)

(IIA.axw.all.gg <- ggplot(data = IIA_data, aes(x = CSA, y = Force)) +
    geom_point(aes(shape = AgexWeight)) +
    geom_line(data = IIA_data_yn, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIA_data_yh, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIA_data_on, aes(y = mdl), linetype = "dotted") +
    scale_shape_manual(values = c(1,6,15),
                       labels = c("Young Normal",
                                  "Young High",
                                  "Older Normal")) 
)

(IIA.axw.fits.gg <- ggplot(data = IIA_data, aes(x = CSA, y = Force)) +
    geom_line(data = IIA_data_yn, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIA_data_yh, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIA_data_on, aes(y = mdl), linetype = "dotted")
)

(IIA.axwxs.all.gg <- ggplot(data = IIA_data, aes(x = CSA, y = Force)) +
    geom_point(aes(shape = AgexWeightxSex)) +
    geom_line(data = IIA_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIA_data_ynf, aes(y = mdl), linetype = "solid", alpha = 0.5) +
    geom_line(data = IIA_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIA_data_yhf, aes(y = mdl), linetype = "longdash", alpha = 0.5) +
    geom_line(data = IIA_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = IIA_data_onf, aes(y = mdl), linetype = "dotted", alpha = 0.5) +
    scale_shape_manual(values = c(16,1,17,6,15,0),
                       labels = c("Young Normal Males",
                                  "Young Normal Females",
                                  "Young High Males",
                                  "Young High Females",
                                  "Older Normal Males",
                                  "Older Normal Females")) 
)

(IIA.axwxs.fits.gg <- ggplot(data = IIA_data, aes(x = CSA, y = Force)) +
    geom_line(data = IIA_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIA_data_ynf, aes(y = mdl), linetype = "solid", alpha = 0.5) +
    geom_line(data = IIA_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIA_data_yhf, aes(y = mdl), linetype = "longdash", alpha = 0.5) +
    geom_line(data = IIA_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = IIA_data_onf, aes(y = mdl), linetype = "dotted", alpha = 0.5) 
)

(IIA_gg <- (IIA.axw.all.gg | IIA.axw.fits.gg) / (IIA.axwxs.all.gg | IIA.axwxs.fits.gg) +
    plot_annotation(title = "MHC IIA")
)

ggsave("R21_ForcevCSA_MHCIIA.tiff",
       IIA_gg , width = 12, height = 7, units = "in",  dpi = 300)

### MHC IIAX -----------------------------------------------------------

IIAX_data <- my_data %>% filter(FiberType == "IIAX")

IIAX_data_yn <- IIAX_data %>% filter(AgexWeight == 0)
IIAX_data_yh <- IIAX_data %>% filter(AgexWeight ==1)
IIAX_data_on <- IIAX_data %>% filter(AgexWeight ==2)

YN.IIAX.lm <-  lm(Force ~ CSA, data = IIAX_data_yn)
YH.IIAX.lm <- lm(Force ~ CSA, data = IIAX_data_yh)
ON.IIAX.lm <- lm(Force ~ CSA, data = IIAX_data_on)

IIAX_data_yn$mdl <- predict(YN.IIAX.lm)
IIAX_data_yh$mdl <- predict(YH.IIAX.lm)
IIAX_data_on$mdl <- predict(ON.IIAX.lm)

IIAX_data_ynm <- IIAX_data %>% filter(AgexWeightxSex == 0)
IIAX_data_ynf <- IIAX_data %>% filter(AgexWeightxSex == 1)
IIAX_data_yhm <- IIAX_data %>% filter(AgexWeightxSex ==2)
IIAX_data_yhf <- IIAX_data %>% filter(AgexWeightxSex ==3)
IIAX_data_onm <- IIAX_data %>% filter(AgexWeightxSex ==4)
IIAX_data_onf <- IIAX_data %>% filter(AgexWeightxSex ==5)

YNM.IIAX.lm <-  lm(Force ~ CSA, data = IIAX_data_ynm)
YNF.IIAX.lm <-  lm(Force ~ CSA, data = IIAX_data_ynf)
YHM.IIAX.lm <- lm(Force ~ CSA, data = IIAX_data_yhm)
YHF.IIAX.lm <- lm(Force ~ CSA, data = IIAX_data_yhf)
ONM.IIAX.lm <- lm(Force ~ CSA, data = IIAX_data_onm)
ONF.IIAX.lm <- lm(Force ~ CSA, data = IIAX_data_onf)

IIAX_data_ynm$mdl <- predict(YNM.IIAX.lm)
IIAX_data_ynf$mdl <-predict(YNF.IIAX.lm)
IIAX_data_yhm$mdl <- predict(YHM.IIAX.lm)
IIAX_data_yhf$mdl <-predict(YHF.IIAX.lm)
IIAX_data_onm$mdl <- predict(ONM.IIAX.lm)
IIAX_data_onf$mdl <- predict(ONF.IIAX.lm)

(IIAX.axw.all.gg <- ggplot(data = IIAX_data, aes(x = CSA, y = Force)) +
    geom_point(aes(shape = AgexWeight)) +
    geom_line(data = IIAX_data_yn, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIAX_data_yh, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIAX_data_on, aes(y = mdl), linetype = "dotted") +
    scale_shape_manual(values = c(1,6,15),
                       labels = c("Young Normal",
                                  "Young High",
                                  "Older Normal")) 
)

(IIAX.axw.fits.gg <- ggplot(data = IIAX_data, aes(x = CSA, y = Force)) +
    geom_line(data = IIAX_data_yn, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIAX_data_yh, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIAX_data_on, aes(y = mdl), linetype = "dotted")
)

(IIAX.axwxs.all.gg <- ggplot(data = IIAX_data, aes(x = CSA, y = Force)) +
    geom_point(aes(shape = AgexWeightxSex)) +
    geom_line(data = IIAX_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIAX_data_ynf, aes(y = mdl), linetype = "solid", alpha = 0.5) +
    geom_line(data = IIAX_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIAX_data_yhf, aes(y = mdl), linetype = "longdash", alpha = 0.5) +
    geom_line(data = IIAX_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = IIAX_data_onf, aes(y = mdl), linetype = "dotted", alpha = 0.5) +
    scale_shape_manual(values = c(16,1,17,6,15,0),
                       labels = c("Young Normal Males",
                                  "Young Normal Females",
                                  "Young High Males",
                                  "Young High Females",
                                  "Older Normal Males",
                                  "Older Normal Females")) 
)

(IIAX.axwxs.fits.gg <- ggplot(data = IIAX_data, aes(x = CSA, y = Force)) +
    geom_line(data = IIAX_data_ynm, aes(y = mdl), linetype = "solid") +
    geom_line(data = IIAX_data_ynf, aes(y = mdl), linetype = "solid", alpha = 0.5) +
    geom_line(data = IIAX_data_yhm, aes(y = mdl), linetype = "longdash") +
    geom_line(data = IIAX_data_yhf, aes(y = mdl), linetype = "longdash", alpha = 0.5) +
    geom_line(data = IIAX_data_onm, aes(y = mdl), linetype = "dotted") +
    geom_line(data = IIAX_data_onf, aes(y = mdl), linetype = "dotted", alpha = 0.5) 
)

(IIAX_gg <- (IIAX.axw.all.gg | IIAX.axw.fits.gg) / (IIAX.axwxs.all.gg | IIAX.axwxs.fits.gg) +
    plot_annotation(title = "MHC IIAX")
)

ggsave("R21_ForcevCSA_MHCIIAX.tiff",
       IIAX_gg , width = 12, height = 7, units = "in",  dpi = 300)
