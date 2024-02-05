library(tidyverse)
library(readxl)
library(car)
library(multcomp)
library(lme4)

### Two-Way Anova for ExpCond:Sex Interaction ---------------
# my_data = read_excel(file.choose(), sheet = 'raw')%>%
#   filter(Fibertypenum %in% c(1,2,4,5)) %>%
#   filter(ExpCondnum %in% c(1:3)) %>%
#   group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum)

my_data = read_excel(file.choose(), sheet = 'raw')%>%
  filter(Fibertypenum %in% c(1,2)) %>%
  filter(ExpCondnum %in% c(4,10)) %>%
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum)

I = my_data %>% filter(FiberType == "I")
I.IIA = my_data %>% filter(FiberType == "I/IIA") 
IIA = my_data %>% filter(FiberType == "IIA") 
IIAX = my_data %>% filter(FiberType == "IIA/IIX") 

# interaction = function(x){
#   tension = summary(aov(PoControl25C ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   force = summary(aov(Force ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   b = summary(aov(B.kn ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   a.elastic = summary(aov(Aelastic ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   a.viscous = summary(aov(Aviscous ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   twopib = summary(aov(twopib ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   ton = summary(aov(ton ~ Sex*ExpCond + Error(SubjectNum), data =x))
#   
#   
#  
# }


tension = summary(aov(PoControl25C ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))
force = summary(aov(Force ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))
b = summary(aov(B.kn ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))
a.elastic = summary(aov(Aelastic ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))
a.viscous = summary(aov(Aviscous ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))
twopib = summary(aov(twopib ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))
ton = summary(aov(ton ~ Sex*ExpCond + Error(SubjectNum), data =IIAX))

### Ancova Testing for Slope Differences ------------------------
my_data = read_excel(file.choose(), sheet = 'raw') %>% 
  select(c(Filename, FiberType, Fibertypenum, CSA, ExpCond,ExpCondnum, Force)) %>% 
  filter(ExpCondnum %in% c(1:3, 4, 10)) %>% 
  filter(Fibertypenum %in% c(1:2))

study1_I = my_data %>% filter(FiberType == "I") %>% filter(ExpCondnum %in% c(1:3)) 
study1_IIA = my_data %>% filter(FiberType == "IIA") %>% filter(ExpCondnum %in% c(1:3)) 
study2_I = my_data %>% filter(FiberType == "I") %>% filter(ExpCondnum %in% c(4,10)) 
study2_IIA = my_data %>% filter(FiberType == "IIA") %>% filter(ExpCondnum %in% c(4,10)) 


study1_I$ExpCond = relevel(factor(study1_I$ExpCond), "Fatigue")
study1_IIA$ExpCond = relevel(factor(study1_IIA$ExpCond), "Fatigue")


ancova1 = lm(Force ~ CSA + ExpCond, data = study1_I)
ancova2 = lm(Force ~ CSA + ExpCond, data = study1_IIA)
ancova3 = lm(Force ~ CSA + ExpCond, data = study2_I)
ancova4 = lm(Force ~ CSA + ExpCond, data = study2_IIA)

### RE-run of MHC I/IIA tension and force for Marky Mark -----------------

my_data = read_excel(file.choose(), sheet = 'raw')%>%
  filter(Fibertypenum %in% c(4)) %>%
  filter(ExpCondnum %in% c(1:3)) %>%
  mutate(PhilForce = (PoControl25C * CSA) / (1000*1000)) %>% 
  group_by(SubjectNum, FiberType, Fibertypenum, ExpCondnum)


tension = lmer(PoControl25C ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = my_data)
summary(glht(tension, linfct = mcp(ExpCond = "Tukey")))

force = lmer(PhilForce ~ ExpCond + (1 + as.factor(ExpCond) | SubjectNum), data = my_data)
summary(glht(force, linfct = mcp(ExpCond = "Tukey")))
