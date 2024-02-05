library(tidyverse)
library(readxl)

my_data = read_excel(file.choose(), sheet = 'raw')%>%
  filter(Fibertypenum %in% c(1,2,4,5)) %>%
  filter(ExpCondnum %in% c(1:3)) %>%
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
