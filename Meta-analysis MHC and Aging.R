#########################################################################
# 
# AUTHOR    Amanda Paluch
# TITLE     meta-regression MHC
# LOCATION  C:\Users\apaluch\OneDrive - University of Massachusetts\Manuscripts\MHC and Aging Meta-analysis
# DATE      6/7/23
# NOTES:     
#     useful instructions: 

#########################################################################
#############
# clears environment;
rm(list=ls())


# SET WORKING DIRECTORY
path <- "C:/Users/pcw00/Dropbox/University of Massachusetts Amherst/MBL/MHC Composition and Aging"
setwd(path) 


packages <- c("Hmisc","rms","foreign","haven","ggplot2","tidyverse","readxl", "metagen","meta","metasens","metafor","rmeta","mvmeta")
install.packages(packages) #update all packages
lapply(packages, require, character.only = TRUE) #call all packages


MHC <- read_excel("MHC meta-regression data set.xlsx",
                  sheet="Age")


MHC_1vs2 <- read_excel("MHC meta-regression data set.xlsx", 
                       sheet="I vs II")

PA_level <- read_excel("MHC meta-regression data set.2.xlsx",
                       sheet = "PA")


####Fiber Distribution;
#MHC_I
MHC1_reg <- metacont(Old_n, MHC_1_Fiber_mean_Old, MHC_1_Fiber_sd_Old, 
                  Young_n, MHC_1_Fiber_mean_Young, MHC_1_Fiber_sd_Young, 
                    studlab = paste(Study), fixed=FALSE, random=TRUE,  
                  data=MHC, sm="MD")
MHC1_reg
forest(MHC1_reg, allstudies = FALSE, digits = 1, sortvar = TE)
metareg(MHC1_reg, CSA_method)

#MHC_II
MHC2_reg <- metacont(Old_n, MHC_2_Fiber_mean_Old, MHC_2_Fiber_sd_Old, 
                     Young_n, MHC_2_Fiber_mean_Young, MHC_2_Fiber_sd_Young, 
                    studlab = paste(Study), fixed=FALSE, random=TRUE,  
                    data=MHC, sm="MD")
MHC2_reg
forest(MHC2_reg, allstudies = FALSE, digits = 1, sortvar = TE)
metareg(MHC2_reg, CSA_method)

#MHC_IIA
MHC2A_reg <- metacont(Old_n, MHC_2A_Fiber_mean_Old, MHC_2A_Fiber_sd_Old, 
                      Young_n, MHC_2A_Fiber_mean_Young, MHC_2A_Fiber_sd_Young, 
                     studlab = paste(Study), fixed=FALSE, random=TRUE,  
                     data=MHC, sm="MD")
MHC2A_reg
forest(MHC2A_reg, allstudies = FALSE, digits = 1, sortvar = TE)
metareg(MHC2A_reg, CSA_method)





####CSA;
#MHC_I
MHC1_CSA <- metacont(Old_n, MHC_1_CSA_mean_Old, MHC_1_CSA_sd_Old, 
                     Young_n, MHC_1_CSA_mean_Young, MHC_1_CSA_sd_Young, 
                     studlab = paste(Study), subgroup=CSA_method, fixed=FALSE, random=TRUE,  
                     data=MHC, sm="MD")
MHC1_CSA
forest(MHC1_CSA, allstudies = FALSE, digits = 0, sortvar = TE)
metareg(MHC1_CSA, CSA_method)

#MHC_II
MHC2_CSA <- metacont(Old_n, MHC_2_CSA_mean_Old, subgroup=CSA_method,
                     Young_n, MHC_2_CSA_mean_Young, MHC_2_CSA_sd_Young, 
                      MHC_2_CSA_sd_Old, studlab = paste(Study), fixed=FALSE, random=TRUE,  
                     data=MHC, sm="MD")
MHC2_CSA
forest(MHC2_CSA, allstudies = FALSE, digits = 0, sortvar = TE , layout="JAMA")
metareg(MHC2_CSA, CSA_method)

#MHC_IIA
MHC2A_CSA <- metacont(Old_n, MHC_2A_CSA_mean_Old, MHC_2A_CSA_sd_Old,
                      Young_n, MHC_2A_CSA_mean_Young, MHC_2A_CSA_sd_Young, 
                       studlab = paste(Study), fixed=FALSE, random=TRUE,  
                      data=MHC, sm="MD")
MHC2A_CSA
forest(MHC2A_CSA, allstudies = FALSE, digits = 0, sortvar = TE, layout="JAMA")

metareg(MHC2A_CSA, CSA_method)



dev.off()


####Rel;
#MHC_I
MHC1_Rel <- metacont( Old_n, MHC_1_Rel_mean_Old, MHC_1_Rel_sd_Old, 
                      Young_n, MHC_1_Rel_mean_Young, MHC_1_Rel_sd_Young,
                      studlab = paste(Study), fixed=FALSE, random=TRUE,  
                     data=MHC, subgroup=CSA_method, sm="MD")
MHC1_Rel
forest(MHC1_Rel, allstudies = FALSE, digits = 0, sortvar = TE)
metareg(MHC1_Rel, CSA_method)

#MHC_II
MHC2_Rel <- metacont(Old_n, MHC_2_Rel_mean_Old, MHC_2_Rel_sd_Old, 
                    Young_n, MHC_2_Rel_mean_Young, MHC_2_Rel_sd_Young, 
                     studlab = paste(Study), fixed=FALSE, random=TRUE,  
                     data=MHC, sm="MD")
MHC2_Rel
forest(MHC2_Rel, allstudies = FALSE, digits = 0, sortvar = TE)
metareg(MHC2_Rel, CSA_method)

#MHC_IIA
MHC2A_Rel <- metacont(Old_n, MHC_2A_Rel_mean_Old, MHC_2A_Rel_sd_Old,
                       Young_n, MHC_2A_Rel_mean_Young, MHC_2A_Rel_sd_Young, 
                       studlab = paste(Study), fixed=FALSE, random=TRUE,  
                      data=MHC, sm="MD")
MHC2A_Rel
forest(MHC2A_Rel, allstudies = FALSE, digits = 0, sortvar = TE)

metareg(MHC2A_Rel, ~CSA_method)






####I vs II;

MHC1v2_CSA <- metacont(Old_n, MHC_CSA_mean_Old, MHC_CSA_sd_Old, 
                     Young_n, MHC_CSA_mean_Young, MHC_CSA_sd_Young, 
                     studlab = paste(Study), subgroup=MHC, fixed=FALSE, random=TRUE,  
                     data=MHC_1vs2, sm="MD")
MHC1v2_CSA

metareg(MHC1v2_CSA, CSA_method)

forest(MHC1v2_CSA,  digits=0, digits.mean=0, digits.sd=0, digits.addcols = 0,
       just="center",
       print.tau2 = FALSE,
       leftcols = c("studlab", "n.e", "mean.e", "sd.e","mean.c", "sd.c"), 
       leftlabs=c("Study",   "n", "  ",  "Older", "Younger"),
       label.e = "Older", label.c="Younger",
       smlab = "Older - Younger",
       colgap.left=unit(0.5, "cm"),
       rightlabs = c("Mean Diff.", "95% CI"),
       sortvar = TE, spacing=1.25,
       overall=FALSE, overall.hetstat=FALSE, test.subgroup = TRUE, col.subgroup="black" )


# #MHC_IIA (subgroup) * receiving error* 
# MHC2A_Rel_pa <- metacont(Old_n, MHC_2A_Rel_mean_Old, MHC_2A_Rel_sd_Old,
#                       Young_n, MHC_2A_Rel_mean_Young, MHC_2A_Rel_sd_Young, 
#                       studlab = paste(Study),subgroup = PA_level, fixed=FALSE, random=TRUE,  
#                       data=PA_level, sm="MD")

MHC2A_Rel
forest(MHC2A_Rel, allstudies = FALSE, digits = 0, sortvar = TE)

metareg(MHC2A_Rel, ~CSA_method)


