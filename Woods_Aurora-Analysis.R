library(tidyverse)
library(broom.mixed)
library(multcomp)
library(lmerTest)
library(readxl)
library(emmeans)


table_glht <- function(x) {
  pq <- summary(x)$test
  mtests <- cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
  error <- attr(pq$pvalues, "error")
  pname <- switch(x$alternativ, less = paste("Pr(<", ifelse(x$df ==0, "z", "t"), ")", sep = ""),
                  greater = paste("Pr(>", ifelse(x$df == 0, "z", "t"), ")", sep = ""),
                  two.sided = paste("Pr(>|",ifelse(x$df == 0, "z", "t"), "|)", sep = ""))
  colnames(mtests) <- c("Estimate", "Std. Error", ifelse(x$df ==0, "z value", "t value"), pname)
  return(mtests)
}


my_data = read_excel(file.choose(),
                      sheet = 'raw') %>%
  mutate(FiberType = factor(FiberType)) %>% 
  mutate(FiberTypeNum = factor(FiberTypeNum)) %>% 
  mutate(ExpCond = factor(ExpCond)) %>% 
  mutate(ExpCondNum = factor(ExpCondNum)) %>% 
  mutate(Grp = factor(Grp)) %>% 
  mutate(SubjectNum = factor(SubjectNum)) %>% 
  dplyr::select(`SubjectNum`, `FiberType`, `ExpCond`:`ExpCondNum`, `Grp`,
                `DynamicAmplitude`:`Aviscous`) %>% 
  group_by(SubjectNum, FiberType,  ExpCond)



### Old Method: Individually writting out each line ------------------------------------------------
# DF for specific tension analysis between Control, Fatigue & Fatigue + dATP
# Contains MHC I, IIA, I/IIA, and IIAX
df1 = my_data %>%
  filter(FiberTypeNum %in% c(1,2,4,5)) %>%
  filter(ExpCondNum %in% c(1:3)) %>%
  dplyr::select(c(`Filename`:`Po-Control`))

ST_mod_I = df1 %>% 
  filter(FiberTypeNum == 1) %>% 
  lmer(`Po-Control` ~ `ExpCond` + (1 + `ExpCond` | `SubjectNum`), data = .) 
ST_post_I = table_glht(summary(glht(ST_mod_I, linfct = mcp(ExpCond = "Tukey"))))
ST_mod_I  = tidy(ST_mod_I)

ST_mod_IIA = df1 %>% 
  filter(FiberTypeNum == 2) %>% 
  lmer(`Po-Control` ~ `ExpCond` + (1 + `ExpCond` | `SubjectNum`), data = .) 
ST_post_IIA = table_glht(summary(glht(ST_mod_IIA, linfct = mcp(ExpCond = "Tukey"))))
ST_mod_IIA  = tidy(ST_mod_IIA)


ST_mod_I.IA = df1 %>% 
  filter(FiberTypeNum == 4) %>% 
  lmer(`Po-Control` ~ `ExpCond` + (1 + `ExpCond` | `SubjectNum`), data = .) 
ST_post_I.IA = table_glht(summary(glht(ST_mod_I.IA, linfct = mcp(ExpCond = "Tukey"))))
ST_mod_I.IA  = tidy(ST_mod_I.IA)

ST_mod_IIAX = df1 %>% 
  filter(FiberTypeNum == 5) %>% 
  lmer(`Po-Control` ~ `ExpCond` + (1 + `ExpCond` | `SubjectNum`), data = .) 
ST_post_IIAX = table_glht(summary(glht(ST_mod_IIAX, linfct = mcp(ExpCond = "Tukey"))))
ST_mod_IIAX  = tidy(ST_mod_IIAX)

# DF for sinusoidal analysis b/t Control & Fatigue (MHC I & IIA)
df2_I = my_data %>%
  filter(FiberTypeNum %in% c(1)) %>%
  filter(ExpCondNum %in% c(1,2)) %>%
  filter(Grp == 1) %>%
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`)) 

df2_IIA = my_data %>%
  filter(FiberTypeNum %in% c(2)) %>%
  filter(ExpCondNum %in% c(1,2)) %>%
  filter(Grp == 1) %>%
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`))

models_ConvFat_I = purrr::map(df2_I[,20:length(df2_I)], ~lmer(.x ~ df2_I$ExpCond + (1 + df2_I$ExpCond | df2_I$SubjectNum)))
models_ConvFat_I  = purrr::map(models_ConvFat_I, ~tidy(.x))


models_ConvFat_IIA = purrr::map(df2_IIA[,20:length(df2_IIA)],~lmer(.x ~ df2_IIA$ExpCond + (1 + df2_IIA$ExpCond | df2_IIA$SubjectNum)))
models_ConvFat_IIA  = purrr::map(models_ConvFat_IIA, ~tidy(.x))


# DF for sinusoidal analysis b/t Control & Fatigue+dATP (MHC I & IIA)
df3_I = my_data %>%
  filter(FiberTypeNum %in% c(1)) %>%
  filter(ExpCondNum %in% c(1,3)) %>%
  filter(Grp == 2) %>%
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`))

df3_IIA = my_data %>%
  filter(FiberTypeNum %in% c(2)) %>%
  filter(ExpCondNum %in% c(1,3)) %>%
  filter(Grp == 2) %>%
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`))

models_ConvFatdATP_I = purrr::map(df3_I[,20:length(df3_I)], ~lmer(.x ~ df3_I$ExpCond + (1 + df3_I$ExpCond | df3_I$SubjectNum)))
models_ConvFatdATP_I  = purrr::map(models_ConvFatdATP_I, ~tidy(.x))



models_ConvFatdATP_IIA = purrr::map(df3_IIA[,20:length(df3_IIA)], ~lmer(.x ~ df3_IIA$ExpCond + (1 + df3_IIA$ExpCond | df3_IIA$SubjectNum)))
models_ConvFatdATP_IIA  = purrr::map(models_ConvFatdATP_IIA, ~tidy(.x))



# DF for sinusoidal analysis b/t Fatigue & Fatigue+dATP (MHC I & IIA)
df4_I = my_data %>%
  filter(FiberTypeNum %in% c(1)) %>%
  filter(ExpCondNum %in% c(2,3)) %>%
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`))
df4_I = na.omit(df4_I)

df4_IIA = my_data %>%
  filter(FiberTypeNum %in% c(2)) %>%
  filter(ExpCondNum %in% c(2,3)) %>%
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`))
df4_IIA = na.omit(df4_IIA)

models_FatvFatdATP_I = purrr::map(df4_I[,20:length(df4_I)], ~lmer(.x ~ df4_I$ExpCond + (1 | df4_I$SubjectNum)))
models_FatvFatdATP_I  = purrr::map(models_FatvFatdATP_I, ~tidy(.x))


models_FatvFatdATP_IIA = purrr::map(df4_IIA[,20:length(df4_IIA)],~lmer(.x ~ df4_IIA$ExpCond + (1 | df4_IIA$SubjectNum)))
models_FatvFatdATP_IIA  = purrr::map(models_FatvFatdATP_IIA, ~tidy(.x))


## DF for sinsoidal analysis b/t Control & Control+dATP

df5_I = my_data %>% 
  filter(FiberTypeNum == 1) %>% 
  filter(ExpCondNum %in% c(4,10)) %>% 
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`)) 

df5_IIA = my_data %>% 
  filter(FiberTypeNum == 2) %>% 
  filter(ExpCondNum %in% c(4,10)) %>% 
  dplyr::select(c(`Filename`:`Po-Control`,`DynamicAmplitude`:`Aviscous`))

ConvCondATP_mod_I = purrr::map(df5_I[, 19:length(df5_I)], ~lmer(.x ~ df5_I$ExpCond + (1 + df5_I$ExpCond| df5_I$SubjectNum)))
ConvCondATP_mod_IIA = purrr::map(df5_IIA[, 19:length(df5_IIA)], ~lmer(.x ~ df5_IIA$ExpCond + (1 + df5_IIA$ExpCond| df5_IIA$SubjectNum)))

ConvCondATP_mod_I  = purrr::map(ConvCondATP_mod_I,~tidy(.x))
ConvCondATP_mod_IIA = purrr::map(ConvCondATP_mod_IIA, ~tidy(.x))

## DF for % difference (pdiff)
df6_I = my_data %>% 
  filter(FiberTypeNum == 1) %>% 
  filter(ExpCondNum %in% c(2,3,4)) %>% 
  dplyr::select(c(`Filename`:`ExpCondNum`,`pdiff_A`:`pdiff_twopib`))
df6_I = na.omit(df6_I)

df6_IIA = my_data %>% 
  filter(FiberTypeNum == 2) %>% 
  filter(ExpCondNum %in% c(2,3,4)) %>% 
  dplyr::select(c(`Filename`:`ExpCondNum`,`pdiff_A`:`pdiff_twopib`))
df6_IIA = na.omit(df6_IIA)
  
models_pdiff_I = purrr::map(df6_I[,19:length(df6_I)], ~lmer(.x ~ df6_I$ExpCond + (1 | df6_I$SubjectNum)))
posthoc_pdiff_I = purrr::map(models_pdiff_I, ~ summary(glht(.x, linfct=mcp("df6_I$ExpCond" ="Tukey"))))
models_pdiff_I = purrr::map(models_pdiff_I, ~tidy(.x))
posthoc_pdiff_I = purrr::map(posthoc_pdiff_I, ~table_glht(.x))

models_pdiff_IIA = purrr::map(df6_IIA[,19:length(df6_IIA)], ~lmer(.x ~ df6_IIA$ExpCond + (1 | df6_IIA$SubjectNum)))
posthoc_pdiff_IIA = purrr::map(models_pdiff_IIA, ~ summary(glht(.x, linfct=mcp("df6_IIA$ExpCond" ="Tukey"))))
models_pdiff_IIA = purrr::map(models_pdiff_IIA, ~tidy(.x))
posthoc_pdiff_IIA = purrr::map(posthoc_pdiff_IIA, ~table_glht(.x))

## Exporting 

models = list(ST_mod_I, ST_post_I, ST_mod_IIA, ST_post_IIA, ST_mod_I.IA, ST_post_I.IA, ST_mod_IIAX, ST_post_IIAX,
              models_ConvFat_I,  models_ConvFat_IIA,
              models_ConvFatdATP_I, models_ConvFatdATP_IIA, 
              models_FatvFatdATP_I, models_FatvFatdATP_IIA, 
              ConvCondATP_mod_I, ConvCondATP_mod_IIA,
              models_pdiff_I, posthoc_pdiff_I, models_pdiff_IIA, posthoc_pdiff_IIA)

models = purrr::map(models,
                    ~ data.frame(.x))

names(models) = c('Po Model-MHC I', 'Po Posthoc-MHC I','Po Model-MHC IIA','Po Posthoc-MHC IIA','Po Model-MHC I.IA','Po Posthoc-MHC I.IA','Po Model-MHC IIAX','Po Posthoc-MHC IIAX',
                  'ConvFat Model-MHC I','ConvFat Model-MHC IIA', 
                  'ConvFatdATP Model-MHC I', 'ConvFatdATP Model-MHC IIA', 
                  'FatvFatdATP Model-MHC I', 'FatvFatdATP Model-MHC IIA', 
                  'ConvCondATP Model-MHC I', 'ConvCondATP Model-MHC IIA', 
                  'pdiff Model-MHC I', 'pdiff Posthoc-MHC I','pdiff Model-MHC IIA', 'pdiff Posthoc-MHC IIA' 
                  )

write.xlsx(models, "Auroa.xlsx")


### New Method: Nested functions ----------------------------------

# This works for only one variable. Cannot figure out how to iterate over multiple variables.
# From 'https://www.youtube.com/watch?v=KdDyRt9XV_g'
# fit_model = function(df){
#   model = lmer(`B-N` ~ ExpCond + (1 + ExpCond | SubjectNum), data = df)
#   tidy(model)
# }
# 
# ConvFat_model = my_data %>%
#   filter(FiberType %in% c("I", "IIA")) %>% 
#   filter(ExpCondNum %in% c(1,2)) %>%
#   filter(Grp == 1) %>%
#   dplyr::select(`SubjectNum`, `FiberType`, `ExpCond`,
#                 `DynamicAmplitude`:`Aviscous`) %>%
#   group_by(FiberType) %>%
#   nest() %>%
#   mutate(model = map(data,fit_model))


# ##  This is ChatGPT attempt to itterate over multiple variables. None of this shit works ------------------------------------
# 
# fit_model <- function(df, var_name) {
#   formula <- as.formula(paste(var_name, "~ ExpCond + (1 + ExpCond | SubjectNum)"))
#   print(paste("Fitting model with formula:", formula))
#   print(head(df))  # Print the first few rows of the data
#   model <- tryCatch({
#     lmer(formula, data = df)
#   }, error = function(e) {
#     print(paste("Error:", e$message))
#     return(e)
#   })
#   if (inherits(model, "error")) {
#     return(model)
#   }
#   tidy(model)
# }
# 
# # List of variables to iterate over
# variables <- c("DynamicAmplitude", "SpecificForce:DynamicAmplitude", "MaxWork", "MaxWorkFreq",
#                "MaxPower", "MaxPowerFreq", "A-kN", "A-N", "k", "B-kN", "B-N", "lb", "C-kN",
#                "C-N", "lc", "R2", "twopic", "ton", "twopib", "twopic/twopib", "Aelastic", "Aviscous")
# 
# # Process the nested data and fit models
# ConvFat_model <- my_data %>%
#   filter(FiberType %in% c("I", "IIA")) %>%
#   filter(ExpCondNum %in% c(1,2)) %>%
#   filter(Grp == 1) %>%
#   dplyr::select(SubjectNum, FiberType, ExpCond, DynamicAmplitude:Aviscous) %>%
#   group_by(FiberType) %>%
#   nest() %>%
#   mutate(
#     models = map(data, ~map(variables, ~fit_model(.x, .y)))
#   )
# 
# # Attempt # 2 -------------------------------------------------------------------------
# # Define the function to fit the model
# fit_model = function(df, var_name){
#   model = lmer(var_name ~ ExpCond + (1 + ExpCond | SubjectNum), data = df)
#   tidy(model)
# }
# 
# # List of variables to iterate over
# variables <- c("DynamicAmplitude", "SpecificForce:DynamicAmplitude", "MaxWork", "MaxWorkFreq",
#                "MaxPower", "MaxPowerFreq", "A-kN", "A-N", "k", "B-kN", "B-N", "lb", "C-kN",
#                "C-N", "lc", "R2", "twopic", "ton", "twopib", "twopic/twopib", "Aelastic", "Aviscous")
# 
# # Process the nested data and fit models
# ConvFat_model <- my_data %>%
#   filter(FiberType %in% c("I", "IIA")) %>%
#   filter(ExpCondNum %in% c(1,2)) %>%
#   filter(Grp == 1) %>%
#   dplyr::select(SubjectNum, FiberType, ExpCond, everything()) %>%
#   group_by(FiberType) %>%
#   nest() %>%
#   mutate(
#     models = map(data, ~map(variables, ~fit_model(.x, .y)))
#   )


## Attempt # 3: Microsoft Copilot (works, just with the model)----------------------------------------

fit_model <- function(df, var_name) {
  formula <- as.formula(paste0("`", var_name, "` ~ ExpCond + (1 + ExpCond | SubjectNum)"))
  model <- lmer(formula, data = df)
  tidy(model)
}

var_names <- c("DynamicAmplitude", "SF-DA", "MaxWork", "MaxWorkFreq",
                              "MaxPower", "MaxPowerFreq", "A-kN", "A-N", "k", "B-kN", "B-N", "lb", "C-kN",
                              "C-N", "lc", "R2", "twopic", "ton", "twopib", "twopic-twopib", "Aelastic", "Aviscous")

test <- my_data %>%
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(1, 2)) %>%
  filter(Grp == 1) %>%
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_names)) %>%
  group_by(FiberType) %>%
  nest() %>%
  mutate(models = map(data, ~ map(var_names, fit_model, df = .x)))


## Attempt # 4: CoPilot lmer() plus posthoc------------------------------------------------------

# Function to fit the linear mixed model for a given variable
fit_model <- function(df, var_name) {
  formula <- as.formula(paste0("`", var_name, "` ~ ExpCond + (1 + ExpCond | SubjectNum)"))
  model <- lmer(formula, data = df)
  model
}

# Function to perform Tukey post-hoc analysis
posthoc_analysis <- function(model) {
  emmeans_obj <- emmeans(model, ~ ExpCond)
  tukey_result <- pairs(emmeans_obj, adjust = "tukey")
  tidy(tukey_result)
}

var_names <- c("DynamicAmplitude", "SF-DA", "MaxWork", "MaxWorkFreq",
               "MaxPower", "MaxPowerFreq", "A-kN", "A-N", "k", "B-kN", "B-N", "lb", "C-kN",
               "C-N", "lc", "R2", "twopic", "ton", "twopib", "twopic-twopib", "Aelastic", "Aviscous")

test <- my_data %>%
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(1, 2)) %>%
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_names)) %>%
  group_by(FiberType) %>%
  nest() %>%
  mutate(models = map(data, ~ map(var_names, fit_model, df = .x)),
         posthoc = map(models, ~ map(.x, posthoc_analysis)))

# Print summaries of the models and post-hoc analyses
test %>%
  mutate(model_summaries = map(models, ~ map(.x, summary)),
         posthoc_summaries = map(posthoc, ~ map(.x, print))) %>%
  pull(model_summaries) %>%
  walk2(test$FiberType, ~{
    cat("Summaries for FiberType", .y, ":\n")
    walk(.x, print)
    cat("\n")
  })

test %>%
  pull(posthoc) %>%
  walk2(test$FiberType, ~{
    cat("Post-hoc analyses for FiberType", .y, ":\n")
    walk(.x, print)
    cat("\n")
  })

