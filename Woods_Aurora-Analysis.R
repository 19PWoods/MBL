library(tidyverse)
library(broom.mixed)
library(multcomp)
library(lmerTest)
library(readxl)
library(emmeans)


my_data = read_excel(file.choose(),
                      sheet = 'raw') %>%
  mutate(FiberType = factor(FiberType)) %>% 
  mutate(FiberTypeNum = factor(FiberTypeNum)) %>% 
  mutate(ExpCond = factor(ExpCond)) %>% 
  mutate(ExpCondNum = factor(ExpCondNum)) %>% 
  mutate(Grp = factor(Grp)) %>% 
  mutate(SubjectNum = factor(SubjectNum)) %>% 
  dplyr::select(`SubjectNum`, `FiberType`, `ExpCond`:`ExpCondNum`,`Po-Control`, `pdiff_Ten`, `Po*CSA`,
                `Grp`,`DynamicAmplitude`:`pdiff_twopib`) %>% 
  group_by(SubjectNum, FiberType,  ExpCond)

rigor_data = read_excel(file.choose(),
                        sheet = "Format1") %>%
  dplyr::select(`Subj.#`, `MHC`, `MHC #`, `Rigor-Tension (mN/mm2)`, `Rigor-Ft-Tension (mN/mm2)`,
                `Rigor-Stiffness - 100 Hz (N/mm2)`, `Rigor-Ft-Stiffness - 100 Hz (N/mm2)`,
                `Rigor Tension-Stiffness ratio`, `Rigor-FT-Tension-Stiffnesss ratio`)

# Function to fit repeated measures linear mixed model
fit_model_rep <- function(df, var_name) {
  formula <- as.formula(paste0("`", var_name, "` ~ ExpCond + (1 + ExpCond | SubjectNum)"))
  model <- lmer(formula, data = df)
  model
}

# Function to fit the linear mixed model for a given variable
fit_model <- function(df, var_name) {
  formula <- as.formula(paste0("`", var_name, "` ~ ExpCond + (1 | SubjectNum)"))
  model <- lmer(formula, data = df)
  model
}

# Function to perform Tukey post-hoc analysis
posthoc_analysis <- function(model) {
  hoc = summary(glht(model, linfct = mcp(ExpCond = "Tukey")))
  tidy(hoc)
}

# # Function to perform Tukey post-hoc (alternative)
# posthoc_analysis2 <- function(model) {
#   emmeans_obj <- emmeans(model, ~ ExpCond)
#   tukey_result <- pairs(emmeans_obj, adjust = "tukey")
#   summary(tukey_result)
# }

# String of variable names 
var_names <- c("DynamicAmplitude", "SF-DA", "MaxWork", "MaxWorkFreq", "MaxPower", "MaxPowerFreq", "A-kN",
               "A-N", "k", "B-kN", "B-N", "lb", "C-kN","C-N", "lc", "R2", "twopic", "ton", "twopib", 
               "twopic-twopib", "Aelastic", "Aviscous")
var_force = c("Po-Control", "Po*CSA")
var_controldATP = c("Po-Control", "B-N", "C-N", "A-N", "k", "twopib", "ton")
var_pdiff = c("pdiff_Ten","pdiff_A", "pdiff_Aelastic", "pdiff_Aviscous", "pdiff_k", "pdiff_B",
              "pdiff_C", "pdiff_ton", "pdiff_twopib")


# Model 1: Tension and Forces for Control, Fatigue, Fatigue + dATP
forces_model = my_data %>% 
  filter(FiberType %in% c("I", "I/IIA", "IIA", "IIAX")) %>% 
  filter(ExpCondNum %in% c(1:3)) %>% 
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_force)) %>%
  group_by(FiberType) %>% 
  nest() %>% 
  mutate(models = map(data, ~ map(var_force, fit_model_rep, df = .x)),
         model_tidy = map(models, ~ map(.x, tidy)),
         posthoc = map(models, ~ map(.x, posthoc_analysis)))

# Model 2: Variables for Control, Fatigue
ConvFat_model <- my_data %>%
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(1, 2)) %>%
  filter(Grp == 1) %>%
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_names)) %>%
  group_by(FiberType) %>%
  nest() %>%
  mutate(models = map(data, ~ map(var_names, fit_model_rep, df = .x)),
         model_tidy = map(models, ~ map(.x, tidy))) 

# Model 3: Variables for Control, Fatigue+dATP
ConvFatdATP_model <- my_data %>%
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(1, 3)) %>%
  filter(Grp == 2) %>%
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_names)) %>%
  group_by(FiberType) %>%
  nest() %>%
  mutate(models = map(data, ~ map(var_names, fit_model_rep, df = .x)),
         model_tidy = map(models, ~ map(.x, tidy)))

# Model 4: Variables for Fatigue, Fatigue+dATP
FatvFatdATP_model <- my_data %>%
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(2, 3)) %>%
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_names)) %>%
  group_by(FiberType) %>%
  nest() %>%
  mutate(models = map(data, ~ map(var_names, fit_model, df = .x)),
         model_tidy = map(models, ~ map(.x, tidy)))

# Model 5: Tension & Variables for Control, Control+dATP
ConvCondATP_model = my_data %>% 
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(4,10)) %>% 
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_controldATP)) %>%
  group_by(FiberType) %>% 
  nest() %>% 
  mutate(models = map(data, ~ map(var_controldATP, fit_model, df = .x)),
         model_tidy = map(models, ~ map(.x, tidy)))

# Model 6: Percent difference for Fatigue, Fatigue+dATP, Control+dATP
pdiff_model = my_data %>% 
  filter(FiberType %in% c("I", "IIA")) %>% 
  filter(ExpCondNum %in% c(2:4)) %>% 
  dplyr::select(SubjectNum, FiberType, ExpCond, all_of(var_pdiff)) %>%
  group_by(FiberType) %>% 
  nest() %>% 
  mutate(models = map(data, ~ map(var_pdiff, fit_model_rep, df = .x)),
         model_tidy = map(models, ~ map(.x, tidy)),
         posthoc = map(models, ~ map(.x, posthoc_analysis)))

# Model 7-10: Rigor Models 

rigor_tension_model = rigor_data %>% 
  dplyr::select(`Subj.#`, `MHC`, `MHC #`, 
                `Rigor-Tension (mN/mm2)`, `Rigor-Ft-Tension (mN/mm2)`) %>% 
  filter(MHC %in% c("I", "IIA")) %>% 
  pivot_longer(
    cols = `Rigor-Tension (mN/mm2)`:`Rigor-Ft-Tension (mN/mm2)`,
    names_to = "ExpCond",
    values_to = "value"
               ) %>% 
  group_by(MHC) %>% 
  nest() %>% 
  mutate(model = map(data, ~ lmer(value ~ ExpCond + (1 + ExpCond | `Subj.#`),
                                  data = .x)),
         model_tidy = map(model, tidy))

rigor_stiff_model = rigor_data %>% 
  dplyr::select(`Subj.#`, `MHC`, `MHC #`, 
                `Rigor-Stiffness - 100 Hz (N/mm2)`, `Rigor-Ft-Stiffness - 100 Hz (N/mm2)`) %>% 
  filter(MHC %in% c("I", "IIA")) %>% 
  pivot_longer(
    cols = `Rigor-Stiffness - 100 Hz (N/mm2)`:`Rigor-Ft-Stiffness - 100 Hz (N/mm2)`,
    names_to = "ExpCond",
    values_to = "value"
  ) %>% 
  group_by(MHC) %>% 
  nest() %>% 
  mutate(model = map(data, ~ lmer(value ~ ExpCond + (1 + ExpCond | `Subj.#`),
                                  data = .x)),
         model_tidy = map(model, tidy))

ten_stiff_ratio_model = rigor_data %>% 
  dplyr::select(`Subj.#`, `MHC`, `MHC #`, 
                `Rigor Tension-Stiffness ratio`, `Rigor-FT-Tension-Stiffnesss ratio`) %>% 
  filter(MHC %in% c("I", "IIA")) %>% 
  pivot_longer(
    cols = `Rigor Tension-Stiffness ratio`:`Rigor-FT-Tension-Stiffnesss ratio`,
    names_to = "ExpCond",
    values_to = "value"
  ) %>% 
  group_by(MHC) %>% 
  nest() %>% 
  mutate(model = map(data, ~ lmer(value ~ ExpCond + (1 + ExpCond | `Subj.#`),
                                  data = .x)),
         model_tidy = map(model, tidy))

fig4d_model = read_excel(file.choose()) %>% 
  filter(`Fiber Type` %in% c("I", "IIA")) %>% 
  filter(`Exp Cond` %in% c("Control", "Fatigue")) %>% 
  group_by(`Fiber Type`) %>% 
  nest() %>% 
  mutate(model = map(data, ~ lmer(`Specific Force:Dynamic Amplitude Ratio` ~ 
                                    `Exp Cond` + (1 + `Exp Cond` | `Subject#`),
                                  data = .x)),
         model_tidy = map(model, tidy))
  

# Export to Excel Functions
export_to_excel <- function(model_list, file_name) {
  model_summaries <- model_list %>%
    unnest(model_tidy) %>% 
    unnest(model_tidy)
  
  writexl::write_xlsx(model_summaries, file_name)
}
export_to_excel_force <- function(model_list, file_name) {
  model_tidy <- model_list %>%
    unnest(model_tidy) %>% 
    unnest(model_tidy)
  
  model_posthoc <- model_list %>% 
    unnest(posthoc) %>% 
    unnest(posthoc)
  
  combined = list(model_tidy = model_tidy,
                  model_posthoc = model_posthoc
                  )
  
  writexl::write_xlsx(combined, file_name)
}

# Export each model to a separate Excel sheet
export_to_excel_force(forces_model, "forces_model.xlsx")
export_to_excel(ConvFat_model, "ConvFat_model.xlsx")
export_to_excel(ConvFatdATP_model, "ConvFatdATP_model.xlsx")
export_to_excel(ConvCondATP_model, "ConvCondATP_model.xlsx")
export_to_excel(FatvFatdATP_model, "FatvFatdATP_model.xlsx")
export_to_excel_force(pdiff_model, "pdiff_model.xlsx")
export_to_excel(rigor_tension_model, "rigortension.xlsx")
export_to_excel(rigor_stiff_model, "rigorstiff.xlsx")
export_to_excel(ten_stiff_ratio_model, "tenstiffratio.xlsx")
export_to_excel(fig4d_model, "fig4d.xlsx")

 
# # Print summaries of the models and post-hoc analyses
# # pull() is basically the '$' operator
# ConvFat_model %>% 
#   pull(model_summaries) %>% 
#   walk2(ConvFat_model$FiberType, ~{
#     cat("Summaries for FiberType", .y, ":\n")
#     walk(.x, print)
#     cat("\n")
#   })
