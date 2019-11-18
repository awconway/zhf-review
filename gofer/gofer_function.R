library(tidyverse)
library(ggfittext)
library(gridExtra)
library(grid)
library(gtable)
library(naniar)
library(ggimage)
data <- readxl::read_xlsx("~/zhf-review/gofer/data.xlsx")

data <- data %>%
  mutate(c = N/n)

#variance

data <- data %>% 
  mutate(s2 = case_when(corrected == "Yes"  ~ ((upper - bias)/1.96)^2,
                        corrected == "No" ~  ((upper - bias)/1.96)^2*(N-1)/(N-c)
  )
  )

data <- data %>% 
  mutate(V_bias = s2/n)

data <- data %>% 
  mutate(logs2 = log(s2)+1/(n-1))

data <- data %>% 
  mutate(V_logs2 = 2/(n-1))

# meta-analyses categories

data_NPA <- data %>% 
  filter(comparison == "NPA")

data_SL <- data %>% 
  filter(comparison == "Sublingual")

#eshragi overall
data_core <- data %>% 
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter(comments != "Intraoperative, off cardiopulmonary bypass" & comments != "Postoperative")

data_core_op <- data %>% #eshragi intraop
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter (clinical_setting=="Intraoperative")

data_core_icu <- data %>% #eshragi postop
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter(clinical_setting=="ICU" | clinical_setting=="Postoperative")

# ROB 

data_core_lowrisk <- data_core %>% #eshragi overall
  filter(RoB_selection == "low" & RoB_spoton == "low" & RoB_comparator == "low" & RoB_flow =="low")

# conflict of interests

data_conflict <- data_core %>% 
  filter(`Funding/equipment/conflict with ZHF company` == "No")

format_results <- function(group){
  out <- bama::loa_maker(group$bias,group$V_bias, group$logs2, group$V_logs2)
  out <- round(out, digits=2)
  out <- out %>% 
    mutate(Participants = sum(group$n_count, na.rm=T)) %>% 
    mutate(Measurements = format(sum(group$N, na.rm = T), big.mark=",", scientific = FALSE)) %>% 
    mutate(Studies = length(unique(group$Study))) %>% 
    select(Studies, m, Participants, Measurements, bias_mean, sd2_est, tau_est, LOA_L, LOA_U, CI_L_rve, CI_U_rve) %>% 
    rename("Comparisons" = m)
  out
  
  # Better names for columns
  # , "Mean bias" = bias_mean, "Tau-squared" = tau_est, "Lower bound for 95% limit of agreement" = LOA_L, "Upper bound for 95% limits of agreement" = LOA_U, "Outer confidence interval for lower 95% limits of agreement" = CI_L_rve, "Outer confidence interval for upper 95% limits of agreement" = CI_U_rve
}



primary <- format_results(data_core)
icu <- format_results(data_core_icu)

data_core_icu %>% 
  gofer::gofer(ma_effect=icu$bias_mean, ma_lower=icu$CI_L_rve, 
               ma_upper=icu$CI_U_rve)
