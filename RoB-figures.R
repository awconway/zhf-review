library(robvis)
library(readxl)
library(tidyverse)

frame <- read_excel(here::here("manuscript", "data", "zhf_extracted.xlsx"))

RoB <- frame %>% 
  select(Study, RoB_selection, RoB_spoton, RoB_comparator, RoB_flow) %>% 
  mutate(RoB_overall = if_else(RoB_selection == "low" &
                                 RoB_spoton == "low" &
                                 RoB_comparator == "low" &
                                 RoB_flow == "low", "low", "high"))

RoB[RoB == "unclear"] <- "some concerns"

rob_summary(data = RoB, tool = "QUADAS-2", weighted = FALSE)
rob_traffic_light(data = RoB, tool = "QUADAS-2")

pr_push()
