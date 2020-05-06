library(tidyverse)
library(gofer)
library(robvis)
library(magrittr)
RoB <- data_core %>%
  dplyr::bind_rows(data_SL) %>%
  dplyr::bind_rows(data_NPA) %>%
  dplyr::select(Study, RoB_selection, RoB_spoton, RoB_comparator, RoB_flow) %>%
  dplyr::mutate(RoB_overall = dplyr::if_else(RoB_selection == "low" &
                                               RoB_spoton == "low" &
                                               RoB_comparator == "low" &
                                               RoB_flow == "low", "low", "high"))

RoB[RoB == "unclear"] <- "some concerns"

rob_summary(data = RoB, tool = "QUADAS-2", weighted = FALSE)

ggsave(device = "png", filename = "manuscript/figures/rob-summary.png")


rob_traffic_light(data = RoB, tool = "QUADAS-2")

ggsave(device = "pdf", filename = "manuscript/figures/fig2.pdf",
       height = 234, width = 174, units = "mm")
