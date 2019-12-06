library(gofer)
gofer <- gofer::gofer(data = data_core, ma_effect = ma_core$effect_estimate,
                      ma_lower =  ma_core$lower_limit, ma_upper = ma_core$upper_limit, 
                      data_age = data_core_age, grade_rating = "Moderate")
ggplot2::ggsave(gofer, device = "tiff", filename = "fig-two.tiff",
                height=14, width=20, units = "in")
