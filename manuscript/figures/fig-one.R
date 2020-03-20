fig <- ggprisma::ggprisma(retrieved = 131, included = 18, duplicates = 35, full_text = 23, wrong_intervention = 3, wrong_comparator = 2, wrong_design = 1, awaiting_classification = 2)

ggsave(plot = fig, device = "pdf", filename = "manuscript/figures/fig-one.pdf")

ggsave(plot = fig, device = "tiff", filename = "manuscript/figures/fig-one.tiff", width=7.5, height=7, units='in', dpi=600, compression = 'lzw')
