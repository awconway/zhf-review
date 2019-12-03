fig <- ggprisma::ggprisma(retrieved = 130, included = 16, duplicates = 35, 
                   full_text = 22, wrong_intervention = 4, 
                   wrong_comparator = 2, wrong_design = 1, 
                   awaiting_classification = 2)

ggsave("fig-one.pdf", device = "pdf")
