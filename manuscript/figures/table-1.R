gt <- df %>% 
  rename(" " = Comparison, "Mean bias" = bias_mean, "Variance" = sd2_est, 
         "Tau-squared" = tau_est, "Lower bound for 95% limit of agreement" = LOA_L, 
         "Upper bound for 95% limits of agreement" = LOA_U, 
         "Outer confidence interval for lower 95% limits of agreement" = CI_L_rve, 
         "Outer confidence interval for upper 95% limits of agreement" = CI_U_rve) %>% 
  gt() %>%
  tab_header(
    title = "Table 1. Results of meta-analyses"  )  %>%
  tab_spanner(
    label = "Population limits of agreement",
    columns = vars(
      `Outer confidence interval for lower 95% limits of agreement`, `Outer confidence interval for upper 95% limits of agreement`)
  )

gtsave(data = gt, filename = "table_gt.pdf", vwidth = 600)
