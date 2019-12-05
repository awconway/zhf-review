#' Results of meta-analysis
#'@rdname loa_maker
#'@name loa_maker
#'
#'@return dataframe

#' @export
#' 
loa_maker <- function(bias,V_bias,logs2,V_logs2) {
  bias_row=meta(bias, V_bias)
  logs2_row=meta(logs2, V_logs2)
  bias_mean <- bias_row[2]
  sd2_est <- exp(logs2_row[2])
  tau_est <- bias_row[3]
  LOA_L <- bias_mean - 2*sqrt(sd2_est + tau_est)
  LOA_U <- bias_mean + 2*sqrt(sd2_est + tau_est)
  m <- bias_row[1]
  tcrit <- qt(1-.05/2,m-1)
  B1 <- sd2_est^2/(sd2_est + tau_est)
  B2 <- tau_est^2/(sd2_est + tau_est)
  wt <- 1/V_bias
  S1 <- sum(wt)
  S2 <- sum(wt^2)
  S3 <- sum(wt^3)
  A0 <- 2*(m-1)/(S1-S2/S1)^2
  A1 <- 4/(S1 - S2/S1)
  A2 <- 2*(S2-2*S3/S1+S2^2/S1^2)/(S1-S2/S1)^2
  V_logT2 <- A0/tau_est^2 + A1/tau_est + A2
  V_logT2 <- 2/sum((V_bias + tau_est)^(-2))
  V_LOA_mod <- bias_row[4] + B1*logs2_row[4] + B2*V_logT2
  V_LOA_rve <- bias_row[5] + B1*logs2_row[5] + B2*V_logT2
  CI_L_mod <- LOA_L - tcrit*sqrt(V_LOA_mod)
  CI_U_mod <- LOA_U + tcrit*sqrt(V_LOA_mod)
  CI_L_rve <- LOA_L - tcrit*sqrt(V_LOA_rve)
  CI_U_rve <- LOA_U + tcrit*sqrt(V_LOA_rve)
  data.frame(m, bias_mean, sd2_est, tau_est, LOA_L, LOA_U, CI_L_mod, CI_U_mod, CI_L_rve, CI_U_rve)
}