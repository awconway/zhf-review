#' Intermediate function for calculating meta-analysis
#'@rdname meta
#'@name meta

#' @export
#' 
meta <-  function(Te,V_T){
m <- length(Te)
wt_FE=1/V_T
T_FE <- sum(Te*wt_FE)/sum(wt_FE)
Q <- sum(wt_FE*(Te - T_FE)^2)
S1 <- sum(wt_FE)
S2 <- sum(wt_FE^2)
o2 <- (Q - (m - 1))/(S1 - S2/S1)
wt_RE <- 1/(V_T + o2)
T_RE <- sum(Te*wt_RE)/sum(wt_RE)
V_T_RE_mod <- 1/sum(wt_RE)
V_T_RE_rve <- (m/(m-1))*sum(wt_RE^2*(Te - T_RE)^2)/(sum(wt_RE))^2
c(m,T_RE,o2,V_T_RE_mod, V_T_RE_rve)
}