### curves
library(ggplot2)
library(gofer)
bias=data_core$bias
s2_unb = data_core$s2
pooled_bias = primary$bias_mean
pooled_sd = primary$sd2_est
pooled_tau2 = primary$tau_est
pooled_sd = sqrt(pooled_sd^2 + pooled_tau2)

LOA_l = primary$LOA_L
LOA_u = primary$LOA_U
LOA_l_CI = primary$CI_L_rve
LOA_u_CI = primary$CI_U_rve
g <- ggplot(data.frame(x=seq(-20,20,length=200)), aes(x=x)) + 
  stat_function(fun=dnorm, args = list(bias[1], sd=sqrt(s2_unb[1])), colour = "cornflowerblue", size=0.6, alpha = 0.5) 

for (i in 2:length(bias)){
  g <- g + stat_function(fun=dnorm, args = list(bias[i], sd=sqrt(s2_unb[i])), color = "cornflowerblue", size = 0.6, alpha = 0.5)
} 

g <- g + stat_function(fun=dnorm, args = list(pooled_bias, pooled_sd), colour ="lightcoral", alpha = 0.05) + 
  stat_function(fun=dnorm, args = list(pooled_bias, pooled_sd), colour = NA, geom="area", fill="lightcoral", alpha = 0.6) + 
  scale_x_continuous(limits = c(-3.5,3.5), breaks = c(-2, -1, 0, 1, 2)) + 
  labs(x = expression('Difference between core and ZHF temperature measurements in'*~degree*C*''),
       caption = "Blue curves are distributions of the differences between measurements from zero-heat-flux sensors and core\ntemperature measurements in individual studies. The red curve is the distribution of the pooled estimate.")+
  scale_y_continuous(name = "Density \n", limits = c(0,2.5)) +  
  #labs(title = "\nOuter confidence intervals for pooled limits of agreement\n\n  Pooled limits of agreement")+
  theme(plot.title = element_text(hjust = 0.5, margin = margin(t=10, b=-32), size=10),
        axis.text=element_text(hjust = 0.5, size=10),                                                                        
        axis.title=element_text(hjust = 0.5, size=10),
        axis.line=element_line(colour="black", size=0.2),
        panel.background = element_blank(),
        axis.ticks = element_blank(), 
        plot.caption = element_text(hjust = 0)) #+ 
#CI lines that extend to arrow tips
# geom_segment(aes(x=LOA_u, y=0, xend=LOA_u, yend=2.25), size = 0.3, linetype="dashed")+ 
# geom_segment(aes(x=LOA_l, y=0, xend=LOA_l, yend=2.25), size = 0.3, linetype="dashed")+
# geom_segment(aes(x=LOA_u_CI, y=0, xend=LOA_u_CI, yend=2.48), size = 0.3)+
# geom_segment(aes(x=LOA_l_CI, y=0, xend=LOA_l_CI, yend=2.48), size = 0.3)#+
# 
# geom_segment(aes(x=0, y=2.48, xend=LOA_u_CI, yend=2.48), size = 0.4, arrow = arrow(length = unit(0.03, "npc")))+ #R arrow high
# geom_segment(aes(x=0, y=2.48, xend=LOA_l_CI, yend=2.48), size = 0.4, arrow = arrow(length = unit(0.03, "npc")))+ #L arrow high
# geom_segment(aes(x=0, y=2.25, xend=LOA_u, yend=2.25), size = 0.4,  arrow = arrow(length = unit(0.03, "npc")))+ #R arrow low
# geom_segment(aes(x=0, y=2.25, xend=LOA_l, yend=2.25), size = 0.4,  arrow = arrow(length = unit(0.03, "npc"))) #L arrow low
g
# ggsave(plot = g, device = "tiff", filename = "manuscript/figures/fig-two.tiff", width=7.5, height=7, units='in', dpi=600, compression = 'lzw')

ggsave(plot = g, device = "pdf", filename = "manuscript/figures/fig3.pdf",
       width = 174, units = "mm")

