library(ggfittext)
library(gridExtra)
library(grid)



year <- data_core %>%
  distinct(Study,.keep_all = TRUE) %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Year), place = "left", reflow=TRUE, 
                 position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()

study <- data_core %>% 
  distinct(Study,.keep_all = TRUE) %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(id = row_number()) %>%  
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Study, hjust="left"), place = "left", reflow=TRUE, 
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()


results <- data_core %>% 
  ggplot()+
  geom_point(aes(x=Study, y=bias, alpha=group), colour = "#002a60", position = position_dodge(width = 0.99), show.legend=FALSE)+
  geom_linerange(aes(x=Study, ymin=lower, ymax=upper, alpha=group), colour = "#002a60",size=1, position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  coord_flip()+ 
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white")
        )+
  scale_y_continuous(expand=c(0.1,0.1), limits=c(min(data_core$lower),max(data_core$upper)))+
  scale_x_discrete(breaks = NULL) #removes horizontal grid lines



participants <- data_core %>% 
  mutate(id = row_number()) %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot(aes(x=Study, y=n))+
    geom_linerange(aes(x=Study, ymin=0, ymax=n, alpha=group), 
                   colour = "#930093", 
                   size=2, position = position_dodge(width = 0.99), show.legend=FALSE)+
  geom_label(aes(x=Study, y=n, label = n, alpha=group), 
             colour="#930093", 
             position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "white", colour = "white",
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "#ffceff")
  )+
  scale_x_discrete(breaks = NULL)+ #removes horizontal grid lines
  geom_text(aes( x=Study, y=-5, alpha=group), 
            label =  "\uf0c0", 
            family =  "FontAwesome", 
            size =  3.5,
            vjust="center", 
            colour = "#930093",
            position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_y_continuous(expand = expand_scale(mult = c(0.1, .1)))


comparison <- data_core %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comparison, alpha=group), place = "left", reflow=TRUE, 
            position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")

patients <- data_core %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = patients, alpha=group), place = "left", reflow=TRUE, 
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")

comments <-  data_core %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comments, alpha=group), place = "left", reflow=TRUE, 
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")



measurements <- data_core %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = N, alpha=group), place = "left", reflow=TRUE, 
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")


flow <- data_core %>% 
  distinct(Study,.keep_all = TRUE) %>% 
  mutate(RoB_flow = factor(RoB_flow, 
                              levels = c("high","low","unclear"),
                              labels = c( "\uf00d","\uf00c", "\uf128"))) %>% 
  ggplot()+
  geom_point(aes(x=Study, y=0, colour = RoB_flow), size=8, show.legend = FALSE)+
  geom_text(aes(x=Study, y=0, label= RoB_flow), 
            family =  "FontAwesome", 
            size =  3.5,
            vjust="center", show.legend=FALSE)+  
  theme_void()+
  coord_flip()+
  scale_color_manual(values=c("red", "green", "yellow"))



ma <- primary %>% 
  ggplot()+
  geom_point(aes(x=0, y=bias_mean))+
  geom_linerange(aes(x=0, ymin=CI_L_rve, ymax=CI_U_rve), size=1)+
  coord_flip()+ 
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "white"),
    axis.text.x = element_text()
  )+
  scale_x_continuous(expand=c(0,0), limits=c(-1,1))+
  scale_y_continuous(expand=c(0.1,0.1), limits=c(min(data_core$lower),max(data_core$upper)))+
  scale_x_discrete(breaks = NULL)  #removes horizontal grid lines

 
# using gtable seems to be the best option - see here for documentation: https://gtable.r-lib.org/index.html

library(gtable)

gt <- gtable(widths = unit(c(rep(1,9)), 'null'), heights = unit(c(rep(1,length(unique(data_core$Study))+2)), 'null'))

gtable_show_layout(gt) # use this to see layout
gt <- gt %>%     
  gtable_add_grob(ggplotGrob(study), t=2,l=1, b=14) %>% 
  gtable_add_grob(ggplotGrob(year), t=2, l=2, b=14) %>% 
  gtable_add_grob(ggplotGrob(ma), t=15,l=9) %>% 
    gtable_add_grob(ggplotGrob(results), t=2,l=9, b=14) %>% 
    gtable_add_grob(ggplotGrob(participants), t=2,l=8, b=14) %>% 
    gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                      aes(x=0,y=0, 
                                          label = "Study findings and meta-analysis"))+
                                        geom_fit_text(reflow = TRUE, colour = "white",
                                                      fontface="bold")+
                                        theme_void()+
                                        theme(plot.background = element_rect(fill = "#002a60"))
                                      ), t=1,l=8, r=9) %>%
    gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                      aes(x=0,y=0, 
                                          label = "Average mean bias and population limits of agreement"))+
                                 geom_fit_text(reflow = TRUE, colour = "white",
                                               fontface="bold")+
                                 theme_void()+
                                 theme(plot.background = element_rect(fill = "#002a60"))
                              ), t=15, l=8) %>% 
  gtable_add_grob(roundrectGrob(gp=gpar(fill="transparent")),
                  t=9,l=1,r=9) %>% 
  
  gtable_add_grob(roundrectGrob(gp=gpar(fill="transparent")),
                  t=14,l=1,r=9)


plot(gt) # this is just for debugging - shows plot with grid background
 
grid.newpage() # use newpage and grid.draw to plot the gtable
grid.draw(gt)

ggsave(plot = gt, device = "tiff", "gtable-plot.tiff", width = 279,height = 210, units = "mm",dpi=300, compression = 'lzw')



# # combining with cowplot as below will only work when always same number of columns
# combined <- gridExtra::grid.arrange(arrangeGrob(study,
#                                     year,
#                                     flow,
#                                     participants, 
#                                     measurements,
#                                     comparison,
#                                     patients,
#                                     comments,
#                                     results,
#                                     nrow=1))
# toprow <- cowplot::plot_grid(study,
#           year,
#           flow,
#           participants, 
#           measurements,
#           comparison,
#           patients,
#           comments,
#           results, rel_widths =c(1/9,0.5/9,0.5/9,1.5/9,0.5/9,0.5/9,1/9,1/9,1.5/9), ncol=9)
# 
# bottomrow <- cowplot::plot_grid(NULL,
#                                 NULL,
#                                 NULL,
#                                 NULL, 
#                                 NULL,
#                                 NULL,
#                                 NULL,
#                                 ggplot(data=NULL, aes(x=0, y=0,
#                                            label = "Population limits of agreement"))+
#                                   geom_tile(fill="#002a60") +
#                                   geom_fit_text(reflow = TRUE, colour = "white",  fontface="bold")+
#                                   theme_void(),
#                                 ma,
#                                 rel_widths = c(1/9,0.5/9,0.5/9,0.5/9,1/9,1/9,1/9,1/9,1.5/9), ncol = 9)
# 
# titlerow <- cowplot::plot_grid(NULL,
#                                 NULL,
#                                 NULL,
#                                 NULL, 
#                                 NULL,
#                                 NULL,
#                                 NULL,
#                                ggplot(data=NULL, aes(x=0, y=0,
#                                                      label = "Results"))+
#                                  geom_tile(fill="#002a60") +
#                                  geom_fit_text(reflow = TRUE, colour = "white",  fontface="bold")+
#                                  theme_void(),
#                                ggplot(data=NULL, aes(x=0, y=0,
#                                                      label = "Results"))+
#                                  geom_tile(fill="#002a60") +
#                                  geom_fit_text(reflow = TRUE, colour = "white",  fontface="bold")+
#                                  theme_void(),
#                                 rel_widths = c(1/9,0.5/9,0.5/9,0.5/9,1/9,1/9,1/9,1/9,1.5/9), ncol = 9)
# 
# (cowplot <- cowplot::plot_grid(titlerow,toprow, bottomrow, ncol = 1, rel_heights = c(0.5/10, 9/10,0.5/10)))
# ggsave(plot = cowplot, device = "tiff", "forest-plot.tiff", width = 279,height = 210, units = "mm",dpi=300, compression = 'lzw')
