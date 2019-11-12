library(ggfittext)
library(gridExtra)
library(grid)
library(gtable)
library(naniar)

dodge_width = 0.7

year <- data_core %>%
  distinct(Study,.keep_all = TRUE) %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Year), place = "left", reflow=TRUE, 
                 position = position_dodge(width = dodge_width), show.legend=FALSE)+
  theme_void()+
  coord_flip()

study <- data_core %>% 
  distinct(Study,.keep_all = TRUE) %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Study), place = "left", reflow=TRUE, 
                position = position_dodge(width = dodge_width), show.legend=FALSE)+
  theme_void()+
  coord_flip()
  

results <- data_core %>% 
  ggplot()+
  geom_point(aes(x=Study, y=bias, alpha=group), colour = "#002a60", position = position_dodge(width = dodge_width), show.legend=FALSE)+
  geom_linerange(aes(x=Study, ymin=lower, ymax=upper, alpha=group), colour = "#002a60",size=1, position = position_dodge(width = dodge_width), show.legend=FALSE)+
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
  mutate(Study = as.factor(Study)) %>% 
  ggplot(aes(x=Study, y=n))+
    geom_linerange(aes(x=Study, ymin=0, ymax=n, alpha=group), 
                   colour = "#930093", 
                   size=0.5, position = position_dodge(width = dodge_width), show.legend=FALSE)+
  geom_label(aes(x=Study, y=n, label = n, alpha=group), 
             colour="#930093",
             label.r = unit(0.4, "lines"),
             label.padding = unit(0.2, "lines"),
             size=3,
             position = position_dodge(width = dodge_width), show.legend=FALSE)+
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
            position = position_dodge(width = dodge_width), show.legend=FALSE)+
  scale_y_continuous(expand = expand_scale(mult = c(0.1, .1)))



measurements <- data_core %>% 
  mutate(id = row_number()) %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot(aes(x=Study, y=n))+
  geom_linerange(aes(x=Study, ymin=0, ymax=N, alpha=group), 
                 colour = "#930093", 
                 size=0.5, position = position_dodge(width = dodge_width), show.legend=FALSE)+
  geom_label(aes(x=Study, y=N, label = N, alpha=group), 
             colour="#930093",
             label.r = unit(0.4, "lines"),
             label.padding = unit(0.2, "lines"),
             size=3,
             position = position_dodge(width = dodge_width), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  coord_flip()+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "white", colour = "white",
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "#ffceff"),
    # axis.line.y = element_line(size = 0.5, linetype = 'solid',colour = "#930093") # vertical line at zero
  )+
  scale_x_discrete(breaks = NULL)+ #removes horizontal grid lines
  scale_y_continuous(trans='log', expand = expand_scale(mult = c(0.1, .1)))


comparison <- data_core %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comparison, alpha=group), place = "left", reflow=TRUE, 
            position = position_dodge(width = dodge_width), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")

patients <- data_core %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = patients, alpha=group), place = "left", reflow=TRUE, 
                position = position_dodge(width = dodge_width), show.legend=FALSE)+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")

comments <-  data_core %>% 
  naniar::replace_with_na(replace = list(comments = "NA")) %>%
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comments, alpha=group), place = "left", reflow=TRUE, 
                position = position_dodge(width = dodge_width), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()


RoB_icon <- data_core %>% 
  distinct(Study,.keep_all = TRUE) %>% 
  pivot_longer(cols = starts_with("RoB"), names_to = "RoB_domain", values_to = "RoB_classification") %>% 
  select(Study, RoB_domain, RoB_classification) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Zero heat flux" = "RoB_spoton")) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Comparator" = "RoB_comparator")) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Participant flow" = "RoB_flow")) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Participant selection" = "RoB_selection")) %>% 
  mutate(RoB_classification = factor(RoB_classification, 
                              levels = c("high","low","unclear"),
                              labels = c( "\uf00d","\uf00c", "\uf128"))) %>% 
  ggplot()+
  geom_point(aes(x=Study, y=0, colour=RoB_classification, alpha=RoB_domain), 
             size=2,position = position_dodge(width = dodge_width), 
             show.legend=FALSE)+
  geom_text(aes(x=Study, y=0, label= RoB_classification, alpha=RoB_domain), 
            family =  "FontAwesome", 
            size =  1.5,
            position = position_dodge(width = dodge_width),
            show.legend=FALSE)+  
  geom_text(aes(x=Study, y=0, label= RoB_domain, alpha=RoB_domain),
            position = position_dodge(width = dodge_width),
            show.legend=FALSE,
            hjust = "left",
            size=2)+
  theme_void()+
  coord_flip()+
  scale_color_manual(values=c("red", "green", "yellow"))+
  scale_alpha_discrete(range = c(rep(1,4)))

RoB_text <- data_core %>% 
  distinct(Study,.keep_all = TRUE) %>% 
  pivot_longer(cols = starts_with("RoB"), names_to = "RoB_domain", values_to = "RoB_classification") %>% 
  select(Study, RoB_domain, RoB_classification) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Zero heat flux" = "RoB_spoton")) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Comparator" = "RoB_comparator")) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Participant flow" = "RoB_flow")) %>% 
  mutate(RoB_domain = fct_recode(RoB_domain, "Participant selection" = "RoB_selection")) %>% 
  ggplot()+
  geom_text(aes(x=Study, y=0, label= RoB_domain, alpha=RoB_domain),
            position = position_dodge(width = dodge_width),
            show.legend=FALSE,
            hjust = "left",
            size=2)+
  theme_void()+
  coord_flip()+
  scale_alpha_discrete(range = c(rep(1,4)))
  

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

gt_grid <- gtable(widths = unit(c(1,0.5,0.6,0.4,0.5,1,1,1,1), 'null'), heights = unit(c(rep(1,length(unique(data_core$Study))+4)), 'null'))

gtable_show_layout(gt_grid) # use this to see layout
gt <- gt_grid %>%     
  gtable_add_grob(ggplotGrob(study), t=4,l=1, b=17) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "First author"))+
                               geom_fit_text(fontface="bold",
                                             place = "left")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=1) %>% 
  gtable_add_grob(ggplotGrob(year), t=4, l=2, b=17) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Year"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "left")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=2) %>% 
  gtable_add_grob(ggplotGrob(ma), t=18,l=9) %>% 
    gtable_add_grob(ggplotGrob(results), t=4,l=9, b=17) %>% 
    gtable_add_grob(ggplotGrob(participants), t=4,l=7, b=17) %>%
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Number of participants"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "left")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=7) %>% 
    gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                      aes(x=0,y=0, 
                                          label = "Study findings and meta-analysis"))+
                                        geom_fit_text(colour = "white",
                                                      fontface="bold")+
                                        theme_void()+
                                        theme(plot.background = element_rect(fill = "#002a60"))
                                      ), t=2,l=9) %>%
    gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                      aes(x=0,y=0, 
                                          label = "Average mean bias and population limits of agreement"))+
                                 geom_fit_text( colour = "white",
                                               fontface="bold",
                                               place = "left")+
                                 theme_void()+
                                 theme(plot.background = element_rect(fill = "#002a60"))
                              ), t=18, l=8) %>% 
  gtable_add_grob(ggplotGrob(RoB_text), t=4, l=3, b=17) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Risk of bias"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "left")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=3) %>% 
  gtable_add_grob(ggplotGrob(RoB_icon), t=4, l=4, b=17) %>% 
  # gtable_add_grob(tableGrob(legend), t=3, l=4, b=17) %>% 
  gtable_add_grob(ggplotGrob(comparison), t=4, l=5, b=17) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Comparison thermometer site"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "left",
                                 reflow = TRUE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=5) %>% 
  gtable_add_grob(ggplotGrob(comments), t=4, l=6, b=17) %>% 
  gtable_add_grob(ggplotGrob(measurements), t=4, l=8, b=17) %>% 
gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                  aes(x=0,y=0, 
                                      label = "Number of paired measurements"))+
                             geom_fit_text(
                               fontface="bold",
                               place = "left",
                               reflow = TRUE)+
                             theme_void()+
                             theme(plot.background = element_rect(fill = "white"))
), t=3,l=8)


# function to add rectangles around each study
addrectgrobs<-function(gt){
  for (i in 1:length(unique(data_core$Study))+3){ 
    gt<-gt %>% gtable_add_grob(roundrectGrob(gp=gpar(fill="transparent")),
                               t=i,l=1,r=9) 
  }
  return(gt)
}

gt <- addrectgrobs(gt)  

grid.newpage() # use newpage and grid.draw to plot the gtable

grid.draw(gt)

ggsave(plot = gt, device = "tiff", "gtable-plot.tiff", width = 420,height = 297, units = "mm",dpi=300, compression = 'lzw') #A3 size

