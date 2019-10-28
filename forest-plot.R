# library(ggimage)
# data_core$image <- sample("user.png",
#                           size=19, replace = TRUE)
# data_core$ruler <- sample("ruler.png",
#                           size=19, replace = TRUE)


library(ggfittext)

year <- data_core %>%
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Year, hjust="left"), place = "left", reflow=TRUE, grow = FALSE,
                 position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()

study <- data_core %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(id = row_number()) %>%  
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Study, hjust="left"), place = "left", reflow=TRUE, grow = FALSE,
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()


results <- data_core %>% 
  mutate(id = row_number()) %>%
  ggplot(aes(x=Study, y=bias))+
  geom_point(aes(x=Study, y=bias, colour=group), position = position_dodge(width = 0.99), show.legend=FALSE)+
  geom_linerange(aes(x=Study, ymin=lower, ymax=upper, colour=group), size=1, position = position_dodge(width = 0.99), show.legend=FALSE)+
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
  scale_color_brewer(palette="Dark2")

participants <- data_core %>% 
  mutate(id = row_number()) %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot(aes(x=Study, y=n))+
    geom_linerange(aes(x=Study, ymin=0, ymax=n, colour=group), 
                   # colour = "#930093", 
                   size=2, position = position_dodge(width = 0.99), show.legend=FALSE)+
  geom_label(aes(x=Study, y=n, label = n, colour=group), 
             # colour="#930093", 
             position = position_dodge(width = 0.99), show.legend=FALSE)+
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
  geom_text(aes( x=Study, y=-5, colour=group), 
            label =  "\uf0c0", 
            family =  "FontAwesome", 
            size =  3.5,
            # colour = "#930093",
            vjust="center", position = position_dodge(width = 0.99), show.legend=FALSE)+
  scale_color_brewer(palette="Dark2")+
  scale_y_continuous(expand = expand_scale(mult = c(0.1, .1)))


comparison <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comparison, colour=group), place = "left", reflow=TRUE, grow = FALSE,
            position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")
# ruler <- data_core %>% 
#   mutate(id = row_number()) %>% 
#   ggplot()+
#   geom_image(aes(x=id, y=-1, image=ruler), size=0.05)+
#   theme_void()+
#   coord_flip()
  

patients <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = patients, colour=group), place = "left", reflow=TRUE, grow = FALSE,
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")

comments <-  data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comments, colour=group), place = "left", reflow=TRUE, grow = FALSE,
                position = position_dodge(width = 0.99), show.legend=FALSE)+
  # geom_text(aes(x=Study, y=0, label = comments, colour=group), hjust="left",
  #           position = position_dodge(width = 0.99), show.legend=FALSE)+
  theme_void()+
  coord_flip()+
  scale_color_brewer(palette="Dark2")



# measurements <- data_core %>% 
#   mutate(id = row_number()) %>% 
#   ggplot()+
#   geom_linerange(aes(x=id, ymin=0, ymax=N))+
#   geom_label(aes(x=id, y=N, label = N), colour = "#930093", size=2)+
#   theme_void()+
#   coord_flip()+
#   theme(
#     panel.background = element_rect(fill = "white", colour = "white",
#                                     linetype = "solid"),
#     panel.grid.major = element_line(size = 0.1, linetype = 'solid',
#                                     colour = "#ffceff")
#   )+
#   scale_x_continuous(breaks = NULL)

####

combined <- gridExtra::grid.arrange(study,
                                    year,
                                    participants, 
                                    #measurements,
                                    # ruler,
                                    comparison,
                                    patients,
                                    comments,
                                    results,
                                    ncol=7)

  
ggsave(plot = combined, device = "pdf", "forest-plot.pdf", width = 500, units = "mm")
