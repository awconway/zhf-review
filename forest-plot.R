library(ggimage)
data_core$image <- sample("user.png",
                          size=19, replace = TRUE)
data_core$ruler <- sample("ruler.png",
                          size=19, replace = TRUE)


study <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_text(aes(x=id, y=0, label=Study, hjust="left"))+
  theme_void()+
  coord_flip()


results <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_point(aes(x=id, y=bias), colour = "#6D9EC1")+
  geom_linerange(aes(x=id, ymin=lower, ymax=upper), colour = "#6D9EC1", size=1)+
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
  scale_x_continuous(breaks = NULL)



participants <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
    geom_linerange(aes(x=id, ymin=0, ymax=n), colour = "#930093", size=2)+
  geom_label(aes(x=id, y=n, label = n), colour="#930093")+
  theme_void()+
  coord_flip()+
  theme_void()+
  theme(
    panel.background = element_rect(fill = "white", colour = "white",
                                    linetype = "solid"),
    panel.grid.major = element_line(size = 0.1, linetype = 'solid',
                                    colour = "#ffceff")
  )+
  scale_x_continuous(breaks = NULL)+
  geom_image(aes(x=id, y=-5, image=image), hjust="left")

comparison <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_text(aes(x=id, y=0, label = comparison), hjust="left")+
  theme_void()+
  coord_flip()

# ruler <- data_core %>% 
#   mutate(id = row_number()) %>% 
#   ggplot()+
#   geom_image(aes(x=id, y=-1, image=ruler), size=0.05)+
#   theme_void()+
#   coord_flip()
  

patients <- data_core %>% 
  mutate(id = row_number()) %>% 
  ggplot()+
  geom_text(aes(x=id, y=0, label = patients), hjust="left")+
  theme_void()+
  coord_flip()

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



combined <- gridExtra::grid.arrange(study, 
                                    participants, 
                                    #measurements,
                                    # ruler,
                                    comparison,
                                    patients,
                                    results,
                                    widths=c(5,6,2,5,7),
                                    ncol=5)
  
ggsave(plot = combined, device = "pdf", "forest-plot.pdf", width = 500, units = "mm")
