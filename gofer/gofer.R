library(tidyverse)
library(ggfittext)
library(gridExtra)
library(grid)
library(gtable)
library(naniar)
library(ggimage)

data <- readxl::read_xlsx("~/zhf-review/manuscript/data/zhf_extracted.xlsx")
data <- data %>%
  mutate(c = N/n)

#variance

data <- data %>% 
  mutate(s2 = case_when(corrected == "Yes"  ~ ((upper - bias)/1.96)^2,
                        corrected == "No" ~  ((upper - bias)/1.96)^2*(N-1)/(N-c)
  )
  )

data <- data %>% 
  mutate(V_bias = s2/n)

data <- data %>% 
  mutate(logs2 = log(s2)+1/(n-1))

data <- data %>% 
  mutate(V_logs2 = 2/(n-1))

# meta-analyses categories

data_NPA <- data %>% 
  filter(comparison == "NPA")

data_SL <- data %>% 
  filter(comparison == "Sublingual")

#eshragi overall
data_core <- data %>% 
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter(comments != "Intraoperative, off cardiopulmonary bypass" & comments != "Postoperative")

data_core_op <- data %>% #eshragi intraop
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter (clinical_setting=="Intraoperative")

data_core_icu <- data %>% #eshragi postop
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter(clinical_setting=="ICU" | clinical_setting=="Postoperative")

# ROB 

data_core_lowrisk <- data_core %>% #eshragi overall
  filter(RoB_selection == "low" & RoB_spoton == "low" & RoB_comparator == "low" & RoB_flow =="low")

# conflict of interests

data_conflict <- data_core %>% 
  filter(`Funding/equipment/conflict with ZHF company` == "No")

format_results <- function(group){
  out <- bama::loa_maker(group$bias,group$V_bias, group$logs2, group$V_logs2)
  out <- round(out, digits=2)
  out <- out %>% 
    mutate(Participants = sum(group$n_count, na.rm=T)) %>% 
    mutate(Measurements = format(sum(group$N, na.rm = T), big.mark=",", scientific = FALSE)) %>% 
    mutate(Studies = length(unique(group$Study))) %>% 
    select(Studies, m, Participants, Measurements, bias_mean, sd2_est, tau_est, LOA_L, LOA_U, CI_L_rve, CI_U_rve) %>% 
    rename("Comparisons" = m)
  out
  
  # Better names for columns
  # , "Mean bias" = bias_mean, "Tau-squared" = tau_est, "Lower bound for 95% limit of agreement" = LOA_L, "Upper bound for 95% limits of agreement" = LOA_U, "Outer confidence interval for lower 95% limits of agreement" = CI_L_rve, "Outer confidence interval for upper 95% limits of agreement" = CI_U_rve
}



primary <- format_results(data_core)


dodge_width = 0.7

year <- data_core %>%
  distinct(Study,.keep_all = TRUE) %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(Study = as.factor(Study)) %>% 
  mutate(Year = as.numeric(Year)) %>%
  ggplot(aes(x=reorder(Study, Year), y=0,  label=Year))+
  geom_label(fill="#d4007f", colour = "white", fontface = "bold")+
  theme_void()+
  coord_flip()
year
study <- data_core %>% 
  distinct(Study,.keep_all = TRUE) %>% 
  separate(Study, c("Study", "Year"), sep = ", ")  %>% 
  mutate(Study = as.factor(Study)) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label=Study), 
                place = "left", reflow=TRUE, 
                fontface="plain",
                position = position_dodge(width = dodge_width), show.legend=FALSE)+
  theme_void()+
  coord_flip()
  

results <- data_core %>% 
  ggplot()+
  geom_point(aes(x=Study, y=bias, alpha=group, size=n), colour = "#002a60", position = position_dodge(width = dodge_width), show.legend=FALSE)+
  geom_linerange(aes(x=Study, ymin=lower, ymax=upper, alpha=group), colour = "#002a60",size=1, position = position_dodge(width = dodge_width), show.legend=FALSE)+
  geom_hline(yintercept = primary$bias_mean, linetype = 2, col = "#002a60") +
  scale_alpha_discrete(range = c(rep(1,4)))+
  coord_flip()+ 
  theme_void()+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank()
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
  mutate(comparison = recode(comparison, Eso = "Esophageal")) %>%
  mutate(comparison = recode(comparison, PA = "Pulmonary artery")) %>% 
  mutate(comparison = recode(comparison, Ax = "Axillary")) %>% 
  ggplot()+
  geom_fit_text(aes(x=Study, y=0, label = comparison, alpha=group), place = "center", reflow=TRUE, 
            position = position_dodge(width = dodge_width), show.legend=FALSE)+
  scale_alpha_discrete(range = c(rep(1,4)))+
  theme_void()+
  coord_flip()

patients <- data_core %>% 
  ggplot(aes(x=Study, y=0,label=patients))+
  geom_text(hjust="center", check_overlap = TRUE)+
  theme_void()+
  coord_flip()


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
  ggplot(aes(x=Study, y=0, label=RoB_domain, alpha=RoB_domain))+
  # geom_text(aes(x=Study, y=0, label= RoB_domain, alpha=RoB_domain),
  #           position = position_dodge(width = dodge_width),
  #           show.legend=FALSE,
  #           hjust = "left",
  #           size=2.5,
  #           fontface="italic")+
  geom_fit_text(place = "right", reflow=TRUE, 
                position = position_dodge(width = dodge_width), show.legend=FALSE)+
  theme_void()+
  coord_flip()+
  scale_alpha_discrete(range = c(rep(1,4)))
  

  ma <-  ggplot() +
    geom_polygon(aes(x = c(primary$CI_U_rve, primary$bias_mean, primary$CI_L_rve, primary$bias_mean), 
                     y = c(primary$bias_mean, 1, primary$bias_mean, -1)),
                 colour = "#002a60",
                 fill=NA,
                 size=2)+
    geom_segment(aes(x=primary$bias_mean,xend=primary$bias_mean, y=-1, yend=1),
                 colour = "#002a60",
                 fill=NA,
                 size=2)+
    theme_void()+
  theme(
    panel.background = element_rect(fill = "#BFD5E3", colour = "white",
                                    size = 2, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"), 
    panel.grid.minor = element_blank(),
    axis.text.x=element_text()
  )+
  scale_y_continuous(expand=c(0,0), limits=c(-1,1))+
  scale_x_continuous(expand=c(0.1,0.1), limits=c(min(data_core$lower),max(data_core$upper)))+
  scale_y_discrete(breaks = NULL)  #removes horizontal grid lines
  
  ma_grob <- ggplotGrob(ma)
  ma_axis <- ma_grob$grobs[[which(ma_grob$layout$name == "axis-b")]]$children$axis[2] 
  
  ma_grob$grobs[[which(ma_grob$layout$name == "axis-b")]] <- zeroGrob()
  ma_grob$heights[ma_grob$layout$t[which(ma_grob$layout$name == "axis-b")]] <- unit(0, "cm")
  
  
# Study characteristics data
demographics <- readxl::read_xlsx("~/zhf-review/gofer/study-characteristics.xlsx")

# Age  
  age <- ggplot(demographics) +
    geom_point(aes(x = Study, y = mean_age), size = 2, col = "orange red") +
    geom_linerange(aes(x = Study, ymin = lower_mean, ymax = upper_mean), size = 1, col = "orange red") +
    geom_point(aes(x = Study, y = median_age), size = 3, col = "orange", shape = "diamond") +
    geom_errorbar(aes(x = Study, ymin = lower_IQR, ymax = upper_IQR), size = 1, col = "orange", width = 0.5) +
    
    theme_void() +
    
    coord_flip() +
    
    theme(
      panel.background = element_rect(fill = "white", colour = "white",
                                      size = 2, linetype = "solid"),
      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                      colour = "#FFE5CC"), 
      panel.grid.minor = element_blank(),
      axis.text.x=element_text()
    )+
    
    scale_x_discrete(breaks = NULL)  
  
  age_grob <- ggplotGrob(age)
  age_axis <- age_grob$grobs[[which(age_grob$layout$name == "axis-b")]]$children$axis[2] 
  
  age_grob$grobs[[which(age_grob$layout$name == "axis-b")]] <- zeroGrob()
  age_grob$heights[age_grob$layout$t[which(age_grob$layout$name == "axis-b")]] <- unit(0, "cm")


  
  sex <- demographics %>% 
    pivot_longer(cols = c("female", "male"), names_to = "sex", values_to = "values") %>% 
    ggplot(aes(x = Study, y = values, fill = sex)) +
    
    geom_col(position = "fill", show.legend = FALSE,
             width = 0.3) +
    scale_fill_manual(values = c("female" = "#ff8ea2", "male" = "#8edaff"))+
  
    # geom_text(label = "\uf183", family = "FontAwesome", x = 13.25, y = -0.25, size = 7, show.legend = FALSE) +
    # geom_text(label = "\uf182", family = "FontAwesome", x = 13.25, y = 1.25, fill = "salmon", color = "salmon") +
    # 
    coord_flip() +
    
    # expand_limits(y = -0.35) +
    # expand_limits(y = 1.35) +
    
    theme_void()
  
  flags <- ggplot(demographics, aes(x = Study, y = Country)) +
    
    geom_flag(y = 0.5, aes(image = code), size = 0.2) +
    
    coord_flip() +
    
    theme_void()
  
  
 temp <- ggplot(demographics, aes(x = Study, ymin = lower_temp, ymax = upper_temp)) +
    
    geom_errorbar(size = 1, color = "#990099", width = 0.5) +
    
    coord_flip() +
 
 theme_void()+
   theme(
     panel.background = element_rect(fill = "white", colour = "white",
                                     size = 2, linetype = "solid"),
     panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                     colour = "#FFCCFF"), 
     panel.grid.minor = element_blank(),
     axis.text.x=element_text()
   )+
   
   scale_x_discrete(breaks = NULL)  
 
 temp_grob <- ggplotGrob(temp)
 temp_axis <- temp_grob$grobs[[which(temp_grob$layout$name == "axis-b")]]$children$axis[2] 
 
 temp_grob$grobs[[which(temp_grob$layout$name == "axis-b")]] <- zeroGrob()
 temp_grob$heights[temp_grob$layout$t[which(temp_grob$layout$name == "axis-b")]] <- unit(0, "cm")
 
    
# using gtable seems to be the best option - see here for documentation: https://gtable.r-lib.org/index.html
RoB_img <- png::readPNG("~/zhf-review/gofer/RoB_wide.png")
sex_img <- png::readPNG("~/zhf-review/gofer/sex.png")

gt_grid <- gtable(widths = unit(c(0.05, #left space
                                  1,# First author
                                  0.5,# Year
                                  0.5, # Country
                                  1.2, #population
                                  0.2, # space around RoB
                                  0.8,#Rob_text
                                  0.2,#Rob_icon
                                  0.2, #Space around RoB
                                  1, # Age,
                                  1, # Sex
                                  1.3,#Comparison thermometer
                                  1, #temperature range
                                  1,# Comments
                                  1.3,# Participants
                                  1.8,# Paired measurements
                                  1.5,# Results
                                  0.1, # right space for meta-analysis
                                  0.05 #Right space
                                  ), 'null'), 
                  heights = unit(c(0.1, # top outer space
                                   0.5, # section headers
                                   0.5, # Column headers
                                   rep(1,length(unique(data_core$Study))),# study rows
                                   0.5, # meta-analysis and other axis text row
                                   0.2, # meta-analysis axis text row
                                   0.1 # bottom outer space
                                   ), 'null'))

gtable_show_layout(gt_grid) # use this to see layout
gt <- gt_grid  %>%
  
  # Study design section header
  
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "\uf05a STUDY DESIGN"))+
                               geom_fit_text(family='fontawesome',
                                            colour = "white",
                                             fontface="bold",
                                             place = "center",
                                             grow = FALSE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "#d4007f"))
  ), t=2,l=2, r=5) %>% 
  # First author column
  gtable_add_grob(ggplotGrob(study), t=4,l=2, b=17) %>% 
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "First author"))+
  #                              geom_fit_text(fontface="bold",
  #                                            place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=2) %>% 
  # Year column
  gtable_add_grob(ggplotGrob(year), t=4, l=3, b=17) %>% 
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Year"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=3) %>% 
  
  # Country column
  
  gtable_add_grob(ggplotGrob(flags), t=4,l=4, b=17) %>%
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Country"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=4) %>% 
  
  # Population column
  
  gtable_add_grob(ggplotGrob(patients), t=4, l=5, b=17) %>% 
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Population"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=5) %>% 
  
  # RoB_text column
  
  gtable_add_grob(ggplotGrob(RoB_text), t=4, l=7, b=17) %>% 
  gtable_add_grob(ggplotGrob(RoB_icon), t=4, l=8, b=17) %>%
  gtable_add_grob(rasterGrob(RoB_img), t=3, l=6, r=9) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "\uf046 RISK OF BIAS"))+
                               geom_fit_text(
                                 family="fontawesome",
                                 colour = "white",
                                             fontface="bold",
                                             place = "center",
                                             grow = FALSE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "#52ae32"))
  ), t=2,l=6, r=9) %>% 
  
  # Age column
  
  gtable_add_grob(age_grob, t=4,l=10, b=17) %>%
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Age"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "center")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=10) %>% 
  gtable_add_grob(age_axis, t=18,l=10) %>% # takes axis text from ma and adds to bottom
  
  
  
  # Sex column
  
  gtable_add_grob(ggplotGrob(sex), t=4,l=11, b=17) %>%
  # gtable_add_grob(ggplotGrob(ggplot(data=NULL,
  #                                   aes(x=0,y=0, 
  #                                       label = "Sex"))+
  #                              geom_fit_text(
  #                                fontface="bold",
  #                                place = "center")+
  #                              theme_void()+
  #                              theme(plot.background = element_rect(fill = "white"))
  # ), t=3,l=11) %>% 
  
  gtable_add_grob(rasterGrob(sex_img), t=3, l=11) %>% 
  
  
  # Participants section header 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "\uf007 PARTICIPANTS"))+
                               geom_fit_text(
                                 family="fontawesome",
                                 colour = "white",
                                             fontface="bold",
                                             place = "center",
                                             grow = FALSE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "#ee7219"))
  ), t=2,l=10, r=11) %>% 
  
  # Temperature section header
  
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "\uf06d TEMPERATURE MEASUREMENTS"))+
                               geom_fit_text(colour = "white",
                                             family = "fontawesome",
                                             fontface="bold",
                                             place = "center",
                                             grow = FALSE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "#930093"))
  ), t=2,l=12, r=16) %>% 
  
  # Comparison column 
  
  gtable_add_grob(ggplotGrob(comparison), t=4, l=14, b=17) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Comparison"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "center",
                                 reflow = TRUE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=14) %>% 
  
  # temperature range column
  
  gtable_add_grob(temp_grob, t=4,l=12, b=17) %>%
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Range (°C)"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "center")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=12) %>% 
  
  gtable_add_grob(temp_axis, t=18,l=12) %>% # takes axis text from ma and adds to bottom
  
  
  # Comments column
  
  gtable_add_grob(ggplotGrob(comments), t=4, l=15, b=17) %>% 
  
  
  # Sample size 
  
  gtable_add_grob(ggplotGrob(participants), t=4,l=13, b=17) %>%
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Sample size"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "center")+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=13) %>% 
  
  # paired measurements
  
  gtable_add_grob(ggplotGrob(measurements), t=4, l=16, b=17) %>% 
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Paired measurements"))+
                               geom_fit_text(
                                 fontface="bold",
                                 place = "left",
                                 reflow = TRUE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=16) %>% 
  
  # Results section header
  
    gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                      aes(x=0,y=0, 
                                          label = "\uf1fe RESULTS"))+
                                        geom_fit_text(
                                          family='fontawesome',
                                          colour = "white",
                                                      fontface="bold",
                                                      place = "center",
                                                      grow = FALSE)+
                                        theme_void()+
                                        theme(plot.background = element_rect(fill = "#002a60"))
                                      ), t=2,l=17) %>%
    
  # Results column
  
  gtable_add_grob(ggplotGrob(results), t=4,l=17, b=17) %>% 
  
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                    aes(x=0,y=0, 
                                        label = "Difference in temperature (°C) between comparator and ZHF"))+
                               geom_fit_text(
                                 fontface="italic",
                                 place = "center",
                                 reflow = TRUE)+
                               theme_void()+
                               theme(plot.background = element_rect(fill = "white"))
  ), t=3,l=17) %>% 
  
  # Meta-analysis column
  
  gtable_add_grob(ma_grob, t=18,l=17) %>% 
  gtable_add_grob(ma_axis, t=19,l=17) %>% # takes axis text from ma and adds to bottom

  
  # Meta-analysis grob to left of results
  
  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                      aes(x=0,y=0, 
                                          label = "Pooled mean bias with population limits of agreement"))+
                                # geom_tile(fill = "#002a60")+
                                 geom_fit_text( colour = "white",
                                               fontface="bold",
                                               place = "center",
                                               reflow=TRUE)+
                                theme_void()+
                                 theme(
                                   panel.background = element_rect(fill = "#002a60", colour = "white",
                                                                   size = 2, linetype = "solid"),
                                   panel.grid.major = element_blank(), 
                                   panel.grid.minor = element_blank()
                                 )+
                                 scale_y_continuous(expand=c(0.1,0.1))
                               #   theme(plot.background = element_rect(fill = "#002a60"))
                              ), t=18, l=14, r=16) %>% 

  # Dark blue on right of meta-analysis result

  gtable_add_grob(ggplotGrob(ggplot(data=NULL,
                                  aes(x=0,y=0))+
                             theme_void()+
                             theme(
                               panel.background = element_rect(fill = "#002a60", colour = "white",
                                                               size = 2, linetype = "solid"),
                               panel.grid.major = element_blank(), 
                               panel.grid.minor = element_blank()
                             )+
                             scale_y_continuous(expand=c(0.1,0.1))
                           #   theme(plot.background = element_rect(fill = "#002a60"))
), t=18, l=18)


# function to add rectangles around each study
addrectgrobs<-function(gt){
  for (i in 1:length(unique(data_core$Study))+3){ 
    gt<-gt %>% gtable_add_grob(roundrectGrob(gp=gpar(fill="transparent", col="gray")),
                               t=i,l=2,r=18) 
  }
  return(gt)
}

gt <- addrectgrobs(gt)  

grid.newpage() # use newpage and grid.draw to plot the gtable

grid.draw(gt)

# ggsave(plot = gt, device = "tiff", "gtable-plot.tiff", width = 420,height = 297, units = "mm",dpi=300, compression = 'lzw') #A3 size

