  library(ggplot2)
  library(tibble)
  library(tidyverse)
  library(ggfittext)
  
  line_size <- 0.50
  box_type <- "round"
  box_lines <- 0.5
  box_colour <- "white"
  box_fill <- alpha("#14c4ff", 0.3)
  text_colour <- "#ff4f14"
  arrow_colour <- "#ff4f14"
  
  
  retrieved <- 129
  duplicates <- 35
  screened <- retrieved-duplicates
  full_text <- 21
  screen_excluded <- screened-full_text
  included <- 15
  full_text_excluded <- full_text-included
  wrong_intervention <- 4
  wrong_comparator <- 2
  wrong_design <- 1
  awaiting_classification <- 2
  

# Main flow y coordinates  
  import_y_max <- 100
  import_y_min <- 90
  screen_y_max <- 80
  screen_y_min <- 70
  full_y_max <- 60
  full_y_min <- 50
  included_y_max <- 40
  included_y_min <- 30
  
# Scondary flow y coordinates  
  duplicates_y_max <- 90
  duplicates_y_min <- 80
  
  full_excluded_y_min <- 25
  full_excluded_y_max <- 45
  
  screen_excluded_y_min <- 65
  screen_excluded_y_max <- 75
  
  awaiting_classification_y_min <- 50
  awaiting_classification_y_max <- 60
  
# x coordinates  
  main_x_min <- 20
  main_x_max <- 70
  
  second_x_min <- 75
  second_x_max <- 95
  
  arrow_x <- 45
  
  ###Plot space set-up####
  data <- tibble(x= 23:100, y = 23:100)
  
  p <- data %>% 
    ggplot(aes(x, y)) +
    theme_linedraw() +
  
  
  
  ########## Main box flow ###########
  
  # import box

  annotation_custom(grob = grid::roundrectGrob(gp = grid::gpar(col = box_colour, fill = box_fill)),
    xmin = main_x_min, xmax=main_x_max, ymin=import_y_min, ymax=import_y_max)+
    geom_fit_text(xmin = main_x_min, xmax=main_x_max, ymin=import_y_min, ymax=100,
                  label= glue::glue(retrieved, ' references identified'), 
                  reflow = TRUE, colour = text_colour) +

# screen box
  annotation_custom(grob = grid::roundrectGrob(gp = 
                    grid::gpar(col = box_colour, fill = box_fill)),
                    xmin = main_x_min, xmax=main_x_max, ymin=screen_y_min, 
                    ymax=screen_y_max) +
  geom_fit_text(xmin = main_x_min, xmax=main_x_max, ymin=screen_y_min, ymax=screen_y_max,
           label= glue::glue(screened, ' titles and abstracts screened'), 
           reflow = TRUE, colour = text_colour) +
  
# full text box
  annotation_custom(grob = grid::roundrectGrob(gp = 
          grid::gpar(col = box_colour, fill = box_fill)),
          xmin = main_x_min, xmax=main_x_max, ymin=full_y_min, ymax=full_y_max) +
  geom_fit_text( xmin = main_x_min, xmax=main_x_max, ymin=full_y_min, ymax=full_y_max,
                 label= glue::glue(full_text, ' full-text articles assessed'), 
                  reflow = TRUE, colour = text_colour) +

# included box 
  annotation_custom(grob = grid::roundrectGrob(gp = 
               grid::gpar(col = box_colour, fill = box_fill)),
               xmin = main_x_min, xmax=main_x_max, 
                    ymin=included_y_min, ymax=included_y_max) +
  geom_fit_text(xmin = main_x_min, xmax=main_x_max, 
                ymin=included_y_min, ymax=included_y_max,
                label= glue::glue(included, ' studies included'), 
                 reflow = TRUE, colour = text_colour) +

######### Secondary flow boxes ############


  annotation_custom(grob = grid::roundrectGrob(gp = 
        grid::gpar(col = box_colour, fill = box_fill)),
        xmin = second_x_min, xmax=second_x_max, 
                    ymin=duplicates_y_min, ymax=duplicates_y_max) +
  geom_fit_text(xmin = second_x_min, xmax=second_x_max, 
                ymin=duplicates_y_min, ymax=duplicates_y_max,
                label= glue::glue(duplicates, ' duplicates removed'),
                 reflow = TRUE, colour = text_colour) +


# excluded box 
 annotation_custom(grob = 
                              grid::roundrectGrob(gp = 
                              grid::gpar(col = box_colour, fill = box_fill)),
                            xmin = second_x_min, xmax=second_x_max, 
                    ymin=screen_excluded_y_min, ymax=screen_excluded_y_max) +
  geom_fit_text(xmin = second_x_min, xmax=second_x_max, 
                ymin=screen_excluded_y_min, ymax=screen_excluded_y_max,
                label= glue::glue(screen_excluded, ' studies excluded'),
                 reflow = TRUE, colour = text_colour) +

# awaiting classifications box 
    annotation_custom(grob = 
                        grid::roundrectGrob(gp = 
                                              grid::gpar(col = box_colour, fill = box_fill)),
                      xmin = second_x_min, xmax=second_x_max, 
                      ymin=awaiting_classification_y_min, ymax=awaiting_classification_y_max) +
    geom_fit_text(xmin = second_x_min, xmax=second_x_max, 
                  ymin=awaiting_classification_y_min, ymax=awaiting_classification_y_max,
                  label= glue::glue(awaiting_classification, ' studies awaiting classification'),
                  reflow = TRUE, colour = text_colour) +

# full text excluded box 
annotation_custom(grob = 
                             grid::roundrectGrob(gp = 
                            grid::gpar(col = box_colour, fill = box_fill)),
                           xmin = second_x_min, xmax=second_x_max, 
                   ymin=full_excluded_y_min, ymax=full_excluded_y_max) +
 geom_fit_text(xmin = second_x_min, xmax=second_x_max,
                ymin=full_excluded_y_max-7, ymax=full_excluded_y_max,
                label= glue::glue(full_text_excluded, ' studies excluded'),
                 reflow = TRUE, colour = text_colour) +
  geom_fit_text(xmin = second_x_min, xmax=second_x_max,
                ymin=full_excluded_y_max-17, ymax=full_excluded_y_max-7,
                label= glue::glue(wrong_intervention, ' wrong intervention','\n',
                                  wrong_intervention, ' wrong comparator','\n',
                                  wrong_intervention, ' wrong design'),
                 colour = text_colour, grow = TRUE) +


####### Main flow arrows#######

  # Import - screen
 geom_segment(
  x=arrow_x, xend = arrow_x, y = import_y_min, yend = screen_y_max+0.25, 
  size = line_size,  colour = arrow_colour,
  arrow = arrow(length = unit(2, "mm"), type= "closed")) +
  
# Screen - full text
  geom_segment(
    x=arrow_x, xend=arrow_x, y=screen_y_min, yend=full_y_max+0.25, 
    size = line_size,  colour = arrow_colour,
    arrow = arrow(length = unit(2, "mm"), type= "closed")) +
  
# Full text - included
  
  geom_segment(
    x=arrow_x, xend=arrow_x, y=full_y_min, yend=included_y_max+0.25, 
    size = line_size,  colour = arrow_colour,
    arrow = arrow(length = unit(2, "mm"), type= "closed")) +

####Secondary flow arrows########    
      
# Import - duplicates  
  
  geom_curve(
    x = arrow_x, xend = second_x_min, 
    y = duplicates_y_max,  yend = duplicates_y_max-5, linetype = "dashed",
    curvature = 0.1, colour = arrow_colour) +

# Screen - excluded  
  
  geom_curve(
    x = arrow_x, xend = second_x_min, 
    y = screen_y_min,  yend = screen_excluded_y_max-5, linetype = "dashed",
    curvature = 0.1, colour = arrow_colour) +
    
# Screen - awaiting  
    
    geom_curve(
      x = arrow_x, xend = second_x_min, 
      y = screen_y_min,  yend = awaiting_classification_y_max-5, linetype = "dashed",
      curvature = -0.2, colour = arrow_colour) +

  # Full text - excluded  
  
  geom_curve(
    x = arrow_x, xend = second_x_min, 
    y = full_y_min, yend = full_excluded_y_max-10, linetype = "dashed",
    curvature = -0.2, colour = arrow_colour) +

 theme_void() +
  coord_cartesian(ylim = c(23,100))
ggsave(here::here("zhf_manuscript","figure_one.tiff"), device = "tiff", 
       width=140, units='mm', dpi=600, compression = 'lzw')

