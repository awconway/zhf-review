library(shiny)
library(tidyverse)
library(ggplot2)
library(DT)



# Load data ----

data <- readxl::read_xlsx("zhf_extracted.xlsx")

# Functions for calculating the Bland-Altman meta-analysis ----

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
  c(m, bias_mean,sqrt(sd2_est), tau_est, LOA_L, LOA_U, CI_L_mod, CI_U_mod, CI_L_rve, CI_U_rve)}

#Primary meta-analysis (all included studies)
data <- data %>%
  mutate(c = N/n)

#variance
data <-  if (data$corrected == "Yes"){
  mutate(data, s2 = ((upper - bias)/1.96)^2)
} else {
  mutate(data, s2 = ((upper - bias)/1.96)^2)*((N-1)/(N-c))
}

data <- data %>% 
  mutate(V_bias = s2/n)

data <- data %>% 
  mutate(logs2 = log(s2)+1/(n-1))

data <- data %>% 
  mutate(V_logs2 = 2/(n-1))
  
data_NPA <- data %>% 
  filter(comparison == "NPA")

data_SL <- data %>% 
  filter(comparison == "Sublingual")

data_core <- data %>% 
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac")

data_core_lowrisk <- data_core %>% 
  filter((RoB_selection == "low" & RoB_spoton == "low" & RoB_comparator == "low" & RoB_flow =="low") & clinical_setting != "Intra- and Postoperative")

data_core_op <- data_core %>% 
  filter(clinical_setting=="Intraoperative")

data_core_icu <- data_core %>% 
  filter(clinical_setting=="ICU" | clinical_setting =="Postoperative")

#clean up comparison
data$comparison <- sub("NPA", "Nasopharyngeal", data$comparison) 
data$comparison <- sub("PA", "Pulmonary artery", data$comparison) 
data$comparison <- sub("Iliac", "Iliac artery", data$comparison) 
data$comparison <- sub("Ax", "Axillary artery", data$comparison) 
data$comparison <- sub("Eso", "Esophageal", data$comparison)

# Shiny Panel ----

ui <- shinyUI(fluidPage( 
  
  sidebarPanel(
    fluidRow(
      radioButtons(inputId = "dataset", 
                   h4("Select a subset of studies to display:"), 
                   choices= c("Core",
                              "Core (low risk studies)",
                              "Core (ICU only)",
                              "Core (intraoperative only)",
                              "Nasopharyngeal",
                              "Sublingual"))

      
      
    ),width=3),
  
  mainPanel(fluidRow(
    #tabs for plot and dataframe       
    tabsetPanel(
      tabPanel("Plot", 
               plotOutput("plot"),
               htmlOutput("caption"),
               htmlOutput("selected_subset"),
               DT::dataTableOutput("summary"),br()),
      tabPanel("Dataframe",
               DT::dataTableOutput("df"))),
    width=9)
  ))
)
# Define server logic ----


server <- shinyUI(function(input, output) {
  
  
  
  
  output$plot <- renderPlot({ 
    
    datasetInput <- reactive({
      switch(input$dataset,
             "Core" = data_core,
             "Core (low risk studies)" = data_core_lowrisk,
             "Core (ICU only)" = data_core_icu,
             "Core (intraoperative only)" = data_core_op,
             "Nasopharyngeal" = data_NPA,
             "Sublingual" = data_SL
             )
    })
    
    out <- loa_maker(datasetInput()$bias,datasetInput()$V_bias,datasetInput()$logs2,datasetInput()$V_logs2)
    out <- round(out, digits=2)
    out <- append(out, sum(datasetInput()$n_count, na.rm = T))
    out <- append(out, sum(datasetInput()$N, na.rm = T))
    names(out) <- c("Studies","Bias","SD","τ²","LoAᴸ","LoAᵁ","mCIᴸ","mCIᵁ","rCIᴸ","rCIᵁ", "n", "N")
    out[1:12]
    
    bias=datasetInput()$bias
    s2_unb = datasetInput()$s2
    pooled_bias = out[2]
    pooled_sd = out[3]
    pooled_tau2 = out[4]
    pooled_sd = sqrt(pooled_sd^2 + pooled_tau2)
    
    LOA_l = out[5]
    LOA_u = out[6]
    LOA_l_CI = out[9]
    LOA_u_CI = out[10]
    
    g <- ggplot(data.frame(x=seq(-20,20,length=200)), aes(x=x)) + 
      stat_function(fun=dnorm, args = list(bias[1], sd=sqrt(s2_unb[1])), colour = "cornflowerblue", size=0.6, alpha = 0.5) 
    
    for (i in 2:length(bias)){
      g <- g + stat_function(fun=dnorm, args = list(bias[i], sd=sqrt(s2_unb[i])), color = "cornflowerblue", size = 0.6, alpha = 0.5)
    } 
    
    g <- g + stat_function(fun=dnorm, args = list(pooled_bias, pooled_sd), colour ="lightcoral", alpha = 0.05) + 
      stat_function(fun=dnorm, args = list(pooled_bias, pooled_sd), colour = NA, geom="area", fill="lightcoral", alpha = 0.6) + 
      scale_x_continuous(name = paste('\n Difference between', tolower(input$dataset), "thermometers and ZHF (˚C)\n"), limits = c(-3.5,3.5)) +
      scale_y_continuous(name = "Density \n", limits = c(0,2.5)) +  labs(title = "Outer confidence intervals for pooled limits of agreement\n\nPooled limits of agreement")+
      theme(plot.title = element_text(hjust = 0.5, margin = margin(t=10, b=-37)),
            #plot.subtitle = element_text(hjust=0.5, margin=margin(t=0, b=-40)),
            axis.text=element_text(hjust = 0.5, size=14),                                                                        
            axis.title=element_text(hjust = 0.5, size=14),
            axis.line=element_line(colour="black", size=0.2),
            panel.background = element_blank(),
            axis.ticks = element_blank(), 
            plot.caption = element_text(hjust = 0)) + 
      #CI lines that extend to arrow tips
      geom_segment(aes(x=LOA_u, y=0, xend=LOA_u, yend=2.25), size = 0.3, linetype="dashed")+ 
      geom_segment(aes(x=LOA_l, y=0, xend=LOA_l, yend=2.25), size = 0.3, linetype="dashed")+
      geom_segment(aes(x=LOA_u_CI, y=0, xend=LOA_u_CI, yend=2.5), size = 0.3)+
      geom_segment(aes(x=LOA_l_CI, y=0, xend=LOA_l_CI, yend=2.5), size = 0.3)+
      #geom_hline(yintercept = 0, size=0.7, color="white") + # to get rid of bottom of geom
      geom_segment(aes(x=0, y=2.25, xend=LOA_u, yend=2.25), size = 0.4,  arrow = arrow(length = unit(0.03, "npc")))+ #R arrow low
      geom_segment(aes(x=0, y=2.5, xend=LOA_u_CI, yend=2.5), size = 0.4, arrow = arrow(length = unit(0.03, "npc")))+ #R arrow high
      geom_segment(aes(x=0, y=2.5, xend=LOA_l_CI, yend=2.5), size = 0.4, arrow = arrow(length = unit(0.03, "npc")))+ #L arrow high
      geom_segment(aes(x=0, y=2.25, xend=LOA_l, yend=2.25), size = 0.4,  arrow = arrow(length = unit(0.03, "npc"))) #L arrow low
    
    
    
    # summary stats for pooled data                                                                                        
    output$summary <- DT::renderDataTable ({
      # add hover tags for summary table header
      datatable(t(out[1:12]),  options = list(paging = FALSE, searching = FALSE, info = FALSE, sort = FALSE, processing = FALSE,
                                              
                                              #JS code to add tooltips, set container: 'body' to prevent shifting of header with mouse hover
                                              initComplete = JS("function(settings, json){
                                      $('th:eq(0)').each(function(){this.setAttribute( 'title', 'Number of studies included in subset' );});
                                      $('th:eq(1)').each(function(){this.setAttribute( 'title', 'Pooled estimate of mean differences calculated as comparator-ZHF in ˚C' );});
                                      $('th:eq(2)').each(function(){this.setAttribute( 'title', 'Pooled standard deviation of differences' );});
                                      $('th:eq(3)').each(function(){this.setAttribute( 'title', 'Variation in bias between studies' );});
                                      $('th:eq(4)').each(function(){this.setAttribute( 'title', 'Lower 95% LoA calculated from pooled estimates of bias and standard deviation of differences' );});
                                      $('th:eq(5)').each(function(){this.setAttribute( 'title', 'Upper 95% LoA calculated from pooled estimates of bias and standard deviation of differences' );});
                                      $('th:eq(6)').each(function(){this.setAttribute( 'title', 'Model-based random-effects meta-analysis estimate of lower CI for LoA' );});
                                      $('th:eq(7)').each(function(){this.setAttribute( 'title', 'Model-based random-effects meta-analysis estimate of upper CI for LoA' );});
                                      $('th:eq(8)').each(function(){this.setAttribute( 'title', 'Robust variance estimation meta-analysis estimate of lower CI for LoA' );});
                                      $('th:eq(9)').each(function(){this.setAttribute( 'title', 'Robust variance estimation meta-analysis estimate of upper CI for LoA' );});
                                      $('th:eq(10)').each(function(){this.setAttribute( 'title', 'Number of participants' );});
                                      $('th:eq(11)').each(function(){this.setAttribute( 'title', 'Number of paired measurements' );});
                                      $('th').tooltip({container: 'body', position: 'bottom'})

                                     }")), escape =F)
    }) # end of output$summary
    
    # render reactive table (df)
    output$df <- DT::renderDataTable({
      datasetInput<-datasetInput()[,c(1,3:17)]

      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            th('', title = ''),
            th('Study', title = 'Author, year'),
            th('n', title = 'Number of participants'),
            th('N', title = 'Number of paired measurements'),
            th('Mean bias', title = 'Mean differences calculated as comparator-ZHF in ˚C'),
            th('LoAᴸ', title = 'Lower 95% limit of agreement'),
            th('LoAᵁ', title = 'Upper 95% limit of agreement'),
            th('Corrected', title = 'Data corrected for repeated measures'),
            th('Comparator', title = 'Location of comparator thermometer'),
            th('Multiple', title = 'Multiple measures taken for each participant'),
            th('Patient population', title = 'Patient population'),
            th('Comments', title = 'Comments'),
            th('Clinical setting', title = 'Clinical setting'),
            th('RoB patient selection', title = 'Risk of bias for patient selection'),
            th('RoB ZHF', title = 'Risk of bias for interpretation of ZHF'),
            th('RoB comparator', title = 'Risk of bias for interpretation of comparator device'),
            th('RoB flow', title = 'Risk of bias for patient flow')
            
          )
        )
      ))
      DT::datatable(datasetInput, container = sketch, extensions = "Buttons", options = list(dom = "Bt", scrollX=T, buttons = c('copy', 'csv', 'excel'), sort = FALSE, paging=F, scrollY=T,
                                                                     #use 'title' previously noted in column header as tooltip
                                                                     initComplete = JS("function(settings, json){",
                                                                       "$('th').tooltip({container: 'body'});
                                                                       $(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});
                                                                       }")))
      # 
      # extensions = 'Buttons', 
      # options = list(paging=TRUE,
      #                dom = 'Bfrtip',
      #                scrollX=TRUE,
      #                pageLength=8,
      #                searching = FALSE, 
      #                info = FALSE, 
      #                sort = FALSE,
      #                buttons = c('copy', 'csv', 'excel'),
                                              
        
    }) # end of render output$df
    
    g # g (reactive plot) must be the last object mentioned inside curly bracket to show up in app
    
    
  }) # end of render output$plot ("g")
  
  
  
  # reactive title for summary stats table
  output$selected_subset <- renderUI({HTML(paste("<h4>Pooled summary stats for", tolower(input$dataset), "thermometer comparisons"))})
  
  # caption under graph
  output$caption <- renderUI({HTML(paste("Blue curves are distributions of the differences between measurements from zero-heat-flux (ZHF) sensors and", tolower(input$dataset),
                                         "thermometers in individual studies. Solid curve filled with purple is the distribution of the pooled estimate of the difference between", tolower(input$dataset),
                                         "thermometers and ZHF. Dotted vertical lines indicate bounds for the pooled estimates for limits of agreement between", tolower(input$dataset), 
                                         "thermometers and ZHF. Solid vertical lines indicate bounds for the outer 95% confidence intervals (CIs) for the pooled 
                                         estimates of limits of agreement (LoA) between", tolower(input$dataset), "thermometers and ZHF (ie. population LoA)."))
    }) # end of output$caption
  
  }) # end of server

# Run the application 
shinyApp(ui = ui, server = server)

