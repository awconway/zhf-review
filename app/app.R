library(shiny)
library(shinyLP)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)



# Load data ----

data <- readxl::read_xlsx(here::here("app", "data", "zhf_extracted.xlsx"), n_max = 27)
sum_findings <- readxl::read_xlsx(here::here("app", "data", "sum_findings.xlsx"))

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
  data.frame(m, bias_mean, sd2_est, tau_est, LOA_L, LOA_U, CI_L_mod, CI_U_mod, CI_L_rve, CI_U_rve)
}

#Primary meta-analysis (all included studies)
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

data_NPA <- data %>% 
  filter(comparison == "NPA")

data_SL <- data %>% 
  filter(comparison == "Sublingual")

data_core <- data %>% #eshragi overall
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter(comments != "Intraoperative, off cardiopulmonary bypass" & comments != "Postoperative")

data_core_lowrisk <- data_core %>% #eshragi overall
  filter(RoB_selection == "low" & RoB_spoton == "low" & RoB_comparator == "low" & RoB_flow =="low")

data_core_op <- data %>% #eshragi intraop
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter (clinical_setting=="Intraoperative")

data_core_icu <- data %>% #eshragi postop
  filter(comparison=="PA"|comparison=="Bladder"|comparison=="Eso" | comparison =="Rectal"|comparison =="Ax"|comparison =="Iliac") %>%
  filter(clinical_setting=="ICU" | clinical_setting=="Postoperative")

data_conflict <- data_core %>% 
  filter(`Funding/equipment/conflict with ZHF company` == "Yes")

#clean up comparison column, change to full word
data$comparison <- sub("NPA", "Nasopharyngeal", data$comparison) 
data$comparison <- sub("PA", "Pulmonary artery", data$comparison) 
data$comparison <- sub("Iliac", "Iliac artery", data$comparison) 
data$comparison <- sub("Ax", "Axillary artery", data$comparison) 
data$comparison <- sub("Eso", "Esophageal", data$comparison)

format_results <- function(group){
  out <- loa_maker(group$bias,group$V_bias, group$logs2, group$V_logs2)
  out <- out %>% 
    mutate(Participants = sum(group$n_count, na.rm=T)) %>% 
    mutate(Measurements = format(sum(group$N, na.rm = T), big.mark=",", scientific = FALSE)) %>% 
    mutate(Studies = length(unique(group$Study))) %>% 
    select(Studies, m, Participants, Measurements, bias_mean, sd2_est, tau_est, LOA_L, LOA_U, CI_L_rve, CI_U_rve) %>% 
    rename("Comparisons" = m)
}


ui <- dashboardPage(
  skin ="red",
  dashboardHeader(title="Options"),
  dashboardSidebar(
    sidebarMenu(
      br(),
      # menu item tabs
      menuItem("Home", tabName="landing_page", icon=icon("home")),
      menuItem("Search strategy", tabName="search", icon=icon("search")),
      menuItem("Results", tabName="Plot", icon=icon("chart-area")),
      menuItem("Data used in meta-analysis", tabName="DT", icon=icon("table")),
      menuItem("Extracted data", tabName="frame", icon=icon("file-excel")), 
      
      br(), 
      
      radioButtons(inputId = "dataset", 
                   h4("Select a subset of studies to display in results:"), 
                   choices= c("Core",
                              "Core (low risk studies)",
                              "Core (ICU studies)",
                              "Core (Intra-operative studies)",
                              "Nasopharyngeal",
                              "Sublingual",
                              "No conflicts of interest"))
      
    ) 
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
    # increase padding on "select display format" in side bar  
    '#select_format {
    padding-left: 15px;}',
    #resize fontin caption beneath graph
    '#caption {
    font-size: 12pt;}',
    #center and resize pLoA box content
    '#pLoA th {
    text-align: center;
    font-size: 12pt;}',
    #remove cell border in the reactive boxes to right of plot
    '#dt_grade td {
      border: none;}',
    '#dt_sc td {
      border: none;}',
    '#dt_grade td {
      border: none;}',
    '#dt_impl td {
      border: none;}'
    #custom color for header of page and side bar features
    # '.skin-red .main-header .logo {
    #   background-color: #011480;}'
    # '.skin-red .main-header .logo:hover {
    #                           background-color: #011480;}',
    # '.skin-red .main-header .logo:hover {
    #   background-color: #011480;}',
    # '.skin-red .main-header .navbar {
    #                           background-color: #011480;
    #                           }',
    # '.skin-red .main-sidebar .sidebar .sidebar-menu a:hover{
    #   background-color: #011480;}',
    # ' .skin-red .main-sidebar .sidebar .sidebar-menu .active a{
    #                           background-color: #011480;}',
    # '.skin-red .main-header .navbar .sidebar-toggle:hover{
    #   background-color: #000;
    # }'
    ))),
  

    tabItems(
      tabItem(tabName="Plot", 
             # column(12, h3(htmlOutput("title_selected_subset"))),
              fluidRow(
            column(12, 
                   h4("Pooled summary statistics:"), # title above summary stats table
                   DT::dataTableOutput("summary")), # summary = pooled summary stats table under the plot
                   plotOutput("plot"), 
                   br(),
            br(),
            br(),
            br(),
            br(),
            br(),
                   h2("This plot shows comparisons between core and zero-heat-flux thermometers within and across studies.") ,
                   h4("Blue curves are distributions of the differences between measurements from zero-heat-flux (ZHF) sensors and core temperature measurements in individual studies. The red curve is the distribution of the pooled estimate."),  # caption = caption under the plot
            
            
            # original shiny code for boxes -----
            # fluidRow( #box closed default requires boxPlus, but then we cannot put in icons
            #  column(5, gradientBox(id = "pLoA", width=12, title="Population Limits of Agreement", gradientColor = "black", icon = "fa fa-check-circle",collapsed =T, solidHeader = T, collapsible=T, footer_padding=F, DT::dataTableOutput("dt_pLoA"))),
            #  column(5, gradientBox(id = "GRADE", width=12, title="GRADE Rating of Evidence", gradientColor = "black", icon = "fa fa-sliders-h", collapsed = T, solidHeader = T, collapsible=T,  footer_padding=F, DT::dataTableOutput("dt_grade"))),
            #  column(5, gradientBox(id = "SCh",width=12, title="Study Characteristics", gradientColor = "black", icon = "fa fa-address-card",collapsed =T,  solidHeader = T, collapsible=T,  footer_padding=F, DT::dataTableOutput("dt_sc"))),
            #  column(5, gradientBox(id = "Impl", width=12, title="Clinical Implications", gradientColor = "black", icon = "fa fa-stethoscope", collapsed = T, solidHeader = T, collapsible=T,  footer_padding=F,  DT::dataTableOutput("dt_impl"))))
            
            # HTML used to modify boxes to have plus icon and collapsed as default ----
            HTML('<div class="row">
  <div class="col-sm-12">
    <div class="col-sm-12">
      <div class="box box-solid bg-black-gradient collapsed-box">
        <div class="box-header">
          <i class="fa fa-check-circle"></i>
          <h3 class="box-title">Population Limits of Agreement</h3>
          <div class="pull-right box-tools">
            <button class="btn bg-black btn-sm" data-widget="collapse" type="button">
              <i class="fa fa-plus"></i>
            </button>
          </div>
        </div>
        <div class="box-body border-radius-none" id="pLoA" collapsed="TRUE" solidHeader="TRUE">
          <div id="dt_pLoA" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
        </div>
        <div class="box-footer text-black no-padding"></div>
      </div>
    </div>
  </div>
  <div class="col-sm-12">
    <div class="col-sm-12">
      <div class="box box-solid bg-black-gradient collapsed-box">
        <div class="box-header">
          <i class="fa fa-sliders-h"></i>
          <h3 class="box-title">GRADE Rating of Evidence</h3>
          <div class="pull-right box-tools">
            <button class="btn bg-black btn-sm" data-widget="collapse" type="button">
              <i class="fa fa-plus"></i>
            </button>
          </div>
        </div>
        <div class="box-body border-radius-none" id="GRADE" collapsed="TRUE" solidHeader="TRUE">
          <div id="dt_grade" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
        </div>
        <div class="box-footer text-black no-padding"></div>
      </div>
    </div>
  </div>
  <div class="col-sm-12">
    <div class="col-sm-12">
      <div class="box box-solid bg-black-gradient collapsed-box">
        <div class="box-header">
          <i class="fa fa-address-card"></i>
          <h3 class="box-title">Study Characteristics</h3>
          <div class="pull-right box-tools">
            <button class="btn bg-black btn-sm" data-widget="collapse" type="button">
              <i class="fa fa-plus"></i>
            </button>
          </div>
        </div>
        <div class="box-body border-radius-none" id="SCh" collapsed="TRUE" solidHeader="TRUE">
          <div id="dt_sc" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
        </div>
        <div class="box-footer text-black no-padding"></div>
      </div>
    </div>
  </div>
  <div class="col-sm-12">
    <div class="col-sm-12">
      <div class="box box-solid bg-black-gradient collapsed-box">
        <div class="box-header">
          <i class="fa fa-stethoscope"></i>
          <h3 class="box-title">Clinical Implications</h3>
          <div class="pull-right box-tools">
            <button class="btn bg-black btn-sm" data-widget="collapse" type="button">
              <i class="fa fa-plus"></i>
            </button>
          </div>
        </div>
        <div class="box-body border-radius-none" id="Impl" collapsed="TRUE" solidHeader="TRUE">
          <div id="dt_impl" style="width:100%; height:auto; " class="datatables html-widget html-widget-output"></div>
        </div>
        <div class="box-footer text-black no-padding"></div>
      </div>
    </div>
  </div>
</div>
')
            )), #end of tabItem
      tabItem(tabName="DT",
              column(12,
                 h2(htmlOutput("dt_selected_subset")), # reactive title above table
                 br(), 
                 DT::dataTableOutput("dt"))),
      tabItem(tabName="search",
              fluidRow(
                boxPlus(
                  title = "Medline Search Strategy",
                  closable = FALSE,
                  status = "black",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  p("1.	zero-heat flux"),
                  p("2.	zero-flux"),
                  p("3.	1 OR 2"),
                  p("4.	(SpotON) OR (Temple Touch Pro)"),
                  p("5.	3 OR 4"),
                  p("6.	Accuracy OR precision OR reliability OR validity OR validation OR standard deviation"),
                  p("7.	Bias OR mean difference OR limit of agreement OR Bland Altman"),
                  p("8.	6 OR 7"),
                  p("9.	(exp “diagnostic errors” / OR exp “sensitivity and specificity” / OR (accura* OR reliability* OR target* OR utilt* OR discriminat* OR differentiat*)
10.	8 OR 9"),
                  p("11.	5 AND 10")
                ),
                boxPlus(
                  title = "Embase Search Strategy",
                  closable = FALSE,
                  status = "black",
                  solidHeader = FALSE,
                  collapsible = TRUE,
                  p("1.	zero-heat flux"),
                  p("2.	zero-flux"),
                  p("3.	1 OR 2"),
                  p("4.	(SpotON) OR (Temple Touch Pro)"),
                  p("5.	3 OR 4"),
                  p("6.	Accuracy OR precision OR reliability OR validity OR validation OR standard deviation"),
                  p("7.	Bias OR mean difference OR limit of agreement OR Bland Altman"),
                  p("8.	6 OR 7"),
                  p("9.	('diagnostic accuracy'/de OR 'diagnostic test accuracy study'/de OR 'diagnostic error'/exp OR 'diagnostic value'/de OR 'sensitivity and specificity'/de OR 'predictive value'/de OR (accura* OR reliabilit* OR target* OR utilit* OR discriminat* OR differentiat*)"),
                  p("11.	5 AND 10")
                )
              )),
      tabItem(tabName="frame",
              column(12, 
                     htmlOutput("frame"))),
      tabItem(tabName="landing_page",
              fluidRow(
              column(12, 
                     div(class = "jumbotron", style="background:transparent !important", 
                         h1("Accuracy of zero-heat-flux temperature monitoring"), p("A systematic review and meta-analysis"),
                         p(a(class = "btn btn-primary btn-lg button", id='tabBut', "Click here to access the full review (link will work when available)")))
                     ),
              HTML('<div class="row">
  <div class="col-sm-12">
    <div class="col-sm-12">
      <div class="box box-solid bg-black-gradient collapsed-box">
        <div class="box-header">
          <i class="fa fa-info"></i>
          <h3 class="box-title">Information</h3>
          <div class="pull-right box-tools">
            <button class="btn bg-black btn-sm" data-widget="collapse" type="button">
              <i class="fa fa-plus"></i>
            </button>
          </div>
        </div>
        <div class="box-body border-radius-none" id="pLoA" collapsed="TRUE" solidHeader="TRUE">
          <div style="width:100%; height:auto;" class="datatables html-widget html-widget-output">          
          </div>
        </div>
        <div class="box-footer text-black">
                  <p>This webpage contains supplementary information for the systematic review. In the results and data pages, users can select a subset of studies to view. We also provide source documents for the data that were extracted from the study.</p>
        </div>
      </div>
    </div>
  </div>
</div>
'),
                HTML('<div class="row">
  <div class="col-sm-12">
    <div class="col-sm-12">
      <div class="box box-solid bg-black-gradient collapsed-box">
        <div class="box-header">
          <i class="fa fa-users"></i>
          <h3 class="box-title">Authors</h3>
          <div class="pull-right box-tools">
            <button class="btn bg-black btn-sm" data-widget="collapse" type="button">
              <i class="fa fa-plus"></i>
            </button>
          </div>
        </div>
        <div class="box-body border-radius-none" id="pLoA" collapsed="TRUE" solidHeader="TRUE">
          <div style="width:100%; height:auto;" class="datatables html-widget html-widget-output">          
          </div>
        </div>
        <div class="box-footer text-black">
                  <p>Aaron Conway</p>
                  <p>Megan Bittner</p>
                  <p>Dan Phan</p>
        </div>
      </div>
    </div>
  </div>
</div>
')
              ))
  )) #end dashboardBody
) #end dashboardPage

server <- shinyUI(function(input, output) {
  
  
  
  output$frame <- renderUI({
   shinyLP::iframe(url_link = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSR9-h4cb4ONj7d5Yw9ksyEysVngocRSrMQ6vVjny-6f-d_0CZlhe3HhNG-zaQSJ6rWpBLCb5mVYcYK/pubhtml?widget=true&amp;headers=false", 
                   height=600, width="100%")
  })
  
  output$plot <- renderPlot({ 
    
    datasetInput <- reactive({
      switch(input$dataset,
             "Core" = data_core,
             "Core (low risk studies)" = data_core_lowrisk,
             "Core (ICU studies)" = data_core_icu,
             "Core (Intra-operative studies)" = data_core_op,
             "Nasopharyngeal" = data_NPA,
             "Sublingual" = data_SL,
             "No conflicts of interest" = data_conflict
             
      )
    })
    
    out <- format_results(datasetInput())


    bias=datasetInput()$bias
    s2_unb = datasetInput()$s2
    pooled_bias = out$bias_mean
    pooled_sd = out$sd2_est
    pooled_tau2 = out$tau_est
    pooled_sd = sqrt(pooled_sd^2 + pooled_tau2)
    
    LOA_l = out$LOA_L
    LOA_u = out$LOA_U
    LOA_l_CI = out$CI_L_rve
    LOA_u_CI = out$CI_U_rve
    
    g <- ggplot(data.frame(x=seq(-20,20,length=200)), aes(x=x)) + 
      stat_function(fun=dnorm, args = list(bias[1], sd=sqrt(s2_unb[1])), colour = "cornflowerblue", size=0.6, alpha = 0.5) 
    
    for (i in 2:length(bias)){
      g <- g + stat_function(fun=dnorm, args = list(bias[i], sd=sqrt(s2_unb[i])), color = "cornflowerblue", size = 0.6, alpha = 0.5)
    } 
    
    g <- g + stat_function(fun=dnorm, args = list(pooled_bias, pooled_sd), colour ="lightcoral", alpha = 0.05) + 
      stat_function(fun=dnorm, args = list(pooled_bias, pooled_sd), colour = NA, geom="area", fill="lightcoral", alpha = 0.6) + 
      scale_x_continuous(name = paste('\n Difference between', tolower(input$dataset), "and ZHF thermometry (˚C)\n"), breaks = c(-2, -1, 0, 1, 2), limits = c(-3.5,3.5)) +
      scale_y_continuous(name = "Density \n") +  labs(title = "\nOuter confidence intervals for pooled limits of agreement\n\n  Pooled limits of agreement")+
      theme(plot.title = element_text(hjust = 0.5, margin = margin(t=10, b=-32), size=10),
            axis.text=element_text(hjust = 0.5, size=10),                                                                        
            axis.title=element_text(hjust = 0.5, size=10),
            axis.line=element_line(colour="black", size=0.2),
            panel.background = element_blank(),
            axis.ticks = element_blank(), 
            plot.caption = element_text(hjust = 0)) + 
      #CI lines that extend to arrow tips
      geom_segment(aes(x=LOA_u, y=0, xend=LOA_u, yend=2.25), size = 0.3, linetype="dashed")+ 
      geom_segment(aes(x=LOA_l, y=0, xend=LOA_l, yend=2.25), size = 0.3, linetype="dashed")+
      geom_segment(aes(x=LOA_u_CI, y=0, xend=LOA_u_CI, yend=2.48), size = 0.3)+
      geom_segment(aes(x=LOA_l_CI, y=0, xend=LOA_l_CI, yend=2.48), size = 0.3)+
      
      geom_segment(aes(x=0, y=2.48, xend=LOA_u_CI, yend=2.48), size = 0.4, arrow = arrow(length = unit(0.03, "npc")))+ #R arrow high
      geom_segment(aes(x=0, y=2.48, xend=LOA_l_CI, yend=2.48), size = 0.4, arrow = arrow(length = unit(0.03, "npc")))+ #L arrow high
      geom_segment(aes(x=0, y=2.25, xend=LOA_u, yend=2.25), size = 0.4,  arrow = arrow(length = unit(0.03, "npc")))+ #R arrow low
      geom_segment(aes(x=0, y=2.25, xend=LOA_l, yend=2.25), size = 0.4,  arrow = arrow(length = unit(0.03, "npc"))) #L arrow low
    
    
    
    # summary stats table for pooled data  ----                                                                                      
    output$summary <- DT::renderDataTable({
      # add hover tags for summary table header
      out %>% 
        mutate_if(is.numeric, ~round(., 1)) %>% 
      DT::datatable(out, width = "100%",   options = list(scrollX = TRUE, paging = FALSE, searching = FALSE, info = FALSE, sort = FALSE, processing = FALSE, 
                                              #JS code to add tooltips, set container: 'body' to prevent shifting of header with mouse hover
                                              initComplete = JS("function(settings, json){
                                      $('#summary th:eq(0)').each(function(){this.setAttribute( 'title', 'Number of studies included in subset' );});
                                      $('#summary th:eq(3)').each(function(){this.setAttribute( 'title', 'Pooled estimate of mean differences calculated as comparator-ZHF in ˚C' );});
                                      $('#summary th:eq(4)').each(function(){this.setAttribute( 'title', 'Pooled standard deviation of differences' );});
                                      $('#summary th:eq(5)').each(function(){this.setAttribute( 'title', 'Variation in bias between studies' );});
                                      $('#summary th:eq(6)').each(function(){this.setAttribute( 'title', 'Lower 95% LoA calculated from pooled estimates of bias and standard deviation of differences' );});
                                      $('#summary th:eq(7)').each(function(){this.setAttribute( 'title', 'Upper 95% LoA calculated from pooled estimates of bias and standard deviation of differences' );});
                                      $('#summary th:eq(8)').each(function(){this.setAttribute( 'title', 'Model-based random-effects meta-analysis estimate of lower CI for LoA' );});
                                      $('#summary th:eq(9)').each(function(){this.setAttribute( 'title', 'Model-based random-effects meta-analysis estimate of upper CI for LoA' );});
                                      $('#summary th:eq(1)').each(function(){this.setAttribute( 'title', 'Number of participants' );});
                                      $('#summary th:eq(2)').each(function(){this.setAttribute( 'title', 'Number of paired measurements' );});
                                      $('#summary th').tooltip({container: 'body'});
                                      $(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});
                                      

                                     }")), rownames = FALSE
                    ,
                    escape =F) 
      

        
    }) # end of output$summary      
    
    # render reactive data for pooled LoA box
    output$dt_pLoA <- DT::renderDT({
    DT::datatable(out %>% 
                    mutate_if(is.numeric, ~round(., 1)) %>% 
                    select(CI_L_rve, CI_U_rve) %>% 
                    rename("Outer confidence interval for lower 95% limits of agreement" = CI_L_rve, 
                           "Outer confidence interval for upper 95% limits of agreement" = CI_U_rve), rownames = NULL, 
                  options = list(columnDefs=list(list(class="dt-center", targets=c(0,1))), dom = "t", 
                                 scrollX=T, sort = FALSE, paging=F, scrollY=T,
                                                                 initComplete = JS("function(settings, json){
                                                                                   $('#pLoA th:eq(0)').each(function(){this.setAttribute( 'title', 'Robust variance estimation meta-analysis estimate of lower CI for LoA' );});
                                                                                   $('#pLoA th:eq(1)').each(function(){this.setAttribute( 'title', 'Robust variance estimation meta-analysis estimate of upper CI for LoA' );});
                                                                                   $('#pLoA th').tooltip({container: 'body'});}")
                                 )) %>%
                                                                 formatStyle(columns =1:2, background = 'white', color = 'black')
       })
    
    
    # render reactive cell from sum_findings to match selected data set (dt_sc) 
    output$dt_sc <- DT::renderDT({
    
      sum_findings_sc <- sum_findings[,2]
      sum_findings_sccore <- sum_findings_sc[1:2,]
      sum_findings_sccorelr <- sum_findings_sc[3:4,]
      sum_findings_sccoreicu <- sum_findings_sc[5:6,]
      sum_findings_sccoreop <- sum_findings_sc[7:8,]
      sum_findings_scnpa <- sum_findings_sc[9:10,]
      sum_findings_scsl <- sum_findings_sc[11:12,]  
      
      dt_sc <- reactive({
        switch(input$dataset,
               "Core" = sum_findings_sccore,
               "Core (low risk studies)" = sum_findings_sccorelr,
               "Core (ICU studies)" = sum_findings_sccoreicu,
               "Core (Intra-operative studies)" = sum_findings_sccoreop,
               "Nasopharyngeal" = sum_findings_scnpa,
               "Sublingual" = sum_findings_scsl
        )
      })
      
      DT::datatable(dt_sc(), class = "compact", colnames = NULL, rownames = NULL, options = list(dom = "t", scrollX=T, sort = FALSE, paging=F, scrollY=T)) %>% 
        formatStyle( columns =1, background = 'white', color = 'black') 

    })
   
    # render reactive cell from sum_findings to match selected data set (dt_grade) 
    output$dt_grade <- DT::renderDT({
      
      sum_findings_grade <- sum_findings[,3]
      sum_findings_gradecore <- sum_findings_grade[1:2,]
      sum_findings_gradecorelr <- sum_findings_grade[3:4,]
      sum_findings_gradecoreicu <- sum_findings_grade[5:6,]
      sum_findings_gradecoreop <- sum_findings_grade[7:8,]
      sum_findings_gradenpa <- sum_findings_grade[9:10,]
      sum_findings_gradesl <- sum_findings_grade[11:12,]  
      
      dt_grade <- reactive({
        switch(input$dataset,
               "Core" = sum_findings_gradecore,
               "Core (low risk studies)" = sum_findings_gradecorelr,
               "Core (ICU studies)" = sum_findings_gradecoreicu,
               "Core (Intra-operative studies)" = sum_findings_gradecoreop,
               "Nasopharyngeal" = sum_findings_gradenpa,
               "Sublingual" = sum_findings_gradesl
        )
      })
      
        DT::datatable(dt_grade(), class = "compact", colnames = NULL, rownames = NULL, options = list(dom = "t", scrollX=T, sort = FALSE, paging=F, scrollY=T)) %>%
      formatStyle( columns =1, background = 'white', color = 'black')
      
      })
    
    # render reactive cell from sum_findings to match selected data set (dt_implications) 
    output$dt_impl <- DT::renderDT({
      
      sum_findings_impl <- sum_findings[,4]
      sum_findings_implcore <- sum_findings_impl[1:2,]
      sum_findings_implcorelr <- sum_findings_impl[3:4,]
      sum_findings_implcoreicu <- sum_findings_impl[5:6,]
      sum_findings_implcoreop <- sum_findings_impl[7:8,]
      sum_findings_implnpa <- sum_findings_impl[9:10,]
      sum_findings_implsl <- sum_findings_impl[11:12,]  
      
      dt_impl <- reactive({
        switch(input$dataset,
               "Core" = sum_findings_implcore,
               "Core (low risk studies)" = sum_findings_implcorelr,
               "Core (ICU studies)" = sum_findings_implcoreicu,
               "Core (Intra-operative studies)" = sum_findings_implcoreop,
               "Nasopharyngeal" = sum_findings_implnpa,
               "Sublingual" = sum_findings_implsl)
      })
      
          DT::datatable(dt_impl(), class = "compact", colnames = NULL, rownames = NULL, options = list(dom = "t", scrollX=T, sort = FALSE, paging=F, scrollY=T)) %>%
      formatStyle( columns =1, background = 'white', color = 'black')
    })
  
    
    
    # render reactive table (dt)
    output$dt<- DT::renderDataTable({
      datasetInput<-datasetInput()[,c(1,3:18)]
      
      sketch = htmltools::withTags(table(
        class = 'display',
        thead(
          tr( #change the name of column header, 'title =' will be displayed as tooltip
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
            th('RoB flow', title = 'Risk of bias for patient flow'),
            th('Competing interest', title = 'Funding, equipment or conflict with supplying company')
            
          )
        )
      ))
      DT::datatable(datasetInput, container = sketch, extensions = "Buttons", options = list(dom = "Bt", scrollX=T, buttons = c('copy', 'csv', 'excel'), sort = FALSE, paging=F, scrollY=T,
                                                                                             #use 'title' previously noted in column header as tooltip
                                                                                             initComplete = JS("function(settings, json){",
                                                                                                               "$('th').tooltip({container: 'body'});
                                                                       $(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});
                                                                       }")))
      
      
    }) # end of render output$dt
    
    g # g (reactive plot) must be the last object mentioned inside curly bracket to show up in app
  
}) # end of render output$plot ("g")

output$select_format <- renderUI({HTML('Select display format:')})

# reactive title for plot tab
# output$title_selected_subset <- renderUI({HTML(paste("Comparison between", tolower(input$dataset), "and zero-heat-flux thermometry"))})

# reactive title for dt tab
output$dt_selected_subset <- renderUI({HTML(paste("Comparison between", tolower(input$dataset), "and zero-heat-flux thermometry"))})

# caption under graph
#output$caption <- renderUI({HTML(paste("Blue curves are distributions of the differences between measurements from zero-heat-flux (ZHF) sensors and", tolower(input$dataset),
#                                        "thermometers in individual studies. Solid curve filled with red is the distribution of the pooled estimate of the difference between", tolower(input$dataset),
#                                        "and ZHF thermometry. Dotted vertical lines indicate bounds for the pooled estimates for limits of agreement between", tolower(input$dataset), 
#                                        "and ZHF thermometry. Solid vertical lines indicate bounds for the outer 95% confidence intervals (CIs) for the pooled 
#                                          estimates of limits of agreement (LoA) between", tolower(input$dataset), "and ZHF thermometry (ie. population LoA)."))
# #}) # end of output$caption


}) # end of server

shinyApp(ui, server)