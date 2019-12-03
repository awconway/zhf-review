library(shiny)
library(shinyLP)
library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(DT)
library(robvis)
library(ggprisma) #awconway/ggprisma
library(waiter)
library(gofer)#awconway/gofer
library(ggimage)

######UI#########
ui <- dashboardPage(
  skin ="red",
  dashboardHeader(title="Options"),
  dashboardSidebar(
    sidebarMenu(
      br(),
      # menu item tabs
      menuItem("Home", tabName="landing_page", icon=icon("home")),
      menuItem("Search strategy", tabName="search", icon=icon("search")),
      menuItem("Study selection", tabName="selection", icon=icon("hand-pointer")),
      menuItem("Risk of bias", tabName = "Rob", icon = icon("exclamation-circle")),
      menuItem("Graphical overview of findings", tabName="gofer", icon=icon("chart-area")), 
      menuItem("Data used in meta-analysis", tabName="DT", icon=icon("table")), 
      #menuItem("Meta-analysis results", tabName="Plot", icon=icon("chart-area")),
      menuItem("All data extracted from source", tabName="frame", icon=icon("file-excel")),
      

      br(),

      radioButtons(inputId = "dataset",
                   h4("Select a subset of studies to display:"),
                   inline = FALSE,
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
    
    
    tabItems(
      tabItem(tabName="Rob",
              fluidRow(
                tabBox(width = 12,
                       tabPanel(p("Risk of bias across studies"),
                                plotOutput("Rob_summary")
                       ),
                       tabPanel(p("Risk of bias in individual studies"),
                                plotOutput("Rob_traffic_light")
                       )
                )
              )
      ),
      
      tabItem(tabName = "gofer",
              fillPage(    tags$style(type = "text/css", "#gofer {height: calc(100vh - 80px) !important;}"),
                           use_waitress(),
                           plotOutput("gofer", height="100%")
                              )
              
      ),
      
      
      tabItem(tabName = "selection",
              fillPage(      tags$style(type = "text/css", "#ggprisma {height: calc(100vh - 80px) !important;}"),
                             
                             plotOutput("ggprisma", height = "100%")
              )
              
      ),
      
      tabItem(tabName="DT",
              column(12,
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
              fillPage(tags$style(type = "text/css", "#frame {height: calc(100vh - 80px) !important;}"),
                     htmlOutput("frame"))),
      
      tabItem(tabName="landing_page",
              fluidRow(
                column(12, 
                       div(class = "jumbotron", style="background:transparent !important", 
                           h1("Accuracy of zero-heat-flux temperature monitoring"), p("A systematic review and meta-analysis"),
                           p(a(class = "btn btn-primary btn-lg button", id='tabBut', "Click here to access the full review")))
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

 <p><a href="https://www.aaronconway.info"> Aaron Conway </a> (Peter Munk Cardiac Centre, UHN &amp; Lawrence S. Bloomberg Faculty of Nursing, University of Toronto)</p>
  
<p>Megan Bittner  (Lawrence S. Bloomberg Faculty of Nursing, University of Toronto)
  
<p>Dan Phan  (Lawrence S. Bloomberg Faculty of Nursing, University of Toronto)
  
<p>Navpreet Kamboj  (Lawrence S. Bloomberg Faculty of Nursing, University of Toronto)
  
<p>Kristina Chang  (Toronto General Hospital, UHN)
  
<p>Peter Collins  (Toronto General Hospital, UHN)

<p>Elizabeth Tipton (Dapartment of Statistics, Northwestern University)
  
<p>Matteo Parotto  (Toronto General Hospital, UHN)


        </div>
      </div>
    </div>
  </div>
</div>
')
              ))
    )  
  ) #end dashboardBody
) #end dashboardPage

server <- shinyUI(function(input, output) {
  Sys.sleep(3) # do something that takes time
  
  output$Rob_summary <- renderPlot({
    
    
    RoB <- data_core %>%
      select(Study, RoB_selection, RoB_spoton, RoB_comparator, RoB_flow) %>%
      mutate(RoB_overall = if_else(RoB_selection == "low" &
                                     RoB_spoton == "low" &
                                     RoB_comparator == "low" &
                                     RoB_flow == "low", "low", "high"))
    
    RoB[RoB == "unclear"] <- "some concerns"
    
    rob_summary(data = RoB, tool = "QUADAS-2", weighted = FALSE)
  })
  
  output$Rob_traffic_light <- renderPlot({
    
    
    RoB <- data_core %>%
      select(Study, RoB_selection, RoB_spoton, RoB_comparator, RoB_flow) %>%
      mutate(RoB_overall = if_else(RoB_selection == "low" &
                                     RoB_spoton == "low" &
                                     RoB_comparator == "low" &
                                     RoB_flow == "low", "low", "high"))
    
    RoB[RoB == "unclear"] <- "some concerns"
    
    rob_traffic_light(data = RoB, tool = "QUADAS-2")
  })
  
  output$frame <- renderUI({
    shinyLP::iframe(url_link = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSR9-h4cb4ONj7d5Yw9ksyEysVngocRSrMQ6vVjny-6f-d_0CZlhe3HhNG-zaQSJ6rWpBLCb5mVYcYK/pubhtml?widget=true&amp;headers=false", 
                    height="100%", width="100%")
  })
  
  #   output$plot <- renderPlot({ 
  #     
  datasetInput <- reactive({
    switch(input$dataset,
           "Core" = data_core,
           "Core (low risk studies)" = data_core_low_risk,
           "Core (ICU studies)" = data_core_ICU,
           "Core (Intra-operative studies)" = data_core_OT,
           "Nasopharyngeal" = data_NPA,
           "Sublingual" = data_SL,
           "No conflicts of interest" = data_conflict
           
    )
  })

  output$dt<- DT::renderDataTable({
    datasetInput<-datasetInput()
    
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
    DT::datatable(datasetInput[,c(1,4:19)], container = sketch, extensions = "Buttons", options = list(dom = "Bt", scrollX=T, buttons = c('copy', 'csv', 'excel'), sort = FALSE, paging=F, scrollY=T,
                                                                                                       #use 'title' previously noted in column header as tooltip
                                                                                                       initComplete = JS("function(settings, json){",
                                                                                                                         "$('th').tooltip({container: 'body'});
                                                                       $(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});
                                                                       }")))
    
    
  }) # end of render output$dt

  
  gofer_input <- reactive({
    switch(input$dataset,
           "Core" = list(data_core, ma_core, data_core_age, "Moderate"),
           "Core (low risk studies)" = list(data_core_low_risk, ma_low_risk, data_core_low_risk_age, "Moderate"),
           "Core (ICU studies)" = list(data_core_ICU, ma_ICU, data_core_ICU_age, "Low"),
           "Core (Intra-operative studies)" = list(data_core_OT, ma_OT, data_core_OT_age, "Moderate"),
           "Nasopharyngeal" = list(data_NPA, ma_NPA, data_NPA_age, "Low"),
           "Sublingual" = list(data_SL, ma_SL, data_SL_age, "Very low"),
           "No conflicts of interest" = list(data_conflict, ma_conflict, data_no_conflict_age, "Low")
           
    )
  })
  waitress <- call_waitress("nav", theme = "overlay-percent") # call the waitress
  
  output$gofer <- renderPlot({
    waitress$start()
    
    
    dat <- vector()
    
    for(i in 1:10){
      waitress$increase(10) # increase by 10%
      Sys.sleep(.3)
      dat <- c(dat, sample(1:100, 1))
    }
    
    plot <- gofer::gofer(gofer_input()[[1]], ma_effect = gofer_input()[[2]]$effect_estimate,
                                   ma_lower = gofer_input()[[2]]$lower_limit, ma_upper = gofer_input()[[2]]$upper_limit,
                                   grade_rating=gofer_input()[[4]], data_age = gofer_input()[[3]], 
                                   dodge_width = 0.85)
        
    
    grid::grid.draw(plot)
    
    waitress$hide() # hide when done
     
                                  }) # renderPlot
  
output$ggprisma <- renderPlot({
  ggprisma::ggprisma(retrieved = 130, included = 16, duplicates = 35, 
                     full_text = 22, wrong_intervention = 4, wrong_comparator = 2,
                     wrong_design = 1, awaiting_classification = 2)
})  
  



}) # end of server

shinyApp(ui, server)