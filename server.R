# source("http://news.mrdwab.com/install_github.R")
# if("koboloadeR" %in% rownames(installed.packages()) == FALSE) 
#   {install_github("mrdwab/koboloadeR")}
# 
# devtools::install_github("mrdwab/koboloadeR")
# devtools::install_github("dfalster/5589956")

library(koboloadeR)
library(dplyr)
library(shinythemes)
library(shiny)
library(DT)
library(data.table)
library(shinyjs)
library(htmltools)
library(highcharter)
library(shinyURL)
library(shinythemes)
library(shiny)
library(DT)
library(shinyjs)
library(htmltools)
library(shinydashboard)
library(highcharter)
library(RColorBrewer)

credentials <- list("sirsiya" = "aaf4fd3dbc95bef46f24f8631b320a12")

shinyServer (function(input, output, session) {
#####################Password code below  
  shinyURL.server()
  
  USER <- reactiveValues(Logged = FALSE)
  
  observeEvent(input$.login, {
    if (isTRUE(credentials[[input$.username]]==input$.password)){
      USER$Logged <- TRUE
    } else {
      show("message")
      output$message = renderText("Invalid user name or password")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
output$app = renderUI(
    if (!isTRUE(USER$Logged)) {
      # fluidPage(
      
      fluidRow(
 tags$style("body {background: url(nhm.jpg) 
    no-repeat center center fixed; 
   background-size: cover;}"),
      
      column(width=4, offset = 4,
                      wellPanel(id = "login",
                                textInput(".username", "Username:"),
                                passwordInput(".password", "Password:"),
                                div(actionButton(".login", "Log in"), style="text-align: center;")
                      ),
                      textOutput("message")
      ))
      
      # )
      
      
    } else {
      
      # For dropdown menu
      actionLink <- function(inputId, ...) {
        tags$a(href='javascript:void',
               id=inputId,
               class='action-button',
               ...)
      }
      
      
    
      ################################################################################################################
      ################ Suppress errors in shiny web interface, dont use during development############################
      ################################################################################################################
      # options(shiny.error = function() {
      #   stop("")
      # })
      ################################################################################################################
      ################################################################################################################
      
      fluidPage(
        list(tags$head(HTML('<link rel="icon", href="my.ng",
                            type="image/png" />'))),
        div(style="padding: 1px 0px; width: '100%'",
            titlePanel(
              title="", windowTitle="My Window Title"
            )
        ))
      
      
      
      # PAGE_TITLE <- "My great title"
      navbarPage( header = "",
                  title=div(img(src="nhm-logo.png"), "Health & Wellness Centre"), 
                  theme = shinytheme("spacelab"),inverse = FALSE ,windowTitle = "UP NHM", selected = "Summary",
                  
                  tags$head(tags$style(type="text/css", "
                                       #loadmessage {
                                       position: fixed;
                                       top: 50px;
                                       left: 0px;
                                       width: 100%;
                                       padding: 5px 0px 5px 0px;
                                       text-align: center;
                                       font-weight: bold;
                                       font-size: 100%;
                                       color: #000000;
                                       background-color: #CCFF66;
                                       z-index: 105;
    }
                                       ")),
                  
                  
                  includeCSS(path = "AdminLTE.css"),includeCSS(path = "shinydashboard.css"),
                  
                  # fluidRow(
                  #   column(9,offset = 3,
                  #   # infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 3),
                  #   infoBox("New Orders1", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4),
                  #   infoBox("New Orders11", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4),
                  #   infoBox("New Orders111", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4)
                  #   # infoBoxOutput("progressBox2"),
                  #   # infoBoxOutput("approvalBox2")
                  #   )
                  # ),
                  # tags$hr(),
                  
                  # 
                  # tabPanel("Monitoring",
                  #          verbatimTextOutput("summary")
                  # ),
                  
                  navbarMenu("Monitoring",
                             tabPanel( "Summary",
                                       
                                       fluidRow(
                                         
                                         column(12,offset = 0,
                                                # infoBox("New Orders",  icon = icon("credit-card"), fill = TRUE, width = 3),
                                                infoBoxOutput("subCentres", width = 3),
                                                infoBoxOutput("scAshas",width = 3),
                                                infoBoxOutput("totalHousehold",width = 3),
                                                infoBoxOutput("totalMembers",width = 3)
                                                # infoBox("Total Households", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4),
                                                # infoBox("Total Members", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4)
                                                # infoBoxOutput("progressBox2"),
                                                # infoBoxOutput("approvalBox2")
                                         )
                                       ),
                                       tags$hr(),
                                       
                                       fluidRow( 
                                         column(3,
                                                wellPanel(
                                                  
                                                  # h2("Click to refresh database"),
                                                  useShinyjs(),
                                                  
                                                  tags$b(helpText("Last Update:")),
                                                  tags$hr(),
                                                  h4(textOutput("dttime"),style="color: #B90321"),
                                                  tags$hr(),
                                                  # h6(helpText(tags$b("Note:"),tags$em("Please click the below UPDATE button, only if you want to see information after
                                                  #                                    the last update date and time shown above."))),
                                                  # actionButton("refreshdb", "Update DB",icon("paper-plane"),
                                                  #               style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                   tags$div("Loading...",id="loadmessage")),
                                                  
                                                  br()
                                                  
                                                  )),
                                         
                                         column(9,
                                                # ("plot1"),
                                                # wellPanel(
                                                #   DT::dataTableOutput('sc_hh_summary_ui')
                                                #   # DT::dataTableOutput('sc_tbl'),
                                                #   # span("Number of movies selected:",
                                                #   #      textOutput("n_movies")
                                                #   ),
                                                
                                                
                                                tabsetPanel(
                                                  
                                                  
                                                  tabPanel( "Sub-Centre wise Summary",
                                                            DT::dataTableOutput('sc_hh_summary_ui')
                                                            # DT::dataTableOutput('sc_tbl'),
                                                            # span("Number of movies selected:",
                                                            #      textOutput("n_movies")
                                                  ),
                                                  tabPanel("ASHA wise Summary",
                                                           DT::dataTableOutput('sc_asha_hh_summary_ui')
                                                           # DT::dataTableOutput('sc_tbl'),
                                                           # span("Number of movies selected:",
                                                           #      textOutput("n_movies")
                                                  ),
                                                  
                                                  tabPanel( "ASHA wise HH Details",
                                                            DT::dataTableOutput('asha_hh_summary_ui')
                                                            # DT::dataTableOutput('sc_tbl'),
                                                            # span("Number of movies selected:",
                                                            #      textOutput("n_movies")
                                                  ),
                                                  tabPanel( "HH wise Memeber Details",
                                                            DT::dataTableOutput('hh_member_details_ui')
                                                            # DT::dataTableOutput('sc_tbl'),
                                                            # span("Number of movies selected:",
                                                            #      textOutput("n_movies")
                                                  ),
                                                  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                                   tags$div("Loading...",id="loadmessage"))
                                                  
                                                ))
                             )
                             
                                       ),
                             tags$head(
                               tags$link(
                                 rel = "stylesheet",
                                 type = "text/css",
                                 href = "my.css")
                             ),
                             tabPanel("Household Records",
                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                       tags$div("Loading...",id="loadmessage")),
                                      verbatimTextOutput("updation_date1"),
                                      DT::dataTableOutput('all_household_records')
                             ),
                             
                             tabPanel("Family Member Records",
                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                       tags$div("Loading...",id="loadmessage")),
                                      verbatimTextOutput("updation_date2"),
                                      DT::dataTableOutput('all_household_member_records')
                             ),
                             
                             tabPanel("Birth History Records",
                                      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                                       tags$div("Loading...",id="loadmessage")),
                                      verbatimTextOutput("updation_date3"),
                                      DT::dataTableOutput('all_birth_history')
                             )
                             
                             
                             
                             
                  ),
                  
                  tabPanel("Non Communicable Diseases",
                           conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                            tags$div("Loading...",id="loadmessage")),
                           fluidRow(
                             column(12,offset = 0,
                                    infoBoxOutput("totalPopulation", width = 3),
                                    infoBoxOutput("pop30Years",width = 3),
                                    infoBoxOutput("highRisk",width = 3),
                                    infoBoxOutput("needReferral",width = 3)
                             )),
                           
                           tags$hr(),
                           
                           fluidRow (
                             
                             column(width = 5 ,offset = 1 ,style='padding:0px;',
                                    tabsetPanel(id = "tabs",
                                                tabPanel( "High Risk",id = "tab_highrisk",
                                                          highchartOutput("highriskNCD", height = "500px")
                                                          # DT::dataTableOutput('sc_tbl'),
                                                          # span("Number of movies selected:",
                                                          #      textOutput("n_movies")
                                                )
                                                ,
                                                tabPanel("Referral", id = "tab_referral",
                                                         highchartOutput("referralNCD", height = "500px")
                                                         # DT::dataTableOutput('')
                                                         # DT::dataTableOutput('sc_tbl'),
                                                         # span("Number of movies selected:",
                                                         #      textOutput("n_movies")
                                                )
                                    )), 
                             # style='width: 400px; height: 500px'),
                             # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             #                  tags$div("Loading...",id="loadmessage"))
                             column(width =5,
                                    offset = 1,
                                    style='padding:0px;',  
                                    tabsetPanel(id = "tabs_factors",
                                                tabPanel( "Factors:High Risk",id = "tab_factors",
                                                          highchartOutput("riskfactorsNCD", height = "500px")
                                                          # DT::dataTableOutput('sc_tbl'),
                                                          # span("Number of movies selected:",
                                                          #      textOutput("n_movies")
                                                ),
                                                tabPanel("Symptoms:Referral-All",id = "tab_symptoms_all",
                                                         highchartOutput("symptomsAllNCD", height = "500px")
                                                         # ,
                                                         # DT::dataTableOutput('')
                                                         # DT::dataTableOutput('sc_tbl'),
                                                         # span("Number of movies selected:",
                                                         #      textOutput("n_movies")
                                                ),
                                                tabPanel("Symptoms:Referral-Women",id = "tab_symptoms_women",
                                                         highchartOutput("symptomsWomenNCD", height = "500px")
                                                         # ,
                                                         # DT::dataTableOutput('')
                                                         # DT::dataTableOutput('sc_tbl'),
                                                         # span("Number of movies selected:",
                                                         #      textOutput("n_movies")
                                                )
                                                
                                    )
                                    
                             )
                             
                           ),
                           tags$hr(),
                           
                           fluidRow(
                             
                             column(width =12,
                                    offset = 0,
                                    style='padding:0px;',  
                                    # tabsetPanel(
                                    #  # tabPanel(
                                    DT::dataTableOutput('highriskCases')
                                    # DT::dataTableOutput('sc_tbl'),
                                    # span("Number of movies selected:",
                                    #      textOutput("n_movies")
                                    # )
                                    # tabPanel("Referral"
                                    #          # ,
                                    #          # DT::dataTableOutput('')
                                    #          # DT::dataTableOutput('sc_tbl'),
                                    #          # span("Number of movies selected:",
                                    #          #      textOutput("n_movies")
                                    # )
                                    # )
                                    # style='width: 400px; height: 500px')
                             )
                             
                             
                             
                             
                           )
                           
        ))
      
      
            
      
    }
)
      
#####################Password code till above line  

update_string <- reactiveValues()  
update_string$name <- last_update$x

# sc_hh_summary <- get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre)]
# sc_asha_hh_summary <-  get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre, asha)]
# asha_hh_summary <-  get_dt()[, .(asha, village_name ,household_number, `group_hh_family_members[1]/name`,total_members )] ## we dont need ASHA name




observeEvent(input$refreshdb, {
  
  update_string$name

})

output$subCentres <- renderInfoBox({
  infoBox(
    "Sub Centres", length(unique(get_dt()$subcentre)), icon = icon("medkit"),
    color = "yellow", 
    fill = TRUE, 
    width = 3
  )
})

output$scAshas <- renderInfoBox({
  infoBox(
    "ASHA", length(unique(get_dt()$asha)), icon = icon("female"),
    color = "light-blue", 
    fill = TRUE, 
    width = 3
  )
})

output$totalHousehold <- renderInfoBox({
  infoBox(
    "Households", format(sum(as.integer( 
      get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre, asha)]$`Total Households`)) 
      # sc_asha_hh_summary$`Total Households`))
      , nsmall=0, big.mark=","),
    icon = icon("home"),
    color = "olive",
    fill = TRUE,
    width = 3
  )
})


output$totalMembers <- renderInfoBox({
  infoBox(
    "Family Members", format(sum(as.integer(
      get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre)]$`Total Members` 
      )),nsmall=0, big.mark=","),
    icon = icon("users"),
    color = "maroon",
    fill = TRUE,
    width = 3
  )
})



output$dttime <- renderText({
    
  update_string$name
  
  })
  

refreshKobo <- function(){
      
    # count <- kobo_submission_count("153088", api = "https://kc.humanitarianresponse.info/api/v1/",user = c("tattva","tattva"))
    
    # if(nrow(hwc_main) < count) {
    # 
    #   hwc_temp <- kobo_data_downloader("153088",user = c("tattva","tattva"), 
    #                                  api = "https://kc.humanitarianresponse.info/api/v1/", check = FALSE)
    #   
    #   reference_date <- as.POSIXct("2017-08-16 00:00:00 IST")
    #   hwc_temp$start <- as.POSIXct(hwc_temp$start)
    #   hwc_temp$end <- as.POSIXct(hwc_temp$end)
    #   hwc_temp <- hwc_temp[start > reference_date]
    #   
    #   pb.date <- Sys.time()
    #   update_string$name <- format(pb.date, tz="Asia/Kolkata",usetz=TRUE)
    # 
    #   write.csv(update_string$name,"./Data/last_update.csv")
    #   write.csv(hwc_temp,"./Data/hwc_data_dump.csv")
    #   
    #   return(hwc_temp)
    #   
    #  }
    
    pb.date <- Sys.time()
    update_string$name <- format(pb.date, tz="Asia/Kolkata",usetz=TRUE)
    
    write.csv(update_string$name,"./Data/last_update.csv")
    
    hwc_main <- data.table(fread("./Data/hwc_data_dump.csv", header = TRUE))[, , keyby = .(subcentre, asha)][order(subcentre, asha)] 
    
    reference_date <- as.POSIXct("2017-08-16 00:00:00 IST")
    hwc_main$start <- as.POSIXct(hwc_main$start)
    hwc_main$end <- as.POSIXct(hwc_main$end)
    hwc_main <- hwc_main[start > reference_date]
    
    
    return(hwc_main)
    
  
  }
  

   
get_dt <- reactive({
    
    # if (input$refreshdb) {
    #   
    #   hwc_main <- refreshKobo()[, , keyby = .(subcentre, asha)] [order(subcentre, asha)]
    #   return(hwc_main)
    #  }
    # 
    # else {
      # 
      # hwc_main <- data.table(test_df)[, , keyby = .(subcentre, asha)][order(subcentre, asha)] 
      # 
      # reference_date <- as.POSIXct("2017-08-16 00:00:00 IST")
      # hwc_main$start <- as.POSIXct(hwc_main$start)
      # hwc_main$end <- as.POSIXct(hwc_main$end)
      # hwc_main <- as.data.table(filter(hwc_main, start > reference_date))
      
      # return(hwc_main[, , keyby = .(subcentre, asha)][order(subcentre, asha)] )
      return(hwc_main)
    
    # }
    
    
  })
  
#############################################################################################################################################
################################################# SUB-CENTRE TABLE ##########################################################################
#############################################################################################################################################
  
  sketch1 <- htmltools::withTags(table(
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", colnames(sc_hh_summary))),
    tableFooter(c("", c("",0,0)))
  ))
  
  opts1 <- list( 
    footerCallback = JS(
      "function( tfoot, data, start, end, display ) {",
      "var api = this.api();",
      "$( api.column( 3 ).footer() ).html(",
      "api.column( 3 ).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",  # remove ; here
      ");",
      "$( api.column( 2 ).footer() ).html(",
      "api.column( 2 ).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",  # remove ; here
      ");",
      
    "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'
      ))
    # pagingType = "full"
    
    
    )
  
  
  
  output$sc_hh_summary_ui = DT::renderDataTable( 
                         
                           DT::datatable(get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members)),
                                                        keyby = .(subcentre)] , container = sketch1, options = opts1, selection = 'single',
                           class = 'cell-border stripe',
                           caption = 'Table 1: Sub-Centre wise Household Details.',
                           extensions = 'Buttons'
                          
                           # filter = 'top'
                          ))
  
  #############################################################################################################################################
  
  
  
  #############################################################################################################################################
  ################################################# ASHA HH SUMMARY ###########################################################################
  #############################################################################################################################################  
  
  sketch2 <- htmltools::withTags(table(
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", colnames(sc_asha_hh_summary))),
    tableFooter(c("", c("","",0,0)))
  ))
  
  opts2 <- list( 
    
    footerCallback = JS(
      "function( tfoot, data, start, end, display ) {",
      "var api = this.api();",
      "$( api.column( 3 ).footer() ).html(",
      "api.column( 3 ).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",  # remove ; here
      ");",
      "$( api.column( 4 ).footer() ).html(",
      "api.column( 4 ).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",  # remove ; here
      ");",
      
      "}"),
  
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}"),
      
      searchHighlight = TRUE,
      dom = 'lfrtiBp',
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
      buttons = 
        list ('print',list(
          extend = 'collection',
          buttons = c('csv','excel', 'pdf'),
          text = 'Download'))  
    
      
)
  
  

  output$sc_asha_hh_summary_ui = DT::renderDataTable( 
    
    if (! is.null(length(input$sc_hh_summary_ui_rows_selected)))
        
      {
  
      selectedrowindex <- input$sc_hh_summary_ui_rows_selected[length(input$sc_hh_summary_ui_rows_selected)]
      
      selectedrowindex <- as.numeric(selectedrowindex)
     
      subcentre_name <- get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members)),
                keyby = .(subcentre)][selectedrowindex]$subcentre
      
      data1 <- get_dt()[subcentre == subcentre_name,
                        .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre, asha)]
      DT::datatable (data1, container = sketch2, options = opts2, selection = 'single', class = 'cell-border stripe',
                     caption = 'Table 2: Sub-Centre and ASHA wise Household Details.',
                     extensions = 'Buttons')
      
      }
    
    )
  
  #############################################################################################################################################
  
  
  #############################################################################################################################################
  ################################################# ASHA HH DETAILS ###########################################################################
  #############################################################################################################################################    

  sketch3 <- htmltools::withTags(table( 
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", c("ASHA","Village","HH No.","HoF Name","Members"))),
    tableFooter(c("", c("","","","",0)))
  ))
  
  opts3 <- list( 
    footerCallback = JS(
      "function( tfoot, data, start, end, display ) {",
      "var api = this.api();",
      "$( api.column( 5 ).footer() ).html(",
      "api.column( 5 ).data().reduce( function ( a, b ) {",
      "return a + b;",
      "} )",  # remove ; here
      ");",
      
      
      "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'))  
    
    
  )
  
  
  
  output$asha_hh_summary_ui = DT::renderDataTable( 
    
   if (!is.null(length(input$sc_asha_hh_summary_ui_rows_selected)))
    {
      
      selectedrowindex <- input$sc_hh_summary_ui_rows_selected[length(input$sc_hh_summary_ui_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
      
      subcentre_name <- get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members)),
                                 keyby = .(subcentre)][selectedrowindex]$subcentre
      
      data1 <- get_dt()[subcentre == subcentre_name,
                        .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre, asha)]
     
      asha_name <- data1[as.numeric(input$sc_asha_hh_summary_ui_rows_selected)]$asha
      
      data2 <- get_dt()[subcentre == subcentre_name & asha == asha_name,
                       .(village_name ,household_number, `group_hh_family_members[1]/name`,total_members ), keyby = .(asha)]
      
      DT::datatable (data2, container = sketch3, options = opts3, selection = 'single',
                        class = 'cell-border stripe',
                        caption = 'Table 3: ASHA wise Household Details.')
      
    }
      
    )
  
  #############################################################################################################################################
  
  

  ##########################################################################################
  #################### FUNCTIONS TO MAKE A DATA TABLE OF FAMILY MEMBERS SELECTED#############   
  ########################################################################################## 
  
  member_details <- function(hwc_dt_temp) {
    
    if (!is.null(length(input$asha_hh_summary_ui_rows_selected)) ) {
    family_details <- hwc_dt_temp
    hh_member_n <- data.table() # Create a blank data.table
    
    for (i in seq(as.numeric(family_details[,total_family_members]))) {
      
      field_1 <- paste0("group_hh_family_members","[",i,"]","/","name")
      field_2 <- paste0("group_hh_family_members","[",i,"]","/","relationship_with_hof")
      field_3 <- paste0("group_hh_family_members","[",i,"]","/","age")
      field_4 <- paste0("group_hh_family_members","[",i,"]","/","sex")
      field_5 <- paste0("group_hh_family_members","[",i,"]","/","marital_status")
      field_6 <- paste0("group_hh_family_members","[",i,"]","/","read_or_write")
      field_7 <- paste0("group_hh_family_members","[",i,"]","/","in_school_college")
      field_8 <- paste0("group_hh_family_members","[",i,"]","/","years_education")
      
      field_all <- c(field_1,field_2,field_3,field_4,field_5,field_6,field_7,field_8) 
      hh_member_temp <- family_details[, field_all, with=FALSE]
      
      hh_member_n <- rbind(hh_member_n,  setnames(hh_member_temp, old = field_all, 
                                                  new = c("Name","Relationship","Age","Sex","Marital Status", "Read or Write",
                                                          "Currently Studying",
                                                          "Years of Education"
                                                          
                                                          ))) ## we dont need ASHA name 
    }
    print(head(hh_member_n))
    return(hh_member_n)  
    }
  }
  
  ##########################################################################################
  #################### FUNCTIONS TO MAKE A DATA TABLE OF FAMILY MEMBERS SELECTED#############   
  ########################################################################################## 
  
 
  
  
  sketch4 <- htmltools::withTags(table( 
   
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", c("Name","Relationship","Age","Sex","Marital Status", "Read or Write",
                          "Currently Studying",
                          "Years of Education"
                          ))),
    # tableHeader(c("ID", )),
    tableFooter(c("", c("","","","","","","","")))
  ))
  
  opts4 <- list(
    footerCallback = JS(
      "function( tfoot, data, start, end, display ) {",
      "var api = this.api();",
      
      "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'))    
    
    
    )
  
  
  output$hh_member_details_ui = DT::renderDataTable(

    # output$selectedrow <- DT::renderDataTable
    
    if (!is.null(length(input$asha_hh_summary_ui_rows_selected)) )

    {
      selectedrowindex <- input$sc_hh_summary_ui_rows_selected[length(input$sc_hh_summary_ui_rows_selected)]
      selectedrowindex <- as.numeric(selectedrowindex)
      subcentre_name <- get_dt()[, .("Total Households"=.N, "Total Members" = sum(total_members)),
                                 keyby = .(subcentre)][selectedrowindex]$subcentre

      data1 <- get_dt()[subcentre == subcentre_name,
                        .("Total Households"=.N, "Total Members" = sum(total_members) ), keyby = .(subcentre, asha)]

      asha_name <- data1[as.numeric(input$sc_asha_hh_summary_ui_rows_selected)]$asha

      data2 <- get_dt()[subcentre == subcentre_name & asha == asha_name,
                        .(village_name ,household_number, `group_hh_family_members[1]/name`,total_members ), keyby = .(asha)]
      hh_number <- data2[as.numeric(input$asha_hh_summary_ui_rows_selected)]$household_number
      vill_name <- data2[as.numeric(input$asha_hh_summary_ui_rows_selected)]$village_name
      tot_members <- data2[as.numeric(input$asha_hh_summary_ui_rows_selected)]$total_members
      hof_name <- data2[as.numeric(input$asha_hh_summary_ui_rows_selected)]$`group_hh_family_members[1]/name`
      
      data3 <- get_dt()[subcentre == subcentre_name & asha == asha_name & village_name == vill_name & total_members == tot_members &
                        `group_hh_family_members[1]/name` == hof_name & household_number == hh_number,]
      
      members_coded <- member_details(data3)
      
      allowedVars <- c("Relation","Gender","Marital.Status","Read.Write","Currently.Studying","Years.of.Education")
      
      members_decoded <- addNewData("./Data/dataNew.csv", members_coded,allowedVars)
      
      # print(members_decoded$Age)
      # print(class(members_decoded$Age))
      # # as.numeric(levels(f))[f]
      
      members_decoded$Age <- as.numeric(levels(members_decoded$Age))[members_decoded$Age]
      
      # members_decoded$Age <- as.integer(members_decoded$Age)
      
      # print("$$$$$$$$$$$$$$")
      # print(members_decoded$Age)
      # print(members_decoded)
      # print("#############")
      
      members_decoded <- as.data.table(members_decoded)
      # print(head(members_decoded))
      # print(class(members_decoded))
      members_decoded <- members_decoded[, c("Name","Relation","Age","Gender","Marital.Status","Read.Write","Currently.Studying","Years.of.Education")]
      # print(head(members_decoded))
     
      DT::datatable (members_decoded, container = sketch4, options = opts4, selection = 'single',
                     class = 'cell-border stripe',
                     caption = 'Table 4: Household member details')

    })
  
  
  #############################################################################################################################################
  ################################################# HOUSEHOLD RECORDS ##########################################################################
  #############################################################################################################################################
  
  output$updation_date1 <- renderText({
    as.character("These Household Records are updated till 22-11-2017")
  })
  
  sketch10 <- htmltools::withTags(table(
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", colnames(all_hh_rec)))
    # tableFooter(c("", c("",0,0)))
  ))
  
  opts10 <- list( 
    # footerCallback = JS(
    #   "function( tfoot, data, start, end, display ) {",
    #   "var api = this.api();",
    #   "$( api.column( 3 ).footer() ).html(",
    #   "api.column( 3 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   "$( api.column( 2 ).footer() ).html(",
    #   "api.column( 2 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   
    #   "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    scrollX = TRUE,
    fixedColumns = TRUE,
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'
      ))
    # pagingType = "full"
    
    
  )
  
  
  
  output$all_household_records = DT::renderDataTable( 
    
    DT::datatable(all_hh_rec, container = sketch10, options = opts10, selection = 'single',
                  class = 'cell-border stripe',
                  caption = 'Table : HouseHold Records.',
                  extensions = 'Buttons',
                  filter = 'top'
    ))
  
  #############################################################################################################################################
  
  
  #############################################################################################################################################
  ################################################# ALL MEMBER RECORDS ##########################################################################
  #############################################################################################################################################
  output$updation_date2 <- renderText({
    as.character("These Household Records are updated till 22-11-2017")
  })
  
  
  sketch11 <- htmltools::withTags(table(
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", colnames(all_hh_mem_rec)))
    # tableFooter(c("", c("",0,0)))
  ))
  
  opts11 <- list( 
    # footerCallback = JS(
    #   "function( tfoot, data, start, end, display ) {",
    #   "var api = this.api();",
    #   "$( api.column( 3 ).footer() ).html(",
    #   "api.column( 3 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   "$( api.column( 2 ).footer() ).html(",
    #   "api.column( 2 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   
    #   "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    scrollX = TRUE,
    fixedColumns = TRUE,
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'
      ))
    # pagingType = "full"
    
    
  )
  
  
  
  output$all_household_member_records = DT::renderDataTable( 
    
    DT::datatable(all_hh_mem_rec, container = sketch11, options = opts11, selection = 'single',
                  class = 'cell-border stripe',
                  caption = 'Table : HouseHold Member Records.',
                  extensions = 'Buttons',
                  filter = 'top'
    ))
  
  #############################################################################################################################################
  
  
  #############################################################################################################################################
  ################################################# MOTHER BIRTH HISTORY RECORDS ##########################################################################
  #############################################################################################################################################
  output$updation_date3 <- renderText({
    as.character("These Household Records are updated till 22-11-2017")
  })
  
  sketch12 <- htmltools::withTags(table(
    class = "display",
    style = "bootstrap",
    tableHeader(c("ID", colnames(all_bh_rec)))
    # tableFooter(c("", c("",0,0)))
  ))
  
  opts12 <- list( 
    # footerCallback = JS(
    #   "function( tfoot, data, start, end, display ) {",
    #   "var api = this.api();",
    #   "$( api.column( 3 ).footer() ).html(",
    #   "api.column( 3 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   "$( api.column( 2 ).footer() ).html(",
    #   "api.column( 2 ).data().reduce( function ( a, b ) {",
    #   "return a + b;",
    #   "} )",  # remove ; here
    #   ");",
    #   
    #   "}"),
    
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"),
    searchHighlight = TRUE,
    dom = 'lfrtiBp',
    scrollX = TRUE,
    fixedColumns = TRUE,
    lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
    buttons = 
      list ('print',list(
        extend = 'collection',
        buttons = c('csv','excel', 'pdf'),
        text = 'Download'
      ))
    # pagingType = "full"
    
    
  )
  
  
  
  output$all_birth_history = DT::renderDataTable( 
    
    DT::datatable(all_bh_rec, container = sketch12, options = opts12, selection = 'single',
                  class = 'cell-border stripe',
                  caption = 'Table : HouseHold Member Records.',
                  extensions = 'Buttons',
                  filter = 'top'
    ))
  
  #############################################################################################################################################
  ####################################################  NON COMMUNICABLE DISEASES PAGE ####################################################  
  #############################################################################################################################################
  
  
  output$totalPopulation <- renderInfoBox({
    infoBox(
      "Total Population", format(nrow(hh_member_details), nsmall = 0, big.mark = ","), icon = icon("users"),
      color = "purple", 
      fill = TRUE, 
      width = 3
    )
    
  })
  
    output$pop30Years <- renderInfoBox({
      infoBox(
        " 30 Years or Above", format(nrow(member_gt_30years), nsmall = 0, big.mark = ","), icon = icon("user-circle"),
        color = "teal", 
        fill = TRUE, 
        width = 3
      )
    })
      
      output$highRisk <- renderInfoBox({
        infoBox(
          " High Risk Cases", format(nrow(ncd_high_risk_cases), nsmall = 0, big.mark = ","), icon = icon("heartbeat"),
          color = "orange", 
          fill = TRUE, 
          width = 3
        )
    })
  
  output$needReferral <- renderInfoBox({
        infoBox(
          "Referral Cases", format(nrow(ncd_referral_cases), nsmall = 0, big.mark = ","), icon = icon("ambulance"),
          color = "red", 
          fill = TRUE, 
          width = 3
        )
      })
  
# reactive flag value for NCD drill down and inter chart logic integration
rv_flag <- reactiveValues(flag = 0)
    
observeEvent(input$canvasClicked,{
    
    # index1 <<- input$canvasClicked[1]
    # index2 <<- as.numeric(input$canvasClicked[2]) + 1
     
    # print(index1)
    # print(index2)
  
    # print(paste(rv_flag$flag, "............ in canvasclicked"))
    rv_flag$flag <- 1
    # print(paste(rv_flag$flag, "............ change in canvasclicked"))
    # print(paste(input$tabs,"tab status"))
    ncd_df()
    
})
      

observeEvent(input$tabs,{

  # print( "tab changed................")
  rv_flag$flag <- 0
  ncd_df()

})

  
ncd_df <- reactive({
  
  # print(paste(flag, "flag............ in ncd_df", input$tabs))
  # print(paste(input$tabs,"tab status"))
   
  if(input$tabs == "High Risk" & rv_flag$flag == 1)
    {
    # print("HHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHHH")

     index1 <- input$canvasClicked[1]
     index2 <- as.numeric(input$canvasClicked[2]) + 1
    
    
    # print("1.........")
    
    # print(index1)
    # print(index2)
   
    
    df_subset <- ncd_high_risk_cases
    
    if(!is.null(index1))
    {
    if(index1 == "High Risk Cases:HWC wise")
    { 
      # print("2.........")
      df_subset <- filter(ncd_high_risk_cases, subCentre == bar_ncd_high_risk[index2,]$name)
      df_subset_temp_hr <<- df_subset
      
      # print(nrow(df_subset))
      
      return (df_subset)
      
      }
    
    else if (index1 == "High Risk Cases:ASHA wise")
    {
      df_asha_names <- df_subset_temp_hr %>%
        group_by(asha)  %>%
        summarise(N_asha = n()) %>% 
        arrange(desc(N_asha))
      
      df_subset <- filter(df_subset_temp_hr, asha == as.character(df_asha_names$asha[index2]))
      return (df_subset)
    }
        }
    else return (df_subset)
    
  }
  
if(input$tabs == "High Risk" & rv_flag$flag == 0)
  {
    # rv_flag$flag <- 1
    # print("high risk df returned..............")
    return (ncd_high_risk_cases) 
   
  }
  
if(input$tabs == "Referral" & rv_flag$flag == 1)
  
  {
    # print("WWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWWW")
    index1 <- input$canvasClicked[1]
    index2 <- as.numeric(input$canvasClicked[2]) + 1
    
   # print(index1)
   # print(index2)

   
   df_subset <- ncd_referral_cases
   
   if(!is.null(index1))
     {
  
  if (index1 == "Referral Cases:HWC wise")
  {
    df_subset <- filter(ncd_referral_cases, subCentre == bar_ncd_referral[index2,]$name)
    df_subset_temp_rf <<- df_subset
    
    return (df_subset)
    
  }
  
  else if (index1 == "Referral Cases:ASHA wise")
  {
    df_asha_names <- df_subset_temp_rf %>%
      group_by(asha)  %>%
      summarise(N_asha = n()) %>% 
      arrange(desc(N_asha))
    
    df_subset <- filter(df_subset_temp_rf, asha == as.character(df_asha_names$asha[index2]))
    return (df_subset)
    
  }
   }
   
   else return (df_subset)
 }
  
if(input$tabs == "Referral" & rv_flag$flag == 0)
  {
    # rv_flag$flag <- 1
    # print("referral df returned..............")
    return (ncd_referral_cases)
  }

    
})
  

  
output$highriskNCD <- renderHighchart ({      
        
        
    canvasClickFunction <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
        
      
        highchart() %>% 
          hc_chart(type = "bar"
                   # ,
                   # events = list(
                   #   drilldown = 
                   #     JS('function(e) {
                   # console.log(e.seriesOptions);
                   # this.setTitle({text: e.seriesOptions.name});
                   # } '))
          ) %>%
          # hc_title(text = "High Risk Cases - NCDs") %>%
          hc_title(text = "<b>High Risk Cases</b> - NCDs",
                   # # margin = 20, 
                   # align = "centre",
                   style = list(color = "#05008b", useHTML = TRUE)) %>%
          
          # hc_title(text = span( "High Risk Cases - NCDs", style="color:#e32c3e")
          #           # style = list(fontWeight = "bold")
          #          ) %>%
          
          
          
          hc_xAxis(type = "category",
                   title = list(text = "Health & Wellness Centres")
                   # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
                   
          ) %>%
          
          hc_yAxis(title = list(text = "No. of Cases")) %>%
          hc_legend(enabled = FALSE) %>%
          hc_plotOptions(
            series = list(
              boderWidth = 0,
              dataLabels = list(enabled = TRUE),
              # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
              colors = rev(colorRampPalette(brewer.pal(9,"Blues"))(nrow(bar_ncd_high_risk))),
              events = list(click = canvasClickFunction)
            )) %>%  
          
          hc_add_series(
            name = "High Risk Cases:HWC wise",
            data = list_parse(bar_ncd_high_risk),
            colorByPoint = TRUE
          )  %>% 
        
          hc_drilldown(
            allowPointDrilldown = TRUE,
            series = series_list
          )
        
      })     

#######################################################################################
########################### DRILL-DOWN HISTOGRAM FOR REFERRAL CASES ###################
#######################################################################################

output$referralNCD <- renderHighchart ({      
  
  
  canvasClickFunction_referral <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
  
  
  highchart() %>% 
    hc_chart(type = "bar"
             # ,
             # events = list(
             #   drilldown = 
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    hc_title(text = "<b>Referral Cases</b> - NCDs",
             # # margin = 20, 
             # align = "centre",
             style = list(color = "#ab0000", useHTML = TRUE)) %>%
    # hc_title(text = "Referral Cases - NCDs") %>%
    hc_xAxis(type = "category",
             title = list(text = "Health & Wellness Centres")
             # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
             
    ) %>%
    
    hc_yAxis(title = list(text = "No. of Cases")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE),
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        colors = rev(colorRampPalette(brewer.pal(9,"Reds"))(nrow(bar_ncd_referral))),
        events = list(click = canvasClickFunction_referral)
      )) %>%  
    
    hc_add_series(
      name = "Referral Cases:HWC wise",
      data = list_parse(bar_ncd_referral),
      colorByPoint = TRUE
    )  %>% 
    
    hc_drilldown(
      allowPointDrilldown = TRUE,
      series = series_list_asha
    )
  
})     


#######################################################################################
########################### SYMPTOMS GRAPHS ###########################################
#######################################################################################
 
output$riskfactorsNCD <- renderHighchart ({      
  
  
  canvasClickFunction_riskfactors <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
  
  
  highchart() %>% 
    hc_chart(type = "bar"
             # ,
             # events = list(
             #   drilldown = 
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    hc_title(text = "<b>NCD Risk Assessment:</b> Factors",
             # # margin = 20, 
             # align = "centre",
             style = list(color = "#eabd00", useHTML = TRUE)) %>%
    
    # hc_title(text = "NCD Risk Assessment: Factors") %>%
    hc_xAxis(type = "category",
             title = list(text = "High Risk Factors")
             # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
             
    ) %>%
    
    hc_yAxis(title = list(text = "No. of Cases")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE),
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        colors = rev(colorRampPalette(brewer.pal(9,"Pastel1"))(nrow(risk_factors_df))),
        events = list(click = canvasClickFunction_riskfactors)
      )) %>%  
    
    hc_add_series(
      name = "High Risk Factors: NCD",
      data = list_parse(risk_factors_df),
      colorByPoint = TRUE
    ) 
    
   
  
})     
  


output$symptomsAllNCD <- renderHighchart ({      
  
  
  canvasClickFunction_riskfactors <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
  
  
  highchart() %>% 
    hc_chart(type = "bar"
             # ,
             # events = list(
             #   drilldown = 
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    
    hc_title(text = "<b>Referral Symptoms:</b> Men + Women",
             # # margin = 20, 
             # align = "centre",
             style = list(color = "#eabd00", useHTML = TRUE)) %>%
    
    hc_xAxis(type = "category",
             title = list(text = "Referral Symptoms")
             # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
             
    ) %>%
    
    hc_yAxis(title = list(text = "No. of Cases")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE),
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        colors = rev(colorRampPalette(brewer.pal(9,"Pastel1"))(nrow(referral_symptoms_both_df))),
        events = list(click = canvasClickFunction_riskfactors)
      )) %>%  
    
    hc_add_series(
      name = "Referral Symptoms (all)",
      data = list_parse(referral_symptoms_both_df),
      colorByPoint = TRUE
    ) 
  

})       
      
      
output$symptomsWomenNCD <- renderHighchart ({      
  
  
  canvasClickFunction_riskfactors <- JS("function(event) {Shiny.onInputChange('canvasClicked', [this.name, event.point.x]);}")
  
  
  highchart() %>% 
    hc_chart(type = "bar"
             # ,
             # events = list(
             #   drilldown = 
             #     JS('function(e) {
             # console.log(e.seriesOptions);
             # this.setTitle({text: e.seriesOptions.name});
             # } '))
    ) %>%
    hc_title(text = "<b>Referral Symptoms:</b> Women",
             # # margin = 20, 
             # align = "centre",
             style = list(color = "#eabd00", useHTML = TRUE)) %>%
    hc_xAxis(type = "category",
             title = list(text = "Referral Symptoms")
             # JS('function(e) {chart.options.yAxis[0].title.text = "new title";  }')
             
    ) %>%
    
    hc_yAxis(title = list(text = "No. of Cases")) %>%
    hc_legend(enabled = FALSE) %>%
    hc_plotOptions(
      series = list(
        boderWidth = 0,
        dataLabels = list(enabled = TRUE),
        # colors = scale_color_brewer( ,type = "seq", palette = 1, direction = 1)
        colors = rev(colorRampPalette(brewer.pal(9,"Pastel1"))(nrow(referral_symptoms_women_df))),
        events = list(click = canvasClickFunction_riskfactors)
      )) %>%  
    
    hc_add_series(
      name = "Referral Symptoms (Women)",
      data = list_parse(referral_symptoms_women_df),
      colorByPoint = TRUE
    ) 
  
  
})             
      
  
#######################################################################################
########################### NCD TABLE ###########################################
#######################################################################################

# {css}
# .chart-wrapper {
#   overflow-x: scroll;
# }


sketch_ncd <- htmltools::withTags(table(
  class = "display",
  style = "bootstrap",
  tableHeader(c("ID", colnames(ncd_high_risk_cases)))
  # tableFooter(c("", c("",0,0)))
))

opts_ncd <- list( 
  # footerCallback = JS(
  #   "function( tfoot, data, start, end, display ) {",
  #   "var api = this.api();",
  #   "$( api.column( 3 ).footer() ).html(",
  #   "api.column( 3 ).data().reduce( function ( a, b ) {",
  #   "return a + b;",
  #   "} )",  # remove ; here
  #   ");",
  #   "$( api.column( 2 ).footer() ).html(",
  #   "api.column( 2 ).data().reduce( function ( a, b ) {",
  #   "return a + b;",
  #   "} )",  # remove ; here
  #   ");",
  #   
  #   "}"),
  
  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
    "}"),
  searchHighlight = TRUE,
  dom = 'lfrtiBp',
  scrollX = TRUE,
  # fixedColumns = TRUE,
  lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
  buttons = 
    list ('print',list(
      extend = 'collection',
      buttons = c('csv','excel', 'pdf'),
      text = 'Download'
    ))
  # pagingType = "full"
  
  
)



output$highriskCases = DT::renderDataTable( 
  
  DT::datatable(ncd_df(), container = sketch_ncd, options = opts_ncd, selection = 'single',
                class = 'cell-border stripe',
                caption = 'Table : NCD High Risk Cases (auto-filtered as per High Risk or Referral case selection)',
                extensions = 'Buttons',
                filter = 'top'
  ))






  

})


