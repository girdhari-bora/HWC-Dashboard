# library(ggvis)
library(shinythemes)
library(shiny)
library(DT)
library(shinyjs)
library(htmltools)
library(shinydashboard)
library(highcharter)
library(RColorBrewer)


###################PASSWORD CODE below
shinyUI(fluidPage(
  # Add Javascript
  tags$head(
    tags$link(rel="stylesheet", type="text/css",href="style.css"),
    tags$script(type="text/javascript", src = "md5.js"),
    tags$script(type="text/javascript", src = "passwdInputBinding.js")
  ),
  useShinyjs(),
  uiOutput("app")

))
  
  #############################PASSWORD CODE was till above line only
  
  
# 
# # For dropdown menu
# actionLink <- function(inputId, ...) {
#   tags$a(href='javascript:void',
#          id=inputId,
#          class='action-button',
#          ...)
# } 
# 
# 
# # mycss <- "
# # #plot-container {
# #   position: relative;
# # }
# # #loading-spinner {
# #   position: absolute;
# #   left: 50%;
# #   top: 50%;
# #   z-index: -1;
# #   margin-top: -33px;  /* half of the spinner's height */
# #   margin-left: -33px; /* half of the spinner's width */
# # }
# # #plot.recalculating {
# #   z-index: -2;
# # }
# # "
# 
# ################################################################################################################
# ################ Suppress errors in shiny web interface, dont use during development############################
# ################################################################################################################
# # options(shiny.error = function() {
# #   stop("")
# # })
# ################################################################################################################
# ################################################################################################################
# 
# fluidPage(
#   list(tags$head(HTML('<link rel="icon", href="my.ng",
#                                    type="image/png" />'))),
#   div(style="padding: 1px 0px; width: '100%'",
#       titlePanel(
#         title="", windowTitle="My Window Title"
#       )
#   ))
# 
#  
# 
# # PAGE_TITLE <- "My great title"
# navbarPage( header = "",
#   title=div(img(src="nhm-logo.png"), "Health & Wellness Centre"), 
#   theme = shinytheme("spacelab"),inverse = FALSE ,windowTitle = "UP NHM", selected = "Summary",
#   
#   tags$head(tags$style(type="text/css", "
#              #loadmessage {
#                 position: fixed;
#                top: 50px;
#                left: 0px;
#                width: 100%;
#                padding: 5px 0px 5px 0px;
#                text-align: center;
#                font-weight: bold;
#                font-size: 100%;
#                color: #000000;
#                background-color: #CCFF66;
#                z-index: 105;
#              }
#           ")),
#   
#   
#   includeCSS(path = "AdminLTE.css"),includeCSS(path = "shinydashboard.css"),
#  
#   # fluidRow(
#   #   column(9,offset = 3,
#   #   # infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 3),
#   #   infoBox("New Orders1", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4),
#   #   infoBox("New Orders11", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4),
#   #   infoBox("New Orders111", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4)
#   #   # infoBoxOutput("progressBox2"),
#   #   # infoBoxOutput("approvalBox2")
#   #   )
#   # ),
#   # tags$hr(),
#   
#   # 
#   # tabPanel("Monitoring",
#   #          verbatimTextOutput("summary")
#   # ),
#   
#   navbarMenu("Monitoring",
#   tabPanel( "Summary",
#             
#             fluidRow(
#               
#                 column(12,offset = 0,
#                      # infoBox("New Orders",  icon = icon("credit-card"), fill = TRUE, width = 3),
#                      infoBoxOutput("subCentres", width = 3),
#                      infoBoxOutput("scAshas",width = 3),
#                      infoBoxOutput("totalHousehold",width = 3),
#                      infoBoxOutput("totalMembers",width = 3)
#                      # infoBox("Total Households", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4),
#                      # infoBox("Total Members", 10 * 2, icon = icon("credit-card"), fill = TRUE, width = 4)
#                      # infoBoxOutput("progressBox2"),
#                      # infoBoxOutput("approvalBox2")
#               )
#             ),
#             tags$hr(),
#             
#             fluidRow( 
#               column(3,
#                      wellPanel(
#                        
#                        # h2("Click to refresh database"),
#                        useShinyjs(),
#                        
#                        tags$b(helpText("Last Update:")),
#                        tags$hr(),
#                        h4(textOutput("dttime"),style="color: #B90321"),
#                        tags$hr(),
#                        h6(helpText(tags$b("Note:"),tags$em("Please click the below UPDATE button, only if you want to see information after 
#                                                            the last update date and time shown above."))),
#                        actionButton("refreshdb", "Update DB",icon("paper-plane"), 
#                                     style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
#                        
#                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                                         tags$div("Loading...",id="loadmessage")),
#                        
#                        br()
#                        
#                        )),
#               
#               column(9,
#                      # ("plot1"),
#                      # wellPanel(
#                      #   DT::dataTableOutput('sc_hh_summary_ui')
#                      #   # DT::dataTableOutput('sc_tbl'),
#                      #   # span("Number of movies selected:",
#                      #   #      textOutput("n_movies")
#                      #   ),
#                      
#                      
#                      tabsetPanel(
#                        
#                        
#                        tabPanel( "Sub-Centre wise Summary",
#                                  DT::dataTableOutput('sc_hh_summary_ui')
#                                  # DT::dataTableOutput('sc_tbl'),
#                                  # span("Number of movies selected:",
#                                  #      textOutput("n_movies")
#                        ),
#                        tabPanel("ASHA wise Summary",
#                                 DT::dataTableOutput('sc_asha_hh_summary_ui')
#                                 # DT::dataTableOutput('sc_tbl'),
#                                 # span("Number of movies selected:",
#                                 #      textOutput("n_movies")
#                        ),
#                        
#                        tabPanel( "ASHA wise HH Details",
#                                  DT::dataTableOutput('asha_hh_summary_ui')
#                                  # DT::dataTableOutput('sc_tbl'),
#                                  # span("Number of movies selected:",
#                                  #      textOutput("n_movies")
#                        ),
#                        tabPanel( "HH wise Memeber Details",
#                                  DT::dataTableOutput('hh_member_details_ui')
#                                  # DT::dataTableOutput('sc_tbl'),
#                                  # span("Number of movies selected:",
#                                  #      textOutput("n_movies")
#                        ),
#                        conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                                         tags$div("Loading...",id="loadmessage"))
#                        
#                      ))
#   )
#   
#             ),
#   tags$head(
#     tags$link(
#       rel = "stylesheet",
#       type = "text/css",
#       href = "my.css")
#   ),
#   tabPanel("Household Records",
#            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                             tags$div("Loading...",id="loadmessage")),
#            verbatimTextOutput("updation_date1"),
#            DT::dataTableOutput('all_household_records')
#   ),
#   
#   tabPanel("Family Member Records",
#            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                             tags$div("Loading...",id="loadmessage")),
#            verbatimTextOutput("updation_date2"),
#            DT::dataTableOutput('all_household_member_records')
#   ),
#   
#   tabPanel("Birth History Records",
#            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                             tags$div("Loading...",id="loadmessage")),
#            verbatimTextOutput("updation_date3"),
#            DT::dataTableOutput('all_birth_history')
#   )
#   
#   
#   
#   
#   ),
#   
#   tabPanel("Non Communicable Diseases",
#            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                             tags$div("Loading...",id="loadmessage")),
#            fluidRow(
#                     column(12,offset = 0,
#                     infoBoxOutput("totalPopulation", width = 3),
#                     infoBoxOutput("pop30Years",width = 3),
#                     infoBoxOutput("highRisk",width = 3),
#                     infoBoxOutput("needReferral",width = 3)
#                 )),
#                 
#             tags$hr(),
#             
#             fluidRow (
#             
#             column(width = 5 ,offset = 1 ,style='padding:0px;',
#                     tabsetPanel(id = "tabs",
#                      tabPanel( "High Risk",id = "tab_highrisk",
#                                 highchartOutput("highriskNCD", height = "500px")
#                                # DT::dataTableOutput('sc_tbl'),
#                                # span("Number of movies selected:",
#                                #      textOutput("n_movies")
#                      )
#                      ,
#                      tabPanel("Referral", id = "tab_referral",
#                               highchartOutput("referralNCD", height = "500px")
#                               # DT::dataTableOutput('')
#                               # DT::dataTableOutput('sc_tbl'),
#                               # span("Number of movies selected:",
#                               #      textOutput("n_movies")
#                      )
#                      )), 
#                    # style='width: 400px; height: 500px'),
#                      # conditionalPanel(condition="$('html').hasClass('shiny-busy')",
#                      #                  tags$div("Loading...",id="loadmessage"))
#             column(width =5,
#                     offset = 1,
#                    style='padding:0px;',  
#                      tabsetPanel(id = "tabs_factors",
#                        tabPanel( "Factors:High Risk",id = "tab_factors",
#                                  highchartOutput("riskfactorsNCD", height = "500px")
#                                   # DT::dataTableOutput('sc_tbl'),
#                                   # span("Number of movies selected:",
#                                   #      textOutput("n_movies")
#                         ),
#                         tabPanel("Symptoms:Referral-All",id = "tab_symptoms_all",
#                                  highchartOutput("symptomsAllNCD", height = "500px")
#                                  # ,
#                                  # DT::dataTableOutput('')
#                                  # DT::dataTableOutput('sc_tbl'),
#                                  # span("Number of movies selected:",
#                                  #      textOutput("n_movies")
#                         ),
#                        tabPanel("Symptoms:Referral-Women",id = "tab_symptoms_women",
#                                 highchartOutput("symptomsWomenNCD", height = "500px")
#                                 # ,
#                                 # DT::dataTableOutput('')
#                                 # DT::dataTableOutput('sc_tbl'),
#                                 # span("Number of movies selected:",
#                                 #      textOutput("n_movies")
#                        )
#                        
#                        )
#                    
#             )
#             
#             ),
#            tags$hr(),
#            
#            fluidRow(
#              
#              column(width =12,
#                     offset = 0,
#                     style='padding:0px;',  
#                      # tabsetPanel(
#                      #  # tabPanel(
#                                 DT::dataTableOutput('highriskCases')
#                                 # DT::dataTableOutput('sc_tbl'),
#                                 # span("Number of movies selected:",
#                                 #      textOutput("n_movies")
#                       # )
#                       # tabPanel("Referral"
#                       #          # ,
#                       #          # DT::dataTableOutput('')
#                       #          # DT::dataTableOutput('sc_tbl'),
#                       #          # span("Number of movies selected:",
#                       #          #      textOutput("n_movies")
#                       # )
#                     # )
#                     # style='width: 400px; height: 500px')
#              )
#            
#            
#            
#             
#               )
#   
# ))


