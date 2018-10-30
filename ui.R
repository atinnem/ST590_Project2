#Amanda Tinnemore
#ST503
#Project 2

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

df.csv<-read_csv("procedure_times.csv")

ui <- dashboardPage(skin="purple",
                    header<-dashboardHeader(title = "Procedure Times"),
                    sidebar<-dashboardSidebar(
                      sidebarMenu(menuItem("about", tabName = "about", icon = icon("archive")),
                                  menuItem("application", tabName = "app",icon = icon("laptop")))
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "about",
                                fluidRow(
                                  column(6,
                                         h1("What does this app do?"),
                                         box(background = "purple", width = 12,
                                             h4("The data in this app is from an echocardiography lab.  To learn more about echocardiography", a("click here", href="https://www.mayoclinic.org/tests-procedures/echocardiogram/about/pac-20393856")),
                                             h4("In this department staff members are allotted one hour to complete a study and to enter a preliminary report."),
                                             h4("Each quarter this data is reviewed to analyze trends, tweak scheduling, and potentially justify additional personnel.  It also serves to identify both efficient and less than efficient staff members."),
                                             h4(HTML("The time frames analyzed are <em>Scan Time</em> and <em>Report Time</em>.")),
                                             h4(HTML("<em>Scan Time</em> refers to the amount of time it took to perform an exam.  It covers the time period from when a patient is taken into an exam room until all analysis is completed (this is usually done after a patient has left).")),
                                             h4(HTML("<em>Report Time</em> is the period of time after an exam is completed until a preliminary report is ready for review by a doctor."))
                                                                                      )),
                                  column(6,
                                         h1("Terms you will see within the app"),
                                         box(background = "purple", width = 12, 
                                             h4("Inpatient - Patients admitted to the hospital"),
                                             h4("Outpatient - Patients seen in an outpatient clinic"),
                                             h4("Portable - inpatients who were unable to travel to the department"),
                                             h4("Contrast - Studies that utilized an image enhancing agent.  These studies require a nurse as well as a sonographer"),
                                             h4("Z-score - The average times per sonograhper were standardized using the formula:"),
                                             withMathJax(),
                                             h4('$$\\frac{Variable-mean(Variable)}{sd(variable)}$$')
                                         )))
                        ),
                        tabItem(tabName = "app",
                                fluidRow(
                                  column(3,
                                         box(width = 12, title = "Select subset of patients",
                                             selectizeInput("class", "Class", selected = "Outpatient", choices = levels(as.factor(df.csv$Class))))),
                                  column(6,
                                         tabBox(
                                           id = "tabset1", height = "500px", width = "250px",
                                           tabPanel("Scan Time", 
                                                    checkboxInput("contrastscan", "Contrast"),
                                                    br(),
                                                    conditionalPanel(condition = "input.class == 'Inpatient'", 
                                                                     checkboxInput("portablescan", "Portable")),
                                                    br(),
                                                    textOutput("attempt"),
                                                    conditionalPanel(condition="input.class == 'Outpatient' && input.portablescan", h4("Please return to the Inpatient data and unselect 'Portable' (either under 'Scan Time' or 'Report Time' tabs) in order to see Outpatient studies", style = "color:red;")),
                                                    plotlyOutput("medScanTimes"), 
                                                    verbatimTextOutput("click"), 
                                                    downloadButton("downloadplot1", "Download")
                                           ),
                                           tabPanel("Report Time", 
                                                    conditionalPanel(condition = "input.class == 'Inpatient'",
                                                      checkboxInput("portablereport", "Portable")),
                                                    textOutput("rt_text"),
                                                    br(),
                                                    conditionalPanel(condition="input.class == 'Outpatient' && input.portablereport", h4("Please return to the Inpatient data and unselect 'Portable' (either under 'Scan Time' or 'Report Time' tabs) in order to see Outpatient studies", style = "color:red;")),
                                                    plotlyOutput("medRepTimes"), 
                                                    verbatimTextOutput("info2"),
                                                    downloadButton("downloadplot2", "Download")),
                                           tabPanel("Dataset",
                                                    "A condensed sample of the dataset is shown here.  To download the complete dataset please click the download button at the bottom of the page", 
                                                    DT::dataTableOutput("table"),
                                                    downloadButton("downloadData", "Download"))
                                         )
                                  ))
                        )
                      )
                    ))