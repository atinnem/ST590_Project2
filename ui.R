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
                                             h4("Z-score - The average times per sonographer were standardized using the formula:"),
                                             withMathJax(),
                                             h4('$$\\frac{Variable-mean(Variable)}{sd(Variable)}$$')
                                         ))),
                              fluidRow(
                                column(12,
                                       h1("Why standardized scores?"),
                                       box(background = "purple", width = 24,
                                           h4("Displaying standardized scores proved to be invaluable in communicating the meaning of the analysis to managers and colleagues with no statistical background."),
                                           h4("With one glance you can easily appreciate staff who are outperforming their coworkers.  Staff were also able to easily see how they compared to one another. This level of intuitive understanding was lacking when the output was a plot of median times. ")))
                              ) 
                        ),
                        tabItem(tabName = "app",
                                fluidRow(
                                  column(3,
                                         box(width = 12, title = "Please select a subset of patients for analysis",
                                             selectizeInput("class", "Class", selected = "Outpatient", choices = levels(as.factor(df.csv$Class))),
                                             selectizeInput("contrast", "Contrast Usage", choices = levels(as.factor(df.csv$Contrast)), selected = "No Contrast"),
                                             conditionalPanel(condition = "input.class=='Inpatient'", 
                                                              selectizeInput("portable", "Portable", choices = levels(as.factor(df.csv$Portable)), selected = "Not Portable"))
                                             )),
                                  column(9,
                                         tabBox(
                                           id = "tabset1", height = "500px", width = "250px",
                                           tabPanel("Scan Time", 
                                                                                                       br(),
                                                    textOutput("attempt"),
                                                    conditionalPanel(condition="input.class == 'Outpatient' && input.portablescan", h4("Please return to the Inpatient data and unselect 'Portable' (either under 'Scan Time' or 'Report Time' tabs) in order to see Outpatient studies", style = "color:red;")),
                                                    plotOutput("medScanTimes", click = "plot_click"), 
                                                    h4("Click on the peak of a bar to view the staff_id and z_score."),
                                                    verbatimTextOutput("info"), 
                                                    plotOutput("stregression"),
                                                    downloadButton("downloadplot1", "Download")
                                           ),
                                           tabPanel("Report Time", 
                                                    #conditionalPanel(condition = "input.class == 'Inpatient'",
                                                      #checkboxInput("portablereport", "Portable")),
                                                    textOutput("rt_text"),
                                                    br(),
                                                    conditionalPanel(condition="input.class == 'Outpatient' && input.portablescan", h4("Please return to the Inpatient data and unselect 'Portable' (either under 'Scan Time' or 'Report Time' tabs) in order to see Outpatient studies", style = "color:red;")),
                                                    plotOutput("medRepTimes", click = "plot_click"), 
                                                    h4("Click on the peak of a bar to view the staff_id and z_score."),
                                                    verbatimTextOutput("info2"),
                                                    downloadButton("downloadplot2", "Download")),
                                           tabPanel("Dataset",
                                                    "A condensed sample of the dataset is shown here.  To download the complete dataset please click the download button at the bottom of the page", 
                                                    DT::dataTableOutput("table"),
                                                    downloadButton("downloadData", "Download")),
                                           tabPanel("Analysis", 
                                                    h4("words"),
                                                    uiOutput("text23"),
                                                    selectizeInput("Staff", "Staff", selected = "1", choices = seq(1:29)),
                                                    selectizeInput("num_clus", "Number of Clusters", selected = "29", choices = seq(1:100)),
                                                    plotOutput("cluster")
                                                    
                                                    #conditionalPanel(condition="input.class == 'Outpatient' && input.portablescan", h4("Please return to the Inpatient data and unselect 'Portable' (either under 'Scan Time' or 'Report Time' tabs) in order to see Outpatient studies", style = "color:red;"))
                                         )
                                  ))
                        )
                      )
                    )))