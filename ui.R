#Amanda Tinnemore
#ST503
#Project 2

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)

df.csv<-read_csv("procedure_times.csv")

ui <- dashboardPage(skin="blue",
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
                                         box(background = "blue", width = 12,
                                             h4("The data in this app is from an echocardiography lab.  To learn more about echocardiography click here. "),
                                             h4("Staff members are allotted a certain amount of time to complete a study and each quarter this data is reviewed to analyze trends, tweak scheduling, and potentially justify additional personnel.  It also serves to identify both efficient and less than efficient staff members."),
                                             h4("The time frames analyzed are “Scan Time” and “Report Time”."),
                                             h4("“Scan Time” is the amount of time it took to perform an exam.  It covers the time period from when a patient is taken into an exam room until the analysis is completed (this is usually done after a patient has left).   The period of time after an exam is completed until a report is printed for review by a doctor is “Report Time”."),
                                             h4(a("click here", href="https://www.mayoclinic.org/tests-procedures/echocardiogram/about/pac-20393856"))
                                         )),
                                  column(6,
                                         h1("Terms you will see within the app"),
                                         box(background = "blue", width = 12, 
                                             h4("Inpatient - Patients admitted to the hospital"),
                                             h4("Outpatient - Patients seen in an outpatient clinic"),
                                             h4("Portable - inpatients who were unable to travel to the department"),
                                             h4("Contrast - Studies that utilized an image enhancing agent.  These studies require a nurse as well as a sonographer"),
                                             h4("Z-score - The average times per sonograhper were standardized using the formula:"),
                                             withMathJax(),
                                             h4('$$\\frac{var-mean(var)}{sd}$$')
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
                                           tabPanel("Scan Time", "Patient time in room",
                                                    checkboxInput("contrastscan", "Contrast"),
                                                    conditionalPanel(condition = "input.class == 'Inpatient'", 
                                                                     checkboxInput("portablescan", "Portable")),
                                                    textOutput("attempt"),
                                                    plotlyOutput("medScanTimes"), 
                                                    verbatimTextOutput("click"), 
                                                    downloadButton("downloadplot1", "Download")
                                                    #conditionalPanel(condition="input.class==Inpatient && input.portablereport !== NULL", textInput("Please uncheck 'portable' on the report tab before proceeding"))
                                           ),
                                           tabPanel("Report Time", 
                                                    "Amount of Time to Enter a Report",
                                                    conditionalPanel(
                                                      condition = "input.class == 'Inpatient'",
                                                      checkboxInput("portablereport", "Portable")),
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