#Amanda Tinnemore
#ST503
#Project 2

library(shiny)
library(shinydashboard)



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
                            h4("testing"))),
          column(6,
                 h1("How to use this app?"),
                 box(background = "blue", width = 12, h4("more words")))
      )),
      tabItem(tabName = "app",
              fluidRow(
                column(3,
                       h1("here goes nothing"),
                       box(width = 12, title = "Choose patient class",
                           selectizeInput("class", "Class", selected = "Outpatient", choices = levels(as.factor(df.csv$Class))))),
                column(6,
                       tabBox(
                         id = "tabset1", height = "500px", width = "250px",
                         tabPanel("Scan Time", "Patient time in room",
                                  checkboxInput("contrastscan", "Contrast"),
                                  conditionalPanel(condition = "input.class == 'Inpatient'", 
                                         checkboxInput("portablescan", "Portable")),
                                  plotOutput("medScanTimes", click = "plot_click"), 
                                  verbatimTextOutput("info"), downloadButton("downloadplot1", "Download")
                                  #conditionalPanel(condition="input.class==Inpatient && input.portablereport !== NULL", textInput("Please uncheck 'portable' on the report tab before proceeding"))
                                  ),
                         tabPanel("Report Time", 
                                  "Amount of Time to Enter a Report",
                                  conditionalPanel(
                                    condition = "input.class == 'Inpatient'",
                                    checkboxInput("portablereport", "Portable")),
                                  plotOutput("medRepTimes", 
                                     click = "plot_click"), 
                                  verbatimTextOutput("info2"), downloadButton("downloadplot2", "Download")),
                         tabPanel("Dataset",
                                  "dataset goes here", dataTableOutput("table"),downloadButton("downloadData", "Download"))
                       )
        ))
    )
    )
  ))
