#Amanda Tinnemore
#ST503
#Project 2

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(tree)
library(gbm)
library(lubridate)
library(class)


df.csv<-read_csv("procedure_times.csv")

#changing name for ease of interpretation by others
colnames(df.csv)[colnames(df.csv)=="log_resptech"]<-"staff_id"

#remove outliers and create factors
df.csv$Class<-as.factor(df.csv$Class)
df.csv$Contrast<-as.factor(df.csv$Contrast)
df.csv$Portable<-as.factor(df.csv$Portable)
df.csv$staff_id<-as.factor(df.csv$staff_id)

levels(df.csv$Contrast)[levels(df.csv$Contrast)=="no Contrast"] <- "No Contrast"

df.csv$Scan_Time<-hms(df.csv$Scan_Time)
df.csv$Scan_Time<-as.numeric(df.csv$Scan_Time)

df.csv$Report_time<-hms(df.csv$Report_time) 
df.csv$Report_time<-as.numeric(df.csv$Report_time)  

df.csv$Scan_Time<-df.csv$Scan_Time/60
df.csv$Report_time<-round(df.csv$Report_time/60,2)

df.csv<-df.csv %>% filter(Scan_Time<9000 & Scan_Time> 5) %>% filter(Report_time<12)

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
                                                    conditionalPanel(condition="input.class == 'Outpatient' && input.portable == 'Portable'", h4("Please return to the Inpatient data and unselect 'Portable' in order to see Outpatient studies", style = "color:red;")),
                                                    plotOutput("medScanTimes", click = "plot_click"), 
                                                    h4("Click on the peak of a bar to view the staff_id and z_score."),
                                                    verbatimTextOutput("info"), 
                                                    #plotOutput("stregression"),
                                                    downloadButton("downloadplot1", "Download")
                                           ),
                                           tabPanel("Report Time", 
                                                    br(),
                                                    textOutput("rt_text"),
                                                    br(),
                                                    conditionalPanel(condition="input.class == 'Outpatient' && input.portable == 'Portable'", h4("Please return to the Inpatient data and unselect 'Portable' in order to see Outpatient studies", style = "color:red;")),
                                                    plotOutput("medRepTimes", click = "plot_click"), 
                                                    h4("Click on the peak of a bar to view the staff_id and z_score."),
                                                    verbatimTextOutput("info2"),
                                                    downloadButton("downloadplot2", "Download")),
                                           tabPanel("Dataset",
                                                    "A condensed sample of the dataset is shown here.  To download the complete dataset please click the download button at the bottom of the page", 
                                                    DT::dataTableOutput("table"),
                                                    downloadButton("downloadData", "Download")),
                                           tabPanel("Analysis", 
                                                    h4("Let's make some predictions! Select the time frame and variables you would like to analyze. Class, Contrast Usage, Staff ID, and Portable variables can all be used to specify your prediction.  You will be able to compare the prediction from both a linear and tree model."),
                                                    selectizeInput("type", "Response Variable", selected = "Scan_Time", choices = c("Scan_Time", "Report_time")),
                                                    selectizeInput("Staff", "Staff ID", selected = "1", choices = seq(1:29)),
                                                    selectizeInput("n_trees", "Number of Trees", selected = 1000, choices = seq(1000,10000, by = 1000)),
                                                    uiOutput("text23"),
                                                    uiOutput("text24"),
                                                    br(),
                                                    br(),
                                                    h4("Below is the result of unsupervised learning analysis.  Please specify the desired method, number of clusters, and point size."),
                                                    selectizeInput("cluster", "Type of Clustering", selected = "K Means", choices = c("K Means", "Hierarchial")),
                                                    selectizeInput("num_clus", "Number of Clusters", selected = "30", choices = c(10:49)),
                                                    sliderInput("size", "Size of Points on Graph",min = .1, max = 5, value = 1, step = .1),
                                                    plotOutput("cluster")
                                                    
                                                    
                                         )
                                  ))
                        )
                      )
                    )))