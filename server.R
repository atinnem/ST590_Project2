#Amanda Tinnemore
#ST 590 Project 2
#October 2018

library(shiny)
library(tidyverse)




df.csv<-read_csv("procedure_times.csv")


shinyServer(function(input, output, session) {
  #Overall scan time by patient class
  getscanDataall <- reactive({
    newData <- df.csv %>% filter(Class == input$class) %>% group_by(log_resptech) %>% summarise(medscantime=median(Scan_Time))
  })
  
  #overall report time by patient class
  getReportDataall <- reactive({
    newData <- df.csv %>% filter(Class == input$class) %>% group_by(log_resptech) %>% summarise(medreptime=median(Report_time))
  })
  
  #overall data set filtered by Class
  getallData<- reactive({
    newData1<-df.csv  %>% filter(Class == input$class)
  })
  
  #scan time by class and contrast
  getScanContrast<- reactive({
    contrastScan<-df.csv %>% filter(Contrast=="Contrast", Class==input$class) %>% group_by(log_resptech) %>% summarise(medSTcontrast=median(Scan_Time))
  })
  
  #scan time by class and portable
  getScanPortable<- reactive({
    portableScan<-df.csv %>% filter(Portable=="Portable", Class==input$class) %>% group_by(log_resptech) %>% summarise(medSTport=median(Scan_Time))
  })
  
  #scan time by clas, portable, contrast
  getScanboth<- reactive({
    contrastScan<-df.csv %>% filter(Contrast=="Contrast", Class==input$class, Portable =="Portable") %>% group_by(log_resptech) %>% summarise(medSTboth=median(Scan_Time))
  })
  
  #report time by class and portable
  getReportPort<- reactive({
    reportPort<-df.csv %>% filter(Portable=="Portable", Class==input$class) %>% group_by(log_resptech) %>% summarise(medRTport=median(Report_time))
  })
  
  #create plotting function for Scan Time
  scanTimeinput<-reactive({
    scanContrast<-getScanContrast()
    scanboth<-getScanboth()
    overall<-getscanDataall()
    scanportable<-getScanPortable()
    
    
    if(input$contrastscan & input$portablescan){
      g<-ggplot(scanboth, aes(x=log_resptech, y = medSTboth))
      g + geom_bar(stat = "identity") + geom_hline(aes(yintercept = median(medSTboth)), color = "red")
    } else if(input$contrastscan){
      g<-ggplot(scanContrast, aes(x=log_resptech, y = medSTcontrast))
      g + geom_bar(stat = "identity") + geom_hline(aes(yintercept = median(medSTcontrast)), color = "red")
    } else if(input$portablescan){
      g<-ggplot(scanportable, aes(x=log_resptech, y = medSTport))
      g + geom_bar(stat = "identity") + geom_hline(aes(yintercept = median(medSTport)), color = "red")
    } else {
      g<-ggplot(overall, aes(x=log_resptech, y = medscantime))
      g + geom_bar(stat = "identity") + geom_hline(aes(yintercept = median(medscantime)), color = "red") + scale_y_continuous(limits = c(0,125), breaks = seq(0,125, length.out = 6), labels = c("00:00:00", "00:20:00", "00:40:00", "01:00:00", "01:20:00", "01:40:00"))
    }
  })
  
  #Scan Times
  output$medScanTimes <- renderPlot({
    scanTimeinput()
    })
    
  
  #Report Times
  output$medRepTimes <- renderPlot({
    #get filtered data
    report_port<-getReportPort()
    repoverall<-getReportDataall()

    
    if(input$portablereport){
      g<-ggplot(report_port, aes(x=log_resptech, y = medRTport))
      g + geom_bar(stat = "identity") + geom_hline(aes(yintercept = median(medRTport)), color = "red")
    } else {
      g<-ggplot(repoverall, aes(x=log_resptech, y = medreptime))
      g + geom_bar(stat = "identity") + geom_hline(aes(yintercept = median(medreptime)), color = "red")
    }
  })
    
  
  
  # Scan Time click table 
  output$info<-renderPrint({
    scanContrast<-getScanContrast()
    scanboth<-getScanboth()
    overall<-getscanDataall()
    scanportable<-getScanPortable()
    
    
    if(input$contrastscan & input$portablescan){
      nearPoints(scanboth, input$plot_click, threshold = 20, maxpoints = 1, addDist = TRUE)
    } else if(input$contrastscan){
      nearPoints(scanContrast, input$plot_click, threshold = 20, maxpoints = 1, addDist = TRUE)
          } else if(input$portablescan){
      nearPoints(scanportable, input$plot_click, threshold = 20, maxpoints = 1, addDist = TRUE)
      
    } else {
      nearPoints(overall, input$plot_click,addDist = TRUE)
    }
    
  
  })
  
  #Report time click table
  output$info2<-renderPrint({
    
    #get filtered data
    report_port<-getReportPort()
    repoverall<-getReportDataall()
    
    if(input$portablereport){
      nearPoints(report_port, input$plot_click,addDist = TRUE)
    } else {
      nearPoints(repoverall, input$plot_click,addDist = TRUE)
    }
  })
  
  
  #download scan time plot
  output$downloadplot1<-downloadHandler(
    
    filename = function(){
    paste("ScanTime", input$class, ".png", sep = "")}, 
      content = function(file){
   ggsave(file, scanTimeinput())
  
  })
  
  #data table download
  output$downloadData<-downloadHandler(filename = "procedure_times.csv", content = function(file){
    write.csv(getallData(), file)
  })
 
  
  

  
  #dataset
  output$table <-renderDataTable({
    getallData()
  })
 })
    

  

