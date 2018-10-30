#Amanda Tinnemore
#ST 590 Project 2
#October 2018

library(shiny)
library(tidyverse)
library(plotly)



df.csv<-read_csv("procedure_times.csv")

#changing name for ease of interpretation by others
colnames(df.csv)[colnames(df.csv)=="log_resptech"]<-"staff_id"


shinyServer(function(input, output, session) {
  
  #Overall scan time by patient class
  getscanDataall <- reactive({
    newData <- df.csv %>% filter(Class == input$class) %>% group_by(staff_id) %>% summarise(medscantime=median(Scan_Time)) %>% mutate(z_score = scale(medscantime))
  })
  
  #overall report time by patient class
  getReportDataall <- reactive({
    newData <- df.csv %>% filter(Class == input$class) %>% group_by(staff_id) %>% summarise(medreptime=median(Report_time)) %>% mutate(z_score = scale(medreptime))
  })
  
  #overall data set filtered by Class
  getallData<- reactive({
    newData1<-df.csv  %>% filter(Class == input$class)
  })
  
  #scan time by class and contrast
  getScanContrast<- reactive({
    contrastScan<-df.csv %>% filter(Contrast=="Contrast", Class==input$class) %>% group_by(staff_id) %>% summarise(medSTcontrast=median(Scan_Time))  %>% mutate(z_score = scale(medSTcontrast))
  })
  
  #scan time by class and portable
  getScanPortable<- reactive({
    portableScan<-df.csv %>% filter(Portable=="Portable", Class==input$class) %>% group_by(staff_id) %>% summarise(medSTport=median(Scan_Time)) %>% mutate(z_score = scale(medSTport))
  })
  
  #scan time by clas, portable, contrast
  getScanboth<- reactive({
    contrastScan<-df.csv %>% filter(Contrast=="Contrast", Class==input$class, Portable =="Portable") %>% group_by(staff_id) %>% summarise(medSTboth=median(Scan_Time))%>% mutate(z_score = scale(medSTboth))
  })
  
  #report time by class and portable
  getReportPort<- reactive({
    reportPort<-df.csv %>% filter(Portable=="Portable", Class==input$class) %>% group_by(staff_id) %>% summarise(medRTport=median(Report_time))%>% mutate(z_score = scale(medRTport))
  })
  
  #create plotting function for Scan Time
  scanTimeinput<-reactive({
    scanContrast<-getScanContrast()
    scanboth<-getScanboth()
    overall<-getscanDataall()
    scanportable<-getScanPortable()
    
    
    if(input$contrastscan & input$portablescan){
      g<-ggplot(scanboth, aes(x=staff_id, y = z_score)) + geom_bar(stat = "identity") 
      
      #+ geom_hline(aes(yintercept = median(medSTboth)), color = "red")
    } else if(input$contrastscan){
      g<-ggplot(scanContrast, aes(x=staff_id, y = z_score)) + geom_bar(stat = "identity") 
      
      #geom_hline(aes(yintercept = median(medSTcontrast)), color = "red")
    } else if(input$portablescan){
      g<-ggplot(scanportable, aes(x=staff_id, y = z_score)) + geom_bar(stat = "identity") 
      
      #geom_hline(aes(yintercept = median(medSTport)), color = "red")
    } else {
      g<-ggplot(overall, aes(x=staff_id, y = z_score))+ geom_bar(stat = "identity") 
      
      #+ geom_hline(aes(yintercept = median(medscantime)), color = "red") + scale_y_continuous(limits = c(0,125), breaks = seq(0,125, length.out = 6), labels = c("00:00:00", "00:20:00", "00:40:00", "01:00:00", "01:20:00", "01:40:00"))
    }
    ggplotly(g)
  })
  
  #Text to display lab medians for selected subgroups
  output$attempt<-renderText({
    scanContrast<-getScanContrast()
    scanboth<-getScanboth()
    overall<-getscanDataall()
    scanportable<-getScanPortable()
    
    if(input$contrastscan & input$portablescan){
    paste0("The median scan time for the lab is ", median(scanboth$Scan_Time), " (in hh:mm:ss format)")
  } else if(input$contrastscan){
    paste0("The median scan time for the lab is ", median(scanContrast$Scan_Time), " (in hh:mm:ss format)")
    }  else if(input$portablescan){
      paste0("The median scan time for the lab is ", median(scanportable$Scan_Time), " (in hh:mm:ss format)")
    }else {
      paste0("The median scan time for the lab is ", median(overall$Scan_Time), " (in hh:mm:ss format)")
    }
    })
  
  
  #plot z score of Scan Times
  output$medScanTimes <- renderPlotly({
    scanTimeinput()
    
  })
  
  
  # plot z score of Report Times
  output$medRepTimes <- renderPlotly({
    #get filtered data
    report_port<-getReportPort()
    repoverall<-getReportDataall()
    
    
    if(input$portablereport){
      g<-ggplot(report_port, aes(x=staff_id, y = z_score)) +geom_bar(stat = "identity") 
      #+ geom_hline(aes(yintercept = median(medRTport)), color = "red")
    } else {
      g<-ggplot(repoverall, aes(x=staff_id, y = z_score))+ geom_bar(stat = "identity") 
      #+ geom_hline(aes(yintercept = median(medreptime)), color = "red")
    }
    ggplotly(g)
  })
  # report Time click table 
  output$info2<-renderPrint({
    event_data("plotly_click")
    
  })
  # Scan Time click table 
  output$click<-renderPrint({
    event_data("plotly_click")
    
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
    write.csv(df.csv, file)
  })

  
  
  #dataset
  output$table <-DT::renderDataTable({
    datatabe<-getallData()
    new<-datatabe %>% select(staff_id, Class, Scan_Time, Report_time, Portable, Contrast)
    DT::datatable(new, options = list(pageLength = 15))
  })
})
