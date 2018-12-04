#Amanda Tinnemore
#ST 590 Project 2
#October 2018

library(shiny)
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



df.csv<-df.csv %>% filter(Scan_Time<9000 & Scan_Time> 500) %>% filter(Report_time<20000)

shinyServer(function(input, output, session) {
  
 


  #create linear regression fit to be used for prediction
  
  
  
  
  #subset data to be used in prediction/analysis
  
  getport_cont<-reactive({df.csv %>% filter(Class == input$class, Portable == "Portable", Contrast == "Contrast")})
  getport<-reactive({df.csv %>% filter(Class == "Inpatient", Portable == "Portable")})
  getcont<-reactive({df.csv %>% filter(Class == input$class, Contrast == "Contrast")})
  
  #Overall scan time by patient class
  getscanDataall <- reactive({
    newData <- df.csv %>% filter(Class == input$class) %>% group_by(staff_id) %>% summarise(medscantime=median(Scan_Time)) %>% mutate(z_score = scale(medscantime)) %>% select(-medscantime)
  })
  
  #overall report time by patient class
  getReportDataall <- reactive({
    newData <- df.csv %>% filter(Class == input$class) %>% group_by(staff_id) %>% summarise(medreptime=median(Report_time)) %>% mutate(z_score = scale(medreptime))%>% select(-medreptime)
  })
  #overall report time by patient class - USE
  getattempt <- reactive({
    attempt <- df.csv %>% filter(Class == input$class, Portable == input$portable, Contrast ==input$contrast) %>% group_by(staff_id) %>% summarise(medreptime=median(Report_time)) %>% mutate(z_score = scale(medreptime))%>% select(-medreptime)
  })
  
  getattempt2 <- reactive({
    attempt <- df.csv %>% filter(Class == input$class, Portable == input$portable, Contrast ==input$contrast) 
  })
  
  #overall data set filtered by Class
  getallData<- reactive({
    newData1<-df.csv  %>% filter(Class == input$class)
  })
  
  #scan time by class and contrast
  getScanContrast<- reactive({
    contrastScan<-df.csv %>% filter(Contrast=="Contrast", Class==input$class) %>% group_by(staff_id) %>% summarise(medSTcontrast=median(Scan_Time))  %>% mutate(z_score = scale(medSTcontrast))%>% select(-medSTcontrast)
  })
  
  #scan time by class and portable
  getScanPortable<- reactive({
    portableScan<-df.csv %>% filter(Portable=="Portable", Class==input$class) %>% group_by(staff_id) %>% summarise(medSTport=median(Scan_Time)) %>% mutate(z_score = scale(medSTport))%>% select(-medSTport)
  })
  
  #scan time by clas, portable, contrast
  getScanboth<- reactive({
    contrastScan<-df.csv %>% filter(Contrast=="Contrast", Class==input$class, Portable =="Portable") %>% group_by(staff_id) %>% summarise(medSTboth=median(Scan_Time))%>% mutate(z_score = scale(medSTboth))%>% select(-medSTboth)
  })
  
  #report time by class and portable
  getReportPort<- reactive({
    reportPort<-df.csv %>% filter(Portable=="Portable", Class==input$class) %>% group_by(staff_id) %>% summarise(medRTport=median(Report_time))%>% mutate(z_score = scale(medRTport))%>% select(-medRTport)
  })
  
  
  #create plotting function for Scan Time
  scanTimeinput<-reactive({
    port_cont<-getport_cont()
    port<- getport()
    cont<-getcont()
    
    if(input$contrast=="Contrast" & input$portable=="Portable"){
      ggplot(port_cont, aes(x = Report_time, y = Scan_Time)) + geom_point() + geom_smooth(method = "lm")
    } else if(input$contrast=="Contrast"){
      ggplot(cont, aes(x = Report_time, y = Scan_Time)) + geom_point(aes(col = Contrast)) + geom_smooth(method = "lm", aes(col=Contrast))
    } else if(input$portable=="Portable"){
      ggplot(port, aes(x = Report_time, y = Scan_Time)) + geom_point(aes(col = Portable)) + geom_smooth(method = "lm", aes(col=Portable))
    } else {
      ggplot(df.csv, aes(x = Report_time, y = Scan_Time)) + geom_point() + geom_smooth(method = "lm")
    }
    
  })
  
  scanTimeinput2<-reactive({
    scanContrast<-getScanContrast()
    scanboth<-getScanboth()
    overall<-getscanDataall()
    scanportable<-getScanPortable()
    
    
    if(input$contrast=="Contrast" & input$portable=="Portable"){
      g<-ggplot(scanboth, aes(x=staff_id, y = z_score))
    } else if(input$contrast=="Contrast"){
      g<-ggplot(scanContrast, aes(x=staff_id, y = z_score))
    } else if(input$portable=="Portable"){
      g<-ggplot(scanportable, aes(x=staff_id, y = z_score))
    } else {
      g<-ggplot(overall, aes(x=staff_id, y = z_score))
    }
    g + geom_bar(stat = "identity")+labs(x = "Staff ID", y = "Z Score")
  })
  #Text to display lab medians of Scan Time for selected subgroups
  output$attempt<-renderText({
   st_text<-getallData()
   scanportable<-getScanPortable()
   
   if(input$contrast=="Contrast" & input$portable=="Portable"){
     st_text1<- st_text %>% filter(Contrast == "Contrast", Portable =="Portable")
      paste( "The overall median Scan Time for this subset of patients is ", round(median(st_text1$Scan_Time)/60,1), " minutes.", sep = "")
   } else if(input$portable=="Portable"){
     st_text2<-st_text %>% filter(Portable =="Portable")
     paste("The overall median Scan Time for this subset of patients is ", round(median(st_text2$Scan_Time)/60,1), " minutes.", sep = "")
   }  else if(input$contrast=="Contrast"){
      st_text3 <- st_text %>% filter(Contrast == "Contrast")
     paste("The overall median Scan Time for this subset of patients is ", round(median(st_text3$Scan_Time)/60,1), " minutes", sep = "")
    }
       else {
     paste("The overall median Scan Time for this subset of patients is ", round(median(st_text$Scan_Time)/60,1), " minutes", sep = "")
   }
    })
  
  
  #plot z score of Scan Times
  output$medScanTimes <- renderPlot({
    scanTimeinput2()
    
  })
  
reptimeinput<-reactive({
  repoverall<-getattempt()
    ggplot(repoverall, aes(x=staff_id, y = z_score))+ geom_bar(stat = "identity") +labs(x = "Staff ID", y = "Z Score")
    
  
})
  
  # plot z score of Report Times
  output$medRepTimes <- renderPlot({
    #get filtered data
    repoverall<-getattempt()
   #repoverall<-repoverall %>% filter(Portable == 'input$portable', Contrast == 'input$contrast')
    
    
      ggplot(repoverall, aes(x=staff_id, y = z_score))+ geom_bar(stat = "identity") +labs(x = "Staff ID", y = "Z Score")
       
  })
  
  
  # report Time click table 
  output$info2<-renderPrint({
    
    #get filtered data
    #report_port<-getReportPort()
   # repoverall<-getReportDataall()
    repoverall<-getattempt()
    
    
     
      nearPoints(repoverall, input$plot_click,threshold = 20, maxpoints = 1)
    
  })
  
 
  
  
  #Text to display lab medians of Report Time for selected subgroups
  output$rt_text<-renderText({
    rt_text1<-getattempt2()
    
      paste("The overall median Report Time for this subset of patients is ", round(median(rt_text1$Report_time)/60,1), " minutes.", sep = "")
 
  })
  
  # Scan Time click table 
  output$info<-renderPrint({
    scanContrast<-getScanContrast()
    scanboth<-getScanboth()
    overall<-getscanDataall()
    scanportable<-getScanPortable()
    
    
    if(input$contrast=="Contrast" & input$portable=="Portable"){
      nearPoints(scanboth, input$plot_click, threshold = 20, maxpoints = 1 )
    } else if(input$contrast=="Contrast"){
      nearPoints(scanContrast, input$plot_click, threshold = 20, maxpoints = 1)
    } else if(input$portable=="Portable"){
      nearPoints(scanportable, input$plot_click, threshold = 20, maxpoints = 1)
      
    } else {
      nearPoints(overall, input$plot_click,threshold = 20, maxpoints = 1)
    }
    
    
  })
  


  
  #download scan time plot
  output$downloadplot1<-downloadHandler(
    
    filename = function(){
      paste("ScanTime", input$class, ".png", sep = "")}, 
    content = function(file){
      ggsave(file, scanTimeinput2())
      
    })
  
  #download report time plot
  output$downloadplot2<-downloadHandler(
    
    filename = function(){
      paste("ReportTime", input$class, ".png", sep = "")}, 
    content = function(file){
      ggsave(file, reptimeinput())
      
    })
  
  #data table download
  output$downloadData<-downloadHandler(filename = "procedure_times.csv", content = function(file){
    new_df<-table()
    write.csv(new_df, file)
  })

  table<-reactive({
    datatabe<-getallData()
    new<-datatabe %>% filter(Class ==input$class, Portable ==input$portable, Contrast == input$contrast) %>% select(staff_id, Class, Scan_Time, Report_time, Portable, Contrast)
  })
  
  #dataset
  output$table <-DT::renderDataTable({
    datatabe<-getallData()
    new<-datatabe %>% filter(Class ==input$class, Portable ==input$portable, Contrast == input$contrast) %>% select(staff_id, Class, Scan_Time, Report_time, Portable, Contrast)
    DT::datatable(new, options = list(pageLength = 10))
  })


getClass<-reactive({
  Class <-input$class
})


###this works.  
predictLM<-reactive({
  if(input$type=="Scan_Time"){
    fit<-lm(Scan_Time ~ Class + staff_id + Portable + Contrast, data = df.csv)
  } else{
    fit<-lm(Report_time ~ Class + staff_id + Portable + Contrast, data = df.csv)
  }
  
  predict(fit, data.frame(Class = c(input$class),staff_id=c(input$Staff), Portable = c(input$portable), Contrast = input$contrast))
})

predictTree<-reactive({
  if(input$type=="Scan_Time"){
  #fitTree<-tree(Scan_Time~ Class + Contrast + Portable + staff_id, data = df.csv)
    boostFit<-gbm(Scan_Time~Class + Contrast + Portable + staff_id, data = df.csv, distribution = "gaussian", n.trees = input$n_trees, shrinkage = 0.1, interaction.depth = 4)} else{
    #fitTree<-tree(Report_time~ Class + Contrast + Portable + staff_id, data = df.csv
      boostFit<-gbm(Report_time~Class + Contrast + Portable + staff_id, data = df.csv, distribution = "gaussian", n.trees = input$n_trees, shrinkage = 0.1, interaction.depth = 4)}
  predict(boostFit,data.frame(Class = c(input$class), Contrast = c(input$contrast), Portable = c(input$portable), staff_id = c(input$Staff)), n.trees = input$n_trees)
  })

#Text to display predictions from lm analysis

output$text23<-renderUI({
  Class<-getClass()
  text<-paste("Prediction of ", input$type, " using a linear model: ", round(predictLM()/60,1), " minutes.")

  h3(text)
})

output$text24<-renderUI({
  text<-paste("Prediction of ", input$type, "using a tree model: ", round(predictTree()/60,1), " minutes.")
  
  h3(text)
})



output$cluster<-renderPlot({
  if(input$cluster == "K Means"){
  cluster<-kmeans(df.csv[,3:4], input$num_clus, nstart = 20)
  cluster$cluster<-as.factor(cluster$cluster)
  ggplot(df.csv, aes(Scan_Time, y = Report_time, color = cluster$cluster)) + geom_point(size = input$size) + labs(x= "Scan Time", y = "Report Time", title = "K Means Cluster Analysis") + scale_color_discrete(name = "# of Clusters")
  }else{
  
  hierClust<-hclust(dist(data.frame(df.csv$Scan_Time, df.csv$Report_time)), method = "average")
  
  hier5<-cutree(hierClust, k =input$num_clus)
  
  
  df_members<- cbind(df.csv, hier5)
  #plot(df_members$Scan_Time, df_members$Report_time, col = df_members$hier5)
  ggplot(df_members, aes(Scan_Time, y = Report_time, color =df_members$hier5)) + geom_point(size = input$size)+labs(x= "Scan Time", y = "Report Time", title = "Hierarchial Cluster Analysis")+ scale_colour_gradientn(colours = terrain.colors(10), name = "# of Clusters")
  }
})


})