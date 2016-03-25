library(shiny)
library(ggplot2)
library(sqldf)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
     

        output$plot1<-renderPlot(
                
                
                if(input$select1==1) {
                        X<-data.frame(state=rep("",32), value=rep("",32))  
                        X[,2]<-aggregate(TOTAL_OPO~CVE_EDO, derrama,FUN=sum)[,2]/sum(as.numeric(derrama[,"TOTAL_OPO"]))*100
                        X[,1]<-unique(derrama[,2])
                        ggplot(X,aes(state,value, fill=value))+geom_bar(stat="identity")+
                                theme(axis.text.x=element_text(angle=90))
                                
                                
                        # qplot(y=value,  data=X, geom="bar", stat="identity")
                       
                }       
                else if(input$select1==2){   X<-data.frame(state=rep("",32), value=rep("",32))  
                        X[,2]<-aggregate(FAMS~CVE_EDO, derrama,FUN=sum)[,2]/sum(as.numeric(derrama[,"FAMS"]))*100
                        X[,1]<- unique(derrama[,2])
                        ggplot(X,aes(state,value, fill=state))+geom_bar(stat="identity")+
                                theme(axis.text.x=element_text(angle=90))
                       
                }
                else if(input$select1==5){   X<-data.frame(state=rep("",32), value=rep("",32))  
                X[,2]<-aggregate(TOT_INT~CVE_EDO, derrama,FUN=sum)[,2]/sum(as.numeric(derrama[,"TOT_INT"]))*100
                X[,1]<- unique(derrama[,2])
                ggplot(X,aes(state,value, fill=state))+geom_bar(stat="identity")+
                        theme(axis.text.x=element_text(angle=90))
                
                }
                
        )
                
        
        output$table <- DT::renderDataTable(DT::datatable({
                data <- derrama
                if (input$select1 ==1) {
                        if(input$radio==1){
                                
                                X<-data.frame(state=rep("",1), value=rep("",1))  
                                X[,2]<-sqldf("select sum(total_opo) Total from derrama")
                                X[,1]<-"Total Nacional"
                                data<-X
                                data          
                        }
                        else if (input$radio==2){
                                
                                data<-sqldf("select CVE_EDO, NOM_EDO, sum(total_opo) TOTAL
                                       from derrama 
                                       group by cve_edo
                                       order by cve_edo")
                        }
                        
                        else if (input$radio==3){
                                data<-sqldf("select CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN, sum(total_opo) TOTAL
                                            from derrama 
                                            group by cve_edo, nom_edo, cve_mun, nom_mun
                                            order by cve_edo, cve_mun")
                        }  
                        
                        else if (input$radio==4){
                                data<-sqldf("select CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN, CVE_LOC, NOM_LOC,sum(total_opo) TOTAL
                                            from derrama 
                                            group by cve_edo, nom_edo, cve_mun, nom_mun, cve_loc, nom_loc
                                            order by cve_edo, cve_mun, cve_loc")
                        }  
                                #X<-data.frame(state=rep("",32), value=rep("",32))  
                                #X[,2]<-aggregate(TOTAL_OPO~CVE_EDO, derrama,FUN=sum)[,2]
                                #X[,1]<-unique(derrama[,2])
                                #data<-X
                        }
                                
                
                
                
                if (input$select1==2) {
                        data <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
                }
                if (input$select1==3) {
                        data <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
                }
                if (input$select1==4) {
                        data <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
                }
                if (input$select1==5) {
                        data <- data[,c(1,2,3,4,5,6,7,8,35)]
                }
                if (input$select1==6) {
                        data <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
                }
                if (input$select1==7) {
                        data <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
                }
                if (input$select1==8) {
                        data <- data[,c(1,2,3,4,5,6,7,8,9,10,11)]
                }
                
                
                data
        }))
        

        
        
        })

        
        
        