library(shiny)
library(ggplot2)
library(sqldf)
library(foreign)

#Getting enconding to use data in spanish
options(encoding="ISO8859-1")
Sys.setlocale("LC_ALL", "pt_PT.ISO8859-1")

#Setting data
DERRAMA$TOTAL_OPO<-as.numeric(DERRAMA$TOTAL_OPO)
DERRAMA$FAMS_HOM<-as.numeric(DERRAMA$FAMS_HOM)
data <- DERRAMA

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
     
       # Grafica
        output$plot1<-renderPlot(
                
                # "Apoyos Emitidos" case
                if(input$select1==1) {
                        X<-data.frame(state=rep("",32), value=rep("",32))  
                        X[,2]<-aggregate(TOTAL_OPO~CVE_EDO, DERRAMA,FUN=sum)[,2]/sum(as.numeric(DERRAMA[,"TOTAL_OPO"]))*100
                        X[,1]<-unique(DERRAMA[,2])
                        ggplot(X,aes(state,value, fill=value))+geom_bar(stat="identity")+
                                theme(axis.text.x=element_text(angle=90))
                }

                # "Familias" case
                else if(input$select1==2){
                        X<-data.frame(state=rep("",32), value=rep("",32))  
                        X[,2]<-aggregate(FAMS~CVE_EDO, DERRAMA,FUN=sum)[,2]/sum(as.numeric(DERRAMA[,"FAMS"]))*100
                        X[,1]<- unique(DERRAMA[,2])
                        ggplot(X,aes(state,value, fill=state))+geom_bar(stat="identity")+
                                 theme(axis.text.x=element_text(angle=90))
                }

                # "Integrantes" case
                else if(input$select1==5){
                        X<-data.frame(state=rep("",32), value=rep("",32))  
                        X[,2]<-aggregate(TOT_INT~CVE_EDO, DERRAMA,FUN=sum)[,2]/sum(as.numeric(DERRAMA[,"TOT_INT"]))*100
                        X[,1]<- unique(DERRAMA[,2])
                        ggplot(X,aes(state,value, fill=state))+geom_bar(stat="identity")+
                                theme(axis.text.x=element_text(angle=90))
                        }
        )
                
        #Tabla
        output$table <- DT::renderDataTable(DT::datatable({
                
                # "Apoyos Emitidos" case
                if (input$select1 ==1) {
                        
                        # "Nacional" case
                        if(input$radio==1){
                                X<-data.frame(ESTADO=rep("",1), TOTAL=rep("",1))  
                                X[,1]<-"Total Nacional"
                                X[,2]<-format(sqldf("select sum(total_opo) from DERRAMA"), 
                                              digits=20, nsmall=2, decimal.mark=".", big.mark=",")
                                data<-X
                                data          
                        }
                        
                        # "Estatal" case
                        else if (input$radio==2){
                                
                                data<-sqldf("SELECT D.CVE_EDO, D.NOM_EDO, SUM(D.TOTAL_OPO) TOTAL,
                                             SUM(D.TOTAL_OPO)/T.TOTAL*100 '%' 
                                             FROM DERRAMA D, (SELECT SUM(TOTAL_OPO) TOTAL FROM DERRAMA) T
                                             GROUP BY CVE_EDO, NOM_EDO
                                             ORDER BY CVE_EDO")
                                data$TOTAL<-format(data$TOTAL, digits=20, nsmall=2, decimal.mark=".", big.mark = "," )
                                data
                        }
                        
                        # "Municipal" case
                        else if (input$radio==3){
                                data<-sqldf("SELECT D.CVE_EDO, D.NOM_EDO, D.CVE_MUN, D.NOM_MUN, 
                                             SUM(D.TOTAL_OPO) TOTAL, SUM(D.TOTAL_OPO)/T.TOTAL*100 '%' 
                                             FROM DERRAMA D, (SELECT SUM(TOTAL_OPO) TOTAL FROM DERRAMA) T
                                             group by cve_edo, nom_edo, cve_mun, nom_mun
                                             order by cve_edo, cve_mun")
                        } 
                        
                        # "Localidad" case
                        else if (input$radio==4){
                                data<-sqldf("select CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN, CVE_LOC, NOM_LOC,sum(total_opo) TOTAL
                                            from DERRAMA 
                                            group by cve_edo, nom_edo, cve_mun, nom_mun, cve_loc, nom_loc
                                            order by cve_edo, cve_mun, cve_loc")
                        }  
                               
                }
                                
                
                
                # "Familias" case
                
                if (input$select1==2) {
                        # "Nacional" case
                        if(input$radio==1){
                                X<-data.frame(ESTADO=rep("",1), FAMS_HOM=rep("",1),
                                              FAMS_MUJ=rep("",1), FAMS=rep("",1))
                                X[,1]<-"Total Nacional"
                                X[,2]<-sqldf("SELECT SUM(FAMS_HOM) FAMS_HOM 
                                              FROM DERRAMA") 
                                X[,3]<-sqldf("SELECT SUM(FAMS_MUJ) FAMS_MUJ
                                              FROM DERRAMA")
                                X[,4]<-sqldf("SELECT SUM(FAMS) FAMS
                                              FROM DERRAMA")
                                data<-X
                                data          
                        }
                        
                        # "Estatal" case
                        else if (input$radio==2){
                                
                                data<-sqldf("SELECT CVE_EDO, NOM_EDO, SUM(FAMS_HOM) FAMS_HOM, 
                                             SUM(FAMS_MUJ) FAMS_MUJ, 
                                             SUM(FAMS) FAMS
                                             FROM DERRAMA
                                             GROUP BY CVE_EDO, NOM_EDO 
                                             ORDER BY CVE_EDO, NOM_EDO")
                        }
                        
                        # "Municipal" case
                        else if (input$radio==3){
                                data<-sqldf("SELECT CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN,
                                             SUM(FAMS_HOM) FAMS_HOM, 
                                             SUM(FAMS_MUJ) FAMS_MUJ, 
                                             SUM(FAMS) FAMS
                                             FROM DERRAMA
                                             GROUP BY CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN  
                                             ORDER BY CVE_EDO, CVE_MUN")
                        } 
                        
                        # "Localidad" case
                        else if (input$radio==4){
                                data<-sqldf("SELECT CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN, CVE_LOC, NOM_LOC,
                                             SUM(FAMS_HOM) FAMS_HOM, 
                                             SUM(FAMS_MUJ) FAMS_MUJ, 
                                             SUM(FAMS) FAMS
                                             FROM DERRAMA
                                             GROUP BY CVE_EDO, NOM_EDO, CVE_MUN, NOM_MUN, CVE_LOC, NOM_LOC  
                                             ORDER BY CVE_EDO, CVE_MUN, CVE_LOC")
                        }  
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

        
        
        