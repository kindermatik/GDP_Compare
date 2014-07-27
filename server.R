library(shiny)
library(googleVis) #package to create graphs
source("aux_functions.R",local=TRUE) #loads formulas to subset data

shinyServer(function(input, output) {
     
     #Selects countries with close GDP pc at starting and ending year
     #And creates graphs to shown 
     output$graph1 <- renderGvis({
     GDPpc<-sel.country.GDPpc(country=input$country,start.year=input$slidyear[1],end.year=input$slidyear[2])
     countries<-names(GDPpc)
     gvisLineChart(data=GDPpc,xvar="year",yvar=countries[2:length(GDPpc)]
                       ,options=list(height=250,width=750
                                     ,title="GDP per capita comparison"))
     })
     
     #Selects countries with close GDP pe at starting and ending year
     #And creates graphs to shown 
     output$graph2 <- renderGvis({
          GDPpe<-sel.country.GDPpe(country=input$country,start.year=input$slidyear[1],end.year=input$slidyear[2])
          countries<-names(GDPpe)
          gvisLineChart(data=GDPpe,xvar="year",yvar=countries[2:length(GDPpe)]
                        ,options=list(height=250,width=750
                                      ,title="GDP per pemployee comparison"))
     })
     
     #Calculates GDP pc annual growth and compares to CAGR
     #And creates graphs to shown 
     output$graph3 <- renderGvis({
          GDPpc.growth<-calc.CAGR(country=input$country,start.year=input$slidyear[1],end.year=input$slidyear[2])
          title1<-paste(input$country," GDP per capita annual growth vs CAGR",sep=":")
          gvisLineChart(data=GDPpc.growth,xvar="year",yvar=c("GDPpc.growth","CAGR.GDPpc")
                        ,options=list(height=250,width=750
                                      ,title=title1))
     })
     
     #Calculates GDP pemployee annual growth and compares to CAGR
     #And creates graphs to shown 
     output$graph4 <- renderGvis({
          GDPpe.growth<-calc.CAGR(country=input$country,start.year=input$slidyear[1],end.year=input$slidyear[2])
          title2<-paste(input$country," GDP per employee annual growth vs CAGR",sep=":")
          gvisLineChart(data=GDPpe.growth,xvar="year",yvar=c("GDPpe.growth","CAGR.GDPpe")
                        ,options=list(height=250,width=750
                                      ,title=title2))
     })
})