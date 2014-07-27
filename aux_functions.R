sel.country.GDPpc<-function(country=country,start.year=start.year,end.year=end.year) {
     #loads last version of Penn World Tables
     library("pwt8")
     data("pwt8.0") 
     
     #Selects from Penn tables the columns related to GDP, population and employees
     first.set<-pwt8.0[pwt8.0$year %in% c(start.year,end.year),c(1,3,5,6,7,8)]
     
     #Calculates data per capita
     bench<-data.frame("country"=first.set$country
                       ,"year"=first.set$year
                       ,"GDPpc"=first.set[,3]/first.set[,5])
     
     #Selects from data above the first year chosen to compare
     bench.start<-bench[bench$year %in% c(start.year),]
     
     #Calculates the distance to the data per capita to the chosen market and picks the 
     #3 closest countries in the beginning year
     dist.start<- abs(bench.start[,3]-bench.start[bench.start$country %in% c(country),3])
     bench.start<-cbind(bench.start,dist.start)
     bench.start<-bench.start[order(dist.start),]
     
     #Selects from data above the first year chosen to compare
     bench.end<-bench[bench$year %in% c(end.year),]
     
     #Calculates the distance to the data per capita to the chosen market and picks the 
     #3 closest countries in the beginning year
     dist.end<- abs(bench.end[,3]-bench.end[bench.end$country %in% c(country),3])
     bench.end<-cbind(bench.end,dist.end)
     bench.end<-bench.end[order(dist.end),]
     
     #Puts together the countris close to selected country in the starting and ending year
     # Eliminates duplicates
     sel.start<-as.character(bench.start[c(1:3),1])
     sel.end<-as.character(bench.end[c(1:3),1])
     selection<-unique(c(sel.start,sel.end))
     selection<-as.factor(selection)
     
     #Subsets the data for countries picked to compare chosen 
     data.sel<-pwt8.0[pwt8.0$country %in% selection & pwt8.0$year %in% c(start.year:end.year)
                      ,c(1,3,5,6,7,8)]
     data.sel<-data.frame("country"=data.sel$country,"year"=data.sel$year,"GDPpc"=ceiling(data.sel[,3]/data.sel[,5]))
     
     #Creates data.frame ready for creating ggogleVis graphs
     #(i.e.: data.frame with years and countries in columns)
     solution<-data.frame("year"=unique(data.sel$year))
     for (i in 1:length(selection)){
          solution<-data.frame(solution,data.sel[data.sel$country %in% selection[i],3])
     }
     names(solution)<-c("year",as.character(selection))
     
     #Returns completed data.frame
     solution
}

sel.country.GDPpe<-function(country=country,start.year=start.year,end.year=end.year) {
     #loads last version of Penn World Tables
     library("pwt8")
     data("pwt8.0") 
     
     #Selects from Penn tables the columns related to GDP, population and employees
     first.set<-pwt8.0[pwt8.0$year %in% c(start.year,end.year),c(1,3,5,6,7,8)]
     
     #Calculates data per employee
     bench<-data.frame("country"=first.set$country
                       ,"year"=first.set$year
                       ,"GDPpe"=first.set[,4]/first.set[,6])
     
     #Selects from data above the first year chosen to compare
     bench.start<-bench[bench$year %in% c(start.year),]
     
     #Calculates the distance to the data per capita to the chosen market and picks the 
     #3 closest countries in the beginning year
     dist.start<- abs(bench.start[,3]-bench.start[bench.start$country %in% c(country),3])
     bench.start<-cbind(bench.start,dist.start)
     bench.start<-bench.start[order(dist.start),]
     
     #Selects from data above the last year chosen to compare
     bench.end<-bench[bench$year %in% c(end.year),]
     
     #Calculates the distance to the data per capita to the chosen market and picks the 
     #3 closest countries in the beginning year
     dist.end<- abs(bench.end[,3]-bench.end[bench.end$country %in% c(country),3])
     bench.end<-cbind(bench.end,dist.end)
     bench.end<-bench.end[order(dist.end),]
     
     #Puts together the countris close to selected country in the starting and ending year
     # Eliminates duplicates
     sel.start<-as.character(bench.start[c(1:3),1])
     sel.end<-as.character(bench.end[c(1:3),1])
     selection<-unique(c(sel.start,sel.end))
     selection<-as.factor(selection)
     
     #Subsets the data for countries picked to compare chosen 
     data.sel<-pwt8.0[pwt8.0$country %in% selection & pwt8.0$year %in% c(start.year:end.year)
                      ,c(1,3,5,6,7,8)]
     data.sel<-data.frame("country"=data.sel$country,"year"=data.sel$year,"GDPpc"=ceiling(data.sel[,4]/data.sel[,6]))
     
     #Creates data.frame ready for creating ggogleVis graphs
     #(i.e.: data.frame with years and countries in columns)
     solution<-data.frame("year"=unique(data.sel$year))
     
     for (i in 1:length(selection)){
          solution<-data.frame(solution,data.sel[data.sel$country %in% selection[i],3])
     }
     names(solution)<-c("year",as.character(selection))
     
     #Returns completed data.frame
     solution
}


calc.CAGR<-function(country=country,start.year=start.year,end.year=end.year){
     #loads last version of Penn World Tables
     library("pwt8")
     data("pwt8.0") 
     
     #Selects from Penn tables the columns related to GDP, population and employees
     first.set<-pwt8.0[pwt8.0$year %in% c(start.year:end.year) & pwt8.0$country %in% c(country)
                       ,c(1,3,5,6,7,8)]
     
     #Calculates data per capita and per employee
     sel.data<-data.frame("country"=first.set$country
                          ,"year"=first.set$year
                          ,"GDPpc"=first.set[,3]/first.set[,5]
                          ,"GDPpe"=first.set[,4]/first.set[,6])
     
     #Calculates CAGR
     ext.data<-sel.data[sel.data$year %in% c(start.year,end.year),]
     cagr.pc<-(((ext.data[2,3]/ext.data[1,3])^(1/(ext.data[2,2]-ext.data[1,2])))-1)*100
     cagr.pe<-(((ext.data[2,4]/ext.data[1,4])^(1/(ext.data[2,2]-ext.data[1,2])))-1)*100
     
     #Calculates annual growth vs Pr Year and adds columns with CAGRs
     growth.data<-data.frame()
     
     for (i in 2:length(sel.data[,1])){
          yr<-sel.data[i,2]
          growth.pc<-(sel.data[i,3]/sel.data[(i-1),3]-1)*100
          growth.pe<-(sel.data[i,4]/sel.data[(i-1),4]-1)*100
          
          growth.data<-rbind(growth.data
                             ,data.frame("year"=yr
                                         ,"GDPpc.growth"=round(growth.pc,2)
                                         ,"CAGR.GDPpc"=round(cagr.pc,2)
                                         ,"GDPpe.growth"=round(growth.pe,2)
                                         ,"CAGR.GDPpe"=round(cagr.pe,2)))
     }
     
     growth.data
     
}