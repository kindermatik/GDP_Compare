GDP Compare
========================================================
transition: rotate

  The [Shiny app](http://kindermatik.shinyapps.io/GDP_Compare/) 
 

What is this app about?
========================================================
You read in the newspaper about the GDP growth in your country and you wonder: how do we compare to other counties?  
The app does basically that using the [Penn World Table v.8.0](http://www.rug.nl/research/ggdc/data/penn-world-table) data:  
* <small>Takes the GDP per capita and per employee of the country you select.</small>
* <small>Searches for countries with similar GDPpcap. and GDPpemp. both in the starting and ending year.</small>
* <small>Two graphs show your country against the ones picked above so you can compare their evolution.</small>
* <small>Moreover, it shows the CAGR and growth year on year, so you can look for virtuous and vicious cycles.</small>  
Next slides show some examples.  
**Enjoy!!!**


```r
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
```

Singaporeans taste for climbing
========================================================
* <small>In 1965, the Malaysian parliament voted 126 to 0 to expel [Singapore from Malaysia](http://en.wikipedia.org/wiki/Singapore)</small>  
* <small>Nowadays they are above Switzerland!!!</small>  
* <small>But they still keep the nice weather</small>  


```r
library(googleVis) #package to create graphs
GDPpc<-sel.country.GDPpc(country="Singapore",start.year=1960,end.year=2011)
countries<-names(GDPpc)
graph1<-gvisLineChart(data=GDPpc,xvar="year",yvar=countries[2:length(GDPpc)]
                       ,options=list(height=300,width=800
                                     ,title="GDP per capita comparison"))
plot(graph1,"chart")
```

<!-- LineChart generated in R 3.1.1 by googleVis 0.5.4 package -->
<!-- Sun Jul 27 17:02:11 2014 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataLineChartID2e021c09cff0 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "1960",
2414,
2417,
2384,
10126,
17056 
],
[
 "1961",
2496,
2486,
2451,
10692,
17972 
],
[
 "1962",
2650,
2631,
2303,
10906,
18405 
],
[
 "1963",
2921,
2712,
2258,
11178,
18915 
],
[
 "1964",
2763,
2887,
2381,
11850,
19743 
],
[
 "1965",
2957,
2933,
2685,
12455,
20190 
],
[
 "1966",
3260,
3094,
2626,
12839,
20587 
],
[
 "1967",
3648,
3154,
2705,
13670,
21136 
],
[
 "1968",
4123,
3134,
2747,
14113,
21774 
],
[
 "1969",
4700,
3221,
2943,
14432,
22609 
],
[
 "1970",
5263,
3358,
2951,
14900,
23659 
],
[
 "1971",
5872,
3355,
3062,
15608,
24647 
],
[
 "1972",
6540,
3327,
3075,
16043,
25461 
],
[
 "1973",
7093,
3509,
3185,
16883,
25962 
],
[
 "1974",
7566,
3640,
3254,
17263,
25861 
],
[
 "1975",
7637,
3540,
3133,
17518,
23931 
],
[
 "1976",
8070,
3536,
3072,
18122,
23934 
],
[
 "1977",
8444,
3493,
3152,
18597,
24310 
],
[
 "1978",
9225,
3370,
3010,
19105,
25222 
],
[
 "1979",
10070,
3666,
3241,
20172,
25800 
],
[
 "1980",
11148,
3677,
3372,
21733,
26583 
],
[
 "1981",
11947,
3679,
3304,
21953,
26608 
],
[
 "1982",
12881,
3727,
3299,
21580,
26527 
],
[
 "1983",
13997,
3305,
3559,
21784,
26703 
],
[
 "1984",
14716,
3426,
3734,
22929,
27547 
],
[
 "1985",
13876,
3477,
3526,
23529,
27964 
],
[
 "1986",
13600,
3689,
3977,
22930,
29288 
],
[
 "1987",
14451,
4057,
4203,
22876,
29856 
],
[
 "1988",
15398,
3808,
4377,
22485,
30454 
],
[
 "1989",
16640,
3139,
4230,
22910,
31422 
],
[
 "1990",
18326,
3124,
4605,
23364,
32569 
],
[
 "1991",
19324,
3188,
5413,
23849,
32052 
],
[
 "1992",
19987,
3202,
6309,
24032,
31423 
],
[
 "1993",
21728,
3350,
7167,
24775,
31460 
],
[
 "1994",
23202,
3870,
8307,
25697,
32066 
],
[
 "1995",
24965,
4291,
9079,
26913,
32157 
],
[
 "1996",
26251,
4460,
11682,
29232,
32462 
],
[
 "1997",
28895,
4666,
11776,
31743,
34261 
],
[
 "1998",
27659,
4493,
11561,
31790,
35640 
],
[
 "1999",
29050,
4417,
10595,
33838,
35940 
],
[
 "2000",
34044,
4485,
10177,
38912,
36983 
],
[
 "2001",
32839,
4437,
9365,
38847,
36828 
],
[
 "2002",
34461,
4658,
8664,
37912,
36816 
],
[
 "2003",
35235,
4887,
9015,
38132,
35737 
],
[
 "2004",
41651,
5267,
9437,
41479,
36480 
],
[
 "2005",
45750,
5747,
9957,
44546,
36815 
],
[
 "2006",
48592,
6481,
10773,
48223,
39118 
],
[
 "2007",
51875,
7049,
11601,
50037,
42442 
],
[
 "2008",
48780,
7475,
12428,
53100,
44327 
],
[
 "2009",
45345,
7396,
12369,
47812,
42496 
],
[
 "2010",
50669,
8278,
13360,
50179,
44403 
],
[
 "2011",
51644,
8924,
14508,
52415,
44824 
] 
];
data.addColumn('string','year');
data.addColumn('number','Singapore');
data.addColumn('number','Peru');
data.addColumn('number','Argentina');
data.addColumn('number','Norway');
data.addColumn('number','Switzerland');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartLineChartID2e021c09cff0() {
var data = gvisDataLineChartID2e021c09cff0();
var options = {};
options["allowHtml"] = true;
options["height"] =    300;
options["width"] =    800;
options["title"] = "GDP per capita comparison";

    var chart = new google.visualization.LineChart(
    document.getElementById('LineChartID2e021c09cff0')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartLineChartID2e021c09cff0);
})();
function displayChartLineChartID2e021c09cff0() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartLineChartID2e021c09cff0"></script>
 
<!-- divChart -->
  
<div id="LineChartID2e021c09cff0" 
  style="width: 800; height: 300;">
</div>

<small>_Expenditure-side real GDP at chained PPPs in 2005 USD_</small>

Luxembourg, Single Market and labour productivity
========================================================
* <small>In Feb 1986 the [Single European Act](http://en.wikipedia.org/wiki/Single_European_Act) was signed.</small>  
* <small>The aim was to move towards a single European market by 1992.</small>  
* <small>Today Luxembourg is the world's second largest investment fund centre.</small> 


```r
library(googleVis) #package to create graphs
GDPpe<-sel.country.GDPpe(country="Luxembourg",start.year=1960,end.year=2011)
countries<-names(GDPpe)
graph2<-gvisLineChart(data=GDPpe,xvar="year",yvar=countries[2:length(GDPpe)]
                       ,options=list(height=300,width=800
                                     ,title="GDP per employee comparison"))
plot(graph2,"chart")
```

<!-- LineChart generated in R 3.1.1 by googleVis 0.5.4 package -->
<!-- Sun Jul 27 17:02:11 2014 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataLineChartID2e024353210e () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "1960",
40764,
40032,
41512,
33841,
20971 
],
[
 "1961",
40308,
40910,
42419,
35017,
21983 
],
[
 "1962",
38873,
42700,
43881,
35353,
23486 
],
[
 "1963",
39363,
44000,
45109,
36411,
24767 
],
[
 "1964",
42487,
45680,
46638,
37965,
26131 
],
[
 "1965",
42812,
47490,
47709,
39120,
27249 
],
[
 "1966",
43124,
49042,
48212,
40186,
28482 
],
[
 "1967",
45383,
49050,
48212,
41254,
29809 
],
[
 "1968",
48507,
50431,
50246,
42576,
31184 
],
[
 "1969",
55186,
50892,
51533,
44347,
32907 
],
[
 "1970",
52891,
50846,
52564,
46612,
34460 
],
[
 "1971",
49876,
52695,
53948,
47966,
36416 
],
[
 "1972",
52605,
54285,
55407,
49137,
38177 
],
[
 "1973",
57341,
55869,
55773,
50244,
40395 
],
[
 "1974",
53337,
54371,
50805,
50335,
42519 
],
[
 "1975",
45792,
54791,
51219,
47698,
42527 
],
[
 "1976",
48207,
56154,
51834,
47855,
44105 
],
[
 "1977",
48780,
56844,
52697,
48732,
45526 
],
[
 "1978",
52728,
57825,
53572,
49387,
47271 
],
[
 "1979",
51543,
57995,
51812,
49910,
48572 
],
[
 "1980",
47983,
57290,
49534,
50502,
49132 
],
[
 "1981",
45865,
58157,
49714,
49874,
48165 
],
[
 "1982",
47006,
57273,
50928,
49093,
48069 
],
[
 "1983",
48101,
59087,
52760,
49607,
47345 
],
[
 "1984",
47738,
61068,
54757,
50677,
47056 
],
[
 "1985",
46087,
61932,
56268,
51178,
46807 
],
[
 "1986",
54066,
62327,
56448,
53209,
48887 
],
[
 "1987",
53292,
62644,
58898,
53363,
49811 
],
[
 "1988",
58412,
63682,
59915,
53228,
51681 
],
[
 "1989",
63576,
64602,
60737,
53198,
52854 
],
[
 "1990",
63059,
64905,
60349,
54294,
54103 
],
[
 "1991",
67990,
65376,
60097,
52565,
54148 
],
[
 "1992",
69036,
67595,
61408,
53792,
55661 
],
[
 "1993",
72442,
68949,
62342,
54982,
56018 
],
[
 "1994",
74215,
70487,
63991,
56838,
57114 
],
[
 "1995",
72414,
71305,
65391,
57261,
57987 
],
[
 "1996",
72594,
73055,
66015,
57651,
58176 
],
[
 "1997",
75157,
74741,
66796,
59999,
60614 
],
[
 "1998",
80254,
76850,
66221,
61051,
63161 
],
[
 "1999",
88290,
79242,
68775,
61724,
64363 
],
[
 "2000",
95207,
81492,
70705,
62778,
67337 
],
[
 "2001",
88705,
82039,
69778,
61861,
68437 
],
[
 "2002",
87463,
83243,
67564,
63369,
69023 
],
[
 "2003",
86440,
84437,
68547,
63486,
66082 
],
[
 "2004",
80765,
86401,
68994,
65878,
67106 
],
[
 "2005",
88922,
87484,
71110,
68034,
69580 
],
[
 "2006",
85857,
88191,
68740,
69258,
69453 
],
[
 "2007",
85639,
88965,
67804,
72838,
70901 
],
[
 "2008",
75566,
88853,
63920,
75361,
70570 
],
[
 "2009",
76494,
88971,
64709,
71548,
69905 
],
[
 "2010",
78092,
91729,
64592,
74966,
72208 
],
[
 "2011",
73664,
93039,
63208,
74057,
72736 
] 
];
data.addColumn('string','year');
data.addColumn('number','Luxembourg');
data.addColumn('number','United States of America');
data.addColumn('number','Canada');
data.addColumn('number','Switzerland');
data.addColumn('number','France');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartLineChartID2e024353210e() {
var data = gvisDataLineChartID2e024353210e();
var options = {};
options["allowHtml"] = true;
options["height"] =    300;
options["width"] =    800;
options["title"] = "GDP per employee comparison";

    var chart = new google.visualization.LineChart(
    document.getElementById('LineChartID2e024353210e')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartLineChartID2e024353210e);
})();
function displayChartLineChartID2e024353210e() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartLineChartID2e024353210e"></script>
 
<!-- divChart -->
  
<div id="LineChartID2e024353210e" 
  style="width: 800; height: 300;">
</div>

<small>_Output-side real GDP at chained PPPs in 2005 USD_</small>

Spain, the dark side of unemployment
========================================================
* <small>Spain GDP peaked in 2008, when it was severely hit by recession</small>  
* <small>Despite that, GDP per employee (labour productivity) has kept momentum</small>  
* <small>The missing ingredient is unemployment. After reaching a minimum of 8% between 2006 and 2007 the rate of unemployment grew quickly exceeding 20% in 2010 and 25% in 2012</small>


```r
GDPpc.growth<-calc.CAGR(country="Spain",start.year=1989,end.year=2011)
title1<-paste("Spain"," GDP per capita annual growth vs CAGR",sep=":")
graph3 <-gvisLineChart(data=GDPpc.growth,xvar="year",yvar=c("GDPpc.growth","CAGR.GDPpc"),options=list(height=250,width=500,title=title1))

GDPpe.growth<-calc.CAGR(country="Spain",start.year=1989,end.year=2011)
title2<-paste("Spain"," GDP per employee annual growth vs CAGR",sep=":")
graph4<-gvisLineChart(data=GDPpe.growth,xvar="year",yvar=c("GDPpe.growth","CAGR.GDPpe"),options=list(height=250,width=500,title=title2))

graph5<-gvisMerge(graph3,graph4,horizontal=TRUE)

plot(graph5,"chart")
```

<!-- LineChart generated in R 3.1.1 by googleVis 0.5.4 package -->
<!-- Sun Jul 27 17:02:12 2014 -->


<!-- jsHeader -->
<script type="text/javascript">
 
// jsData 
function gvisDataLineChartID2e02539b10a5 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "1990",
6.52,
3.1 
],
[
 "1991",
4.99,
3.1 
],
[
 "1992",
2.91,
3.1 
],
[
 "1993",
-0.13,
3.1 
],
[
 "1994",
4.21,
3.1 
],
[
 "1995",
5.48,
3.1 
],
[
 "1996",
3.13,
3.1 
],
[
 "1997",
6.9,
3.1 
],
[
 "1998",
7.85,
3.1 
],
[
 "1999",
4.89,
3.1 
],
[
 "2000",
4.43,
3.1 
],
[
 "2001",
3.15,
3.1 
],
[
 "2002",
3.4,
3.1 
],
[
 "2003",
-0.56,
3.1 
],
[
 "2004",
3.09,
3.1 
],
[
 "2005",
2.15,
3.1 
],
[
 "2006",
7.12,
3.1 
],
[
 "2007",
6.47,
3.1 
],
[
 "2008",
0.27,
3.1 
],
[
 "2009",
-3.94,
3.1 
],
[
 "2010",
-1.48,
3.1 
],
[
 "2011",
-1.48,
3.1 
] 
];
data.addColumn('string','year');
data.addColumn('number','GDPpc.growth');
data.addColumn('number','CAGR.GDPpc');
data.addRows(datajson);
return(data);
}


// jsData 
function gvisDataLineChartID2e0227dbd819 () {
var data = new google.visualization.DataTable();
var datajson =
[
 [
 "1990",
1.88,
2.24 
],
[
 "1991",
3.07,
2.24 
],
[
 "1992",
4.08,
2.24 
],
[
 "1993",
3.94,
2.24 
],
[
 "1994",
4.85,
2.24 
],
[
 "1995",
3.14,
2.24 
],
[
 "1996",
1.06,
2.24 
],
[
 "1997",
3.05,
2.24 
],
[
 "1998",
2.85,
2.24 
],
[
 "1999",
0.94,
2.24 
],
[
 "2000",
3.03,
2.24 
],
[
 "2001",
0.55,
2.24 
],
[
 "2002",
1.69,
2.24 
],
[
 "2003",
-2.9,
2.24 
],
[
 "2004",
0.2,
2.24 
],
[
 "2005",
2.8,
2.24 
],
[
 "2006",
3.26,
2.24 
],
[
 "2007",
2.49,
2.24 
],
[
 "2008",
2.48,
2.24 
],
[
 "2009",
3.84,
2.24 
],
[
 "2010",
1.89,
2.24 
],
[
 "2011",
1.34,
2.24 
] 
];
data.addColumn('string','year');
data.addColumn('number','GDPpe.growth');
data.addColumn('number','CAGR.GDPpe');
data.addRows(datajson);
return(data);
}
 
// jsDrawChart
function drawChartLineChartID2e02539b10a5() {
var data = gvisDataLineChartID2e02539b10a5();
var options = {};
options["allowHtml"] = true;
options["height"] =    250;
options["width"] =    500;
options["title"] = "Spain: GDP per capita annual growth vs CAGR";

    var chart = new google.visualization.LineChart(
    document.getElementById('LineChartID2e02539b10a5')
    );
    chart.draw(data,options);
    

}
  


// jsDrawChart
function drawChartLineChartID2e0227dbd819() {
var data = gvisDataLineChartID2e0227dbd819();
var options = {};
options["allowHtml"] = true;
options["height"] =    250;
options["width"] =    500;
options["title"] = "Spain: GDP per employee annual growth vs CAGR";

    var chart = new google.visualization.LineChart(
    document.getElementById('LineChartID2e0227dbd819')
    );
    chart.draw(data,options);
    

}
  
 
// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartLineChartID2e02539b10a5);
})();
function displayChartLineChartID2e02539b10a5() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}


// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = "corechart";
  
// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
  pkgs.push(chartid);
  
// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartLineChartID2e0227dbd819);
})();
function displayChartLineChartID2e0227dbd819() {
  var pkgs = window.__gvisPackages = window.__gvisPackages || [];
  var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
  window.clearTimeout(window.__gvisLoad);
  // The timeout is set to 100 because otherwise the container div we are
  // targeting might not be part of the document yet
  window.__gvisLoad = setTimeout(function() {
  var pkgCount = pkgs.length;
  google.load("visualization", "1", { packages:pkgs, callback: function() {
  if (pkgCount != pkgs.length) {
  // Race condition where another setTimeout call snuck in after us; if
  // that call added a package, we must not shift its callback
  return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}
 
// jsFooter
</script>
 
<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartLineChartID2e02539b10a5"></script>


<!-- jsChart -->  
<script type="text/javascript" src="https://www.google.com/jsapi?callback=displayChartLineChartID2e0227dbd819"></script>
 
<table border="0">
<tr>
<td>

<!-- divChart -->
  
<div id="LineChartID2e02539b10a5" 
  style="width: 500; height: 250;">
</div>

</td>
<td>

<!-- divChart -->
  
<div id="LineChartID2e0227dbd819" 
  style="width: 500; height: 250;">
</div>

</td>
</tr>
</table>
