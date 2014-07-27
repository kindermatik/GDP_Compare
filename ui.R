
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("GDP Compare"),

  # Sidebar with:
     #Text with explanation of app
     #Text input to select country
     #Slider to select starting year and ending year
  sidebarLayout(
    sidebarPanel(
         textInput("country"
                   , label = h5("Input the name of the country")
                   , value = "Spain"),

      
         sliderInput("slidyear", label = h5("Year Range"), min = 1960, 
                     max = 2011, value = c(1960, 2011)),
         
         h4("How to use this app:"),
         p("This app takes the data from Penn World Tables",a("home website"
          ,href="http://www.rug.nl/research/ggdc/data/penn-world-table")),
          p("It allows you to select the country and the year range. 
            It then search for the 3 markets with closest GDP per capita and GDP per employee 
            both at the starting and the ending year and shows the evolution of these variables 
            throughout the selected period. All figures are meassured in chained Purchase
            Power Parity in 2005 US dollars."),
         p("It also shows the Compound Annual Growth Rate for both variables and compares it with 
           the actual year growth. This way you can see if there are virtual ot vicious cycles in 
           the selected year range."),
         p("In case you select a starting year for which the selected market has no available
           data, the app will match your country with other countries with no available data at that time
          (and CAGR will not be available). Simply change the year range"),
         p("Most of country names are fairly normal (start names with capital letters), 
          if you have any issue finding the correct term 
           you can search for the names in Penn World Tables site (above)"),
         p("Move your mouse pointer on the graphs and you will see the exact values for each year"),
         p("If you want to see some interesting cases, write Venezuela or Singapore in the country box"),

          h3("Enjoy!"),
         br(),
         p("code is available", a("in this github link",href="https://github.com/kindermatik/GDP_Compare"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
         htmlOutput("graph1"),
         htmlOutput("graph2"),
         htmlOutput("graph3"),
         htmlOutput("graph4")
    )
  )
))
