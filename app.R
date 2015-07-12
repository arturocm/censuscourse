require(choroplethr)
require(shiny)
require(dplyr)
require(choroplethrMaps)
require(choroplethrZip)
require(mapproj)
require(ggplot2)

#load required data in State + Value format
data(df_pop_state)
data(df_state_demographics)
data(df_county_demographics)
data(df_zip_demographics)

#fuction to format column names text
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

#obtain variable names for the dropdown menu and the plotting function
## State level
col.names <- colnames(df_state_demographics[, -1])
demographics.names <- gsub("_", " ",col.names) %>% capwords()
selection <- as.data.frame(cbind(demographics.names, col.names))
## County level
col.names2 <- colnames(df_county_demographics[, -1])
demographics.names2 <- gsub("_", " ",col.names2) %>% capwords()
selection2 <- as.data.frame(cbind(demographics.names2, col.names2))
## Zip level
col.names3 <- colnames(df_zip_demographics[, -1])
demographics.names3 <- gsub("_", " ",col.names3) %>% capwords()
selection3 <- as.data.frame(cbind(demographics.names3, col.names2))
url <- "national_county.txt"
zip.code <- read.csv(url, header = FALSE, colClasses = "character", 
                col.names = c("state","zip1","zip2","county","v5")) %>% 
  mutate(zip = paste(zip1,zip2,sep=""), state.county = paste(state, county, sep = "-")) %>%
  select(state.county, zip)

ui <- navbarPage("Mapping Census Data in R",
tabPanel("States (HW1)", fluidPage(fluidRow(
  column(12,
         h4("Homework 1 from ",
            a("@AriLamstein", href="https://twitter.com/AriLamstein", target="_blank"),
              " email course Mapping Census Data in R", 
            a("#censuscourse", href="https://twitter.com/hashtag/censuscourse", target="_blank")),
         p("This is a simple shiny app that lets you select between 
             different variables from the df_state_demographics data frame"),
         p("You can also play with the argument num_colors from the 
             state_choropleth formula to see how it alterate the displayed colors"),
         h5("Twitter:", a("@arturocm", href="https://twitter.com/arturocm", target="_blank")),
         h5("Github:", a("@arturocm", href="https://github.com/arturocm/censuscourse/blob/master/app.R", 
                          target="_blank"))),
  column(2, wellPanel(
    sliderInput("n1", "num_color value to use in state_choropleth formula 
                (n=1 will use a continous scale):", min = 1, max = 9, value = 1, step = 1),
    hr(),
    verbatimTextOutput('out1'),
    selectInput('hw1', 'Choose the demographic statistics you want to plot:', 
                demographics.names, selectize=TRUE)
  )),
  column(10, plotOutput("plot1", width = "100%", height = "800px"))
  ))),

tabPanel("Counties (HW2)", fluidPage(fluidRow(
  column(12,
         h4("Homework 2 from ", 
            a("@AriLamstein", href="https://twitter.com/AriLamstein", target="_blank"),
              " email course Mapping Census Data in R", 
            a("#censuscourse", href="https://twitter.com/hashtag/censuscourse", target="_blank")),
         p("Just like in HW1 we need to create a choropleth of some other demographic statistic in 
           the data.frame df_county_demographics. The advantage of using Shiny is that you can selection
           among all the variables from the data frame and play with the num_colors to see how they interact"),
         h5("Twitter:", a("@arturocm", href="https://twitter.com/arturocm", target="_blank")),
         h5("Github:", a("@arturocm", href="https://github.com/arturocm/censuscourse/blob/master/app.R", 
                                    target="_blank"))),
  column(2, wellPanel(
    sliderInput("n2", "num_color value to use in county_choropleth
                (n=1 will use a continous scale):", min = 1, max = 9, value = 1, step = 1),
    hr(),
    verbatimTextOutput('out2'),
    selectInput('hw2', 'Choose the demographic statistics you want to plot:', 
                demographics.names2, selectize=TRUE)
  )),
             column(10, plotOutput("plot2", width = "100%", height = "800px"))
  ))),
tabPanel("ZIP Codes (HW3)", fluidPage(fluidRow(
  column(12,
         h4("Homework 3 from ", 
            a("@AriLamstein", href="https://twitter.com/AriLamstein", target="_blank"),
            " email course Mapping Census Data in R", 
            a("#censuscourse", href="https://twitter.com/hashtag/censuscourse", target="_blank")),
         p("In HW3 we now create a choropleth - zoomed at the county level - for demographic statistic available 
           the data.frame df_zip_demographics. For this we'll use the formula zip_choropleth(). Also, Shiny gives 
            us the advantage we can cross-reference the FIPS code list from ", 
            a("https://www.census.gov/", href="https://www.census.gov/", target="_blank"), 
          "The advantage of using Shiny is that you can selection among all the variables from the data frame and 
            lay with the num_colors to see how they interact"),
         h5("Twitter:", a("@arturocm", href="https://twitter.com/arturocm", target="_blank")),
         h5("Github:", a("@arturocm", href="https://github.com/arturocm/censuscourse/blob/master/app.R", 
                         target="_blank"))),
  column(2, wellPanel(
    sliderInput("n3", "num_color value to use in zip_choropleth
                (n=1 will use a continous scale):", min = 1, max = 9, value = 1, step = 1),
    hr(),
    verbatimTextOutput('out3'),
    selectInput('hw3', 'Choose the demographic statistics you want to plot:', 
                demographics.names3, selectize=TRUE),
    hr(),
    verbatimTextOutput('out4'),
    selectInput('state.county', 'Choose the state-county you want to plot:', 
                zip.code[1], selectize=TRUE)
    )),
  column(10, plotOutput("plot3", width = "100%", height = "800px"))
  )))
)

server <- function(input, output) {
  
  
  output$plot1 <- renderPlot({
    xref <- selection[selection[,1] %in% input$hw1, 2]
    df_state_demographics$value <- df_state_demographics[,as.character(xref)]
    main <- paste0("2012 State Population Estimates: ", input$hw1)
    state_choropleth(df_state_demographics, title = main, legend = input$hw1, num_colors = input$n1)
  })
  
  output$plot2 <- renderPlot({
    xref2 <- selection2[selection2[,1] %in% input$hw2, 2]
    df_county_demographics$value <- df_county_demographics[,as.character(xref2)]
    main2 <- paste0("2012 County Population Estimates: ", input$hw2)
    county_choropleth(df_county_demographics, title = main2, legend = input$hw2, num_colors = input$n2)
  })
  
  output$plot3 <- renderPlot({
    
    zip <- zip.code[zip.code[,1] %in% input$state.county, 2] %>% as.numeric()
    xref3 <- selection3[selection3[,1] %in% input$hw3, 2]
    df_zip_demographics$value <- df_zip_demographics[,as.character(xref3)]
    main3 <- paste0("2012 ", input$state.county, " Population Estimates: ", input$hw3)
    zip_choropleth(df_zip_demographics, county_zoom=zip, title = main3, legend = input$hw3, num_colors = input$n3) + 
      coord_map()
  })
  
}

shinyApp(ui = ui, server = server)