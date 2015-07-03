require(choroplethr)
require(shiny)
require(dplyr)

#load required data in State + Value format
data(df_pop_state)
data(df_state_demographics)


#fuction to format column names text
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


#obtain variable names for the dropdown menu and the plotting function
col.names <- colnames(df_state_demographics[, -1])
demographics.names <- gsub("_", " ",col.names) %>% capwords()
selection <- as.data.frame(cbind(demographics.names, col.names))

ui <- fluidPage(
  titlePanel("Mapping Census Data in R: States"),
  fluidRow(
    column(12,
           h4("Homework 1 from ", 
              a("@AriLamstein", href="https://twitter.com/AriLamstein", target="_blank"),
              " email course Mapping Census Data in R", 
              a("#censuscourse", href="https://twitter.com/hashtag/censuscourse", target="_blank")),
          
           p("This is a simple shiny app that lets you select between 
             different variables from the df_state_demographics data frame"),
           p("In addition you can select the argument num_colors from the 
             state_choropleth formula to see how it alterate the displayed colors"),
           h4(
             "Twitter:", a("@arturocm", href="https://twitter.com/arturocm", target="_blank"))
    ),
    column(2, wellPanel(
      sliderInput("n", "Select number of colors:", 
                  min = 1, max = 9, value = 1, step = 1),
           hr(),
           verbatimTextOutput('out5'),
           selectInput('in5', 'Choose the demographic you want to plot:', demographics.names, selectize=TRUE)
    )),
    column(10, plotOutput("plot1", width = "100%", height = "800px", click = "plot1_click")
    )
    )
  )

server <- function(input, output) {
  output$plot1 <- renderPlot({
    xref <- selection[selection[,1] %in% input$in5, 2]
    df_state_demographics$value <- df_state_demographics[,as.character(xref)]
    main <- paste0("2012 State Population Estimates: ", input$in5)
    state_choropleth(df_state_demographics, title = main, legend = input$in5, num_colors = input$n)
  })
}

shinyApp(ui = ui, server = server)
