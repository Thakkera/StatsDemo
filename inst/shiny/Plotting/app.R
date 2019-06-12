require(StatsDemo)
require(shiny)
require(shinydashboard)

ui <- fluidPage(
  titlePanel("Plotting multivariate data"),
  sidebarLayout(
    sidebarPanel(
      ## h3("Visualization"),
      p("In this short exercise we will take a look at one data set of 177 Italian wines - for each wine, 13 variables have been measured. This is by all modern standards a tiny data set but you will see that already it presents difficulties in visualization. The effect of different forms of scaling can be seen nicely."),
      ## br(),
      p("Select a plot type and a form of scaling:"),
      radioButtons("plottype", "Plot type:",
                   c("Intro (no plot)" = "intro",
                     "Pairs" = "pairs",
                     "Boxplot" = "box",
                     "Dotplot" = "dot",
                     "Parallel coordinates" = "par")),
      radioButtons("scaling", "Scaling:",
                   c("None" = "no",
                     "Autoscaling" = "auto",
                     "Mean centering" = "mean",
                     "Sqrt scaling" = "sqrt",
                     "Log scaling" = "log"))
    ),
    mainPanel(
      p("Different types of plots serve different purposes. First have a look at the characteristics of the different plots. Think about situations where such plots are useful, or maybe less useful. Next, play around with the different scaling options."),
      plotOutput("plot"),
      h3("Questions"),
      p("1. Compared to no scaling, some scaling methods only affect the axes and not the plots themselves. Identify the scaling methods for which this is true. Can you understand why?"),
      p("2.  Are the effects of the scaling equally clear in all plotting types?")
    )
  )
)


server <- function(input, output) {
  ## require(lattice, quiet = TRUE)
  data(wines)
  wines.df <- data.frame(variety = vintages,
                         as.data.frame.table(wines,
                                             responseName = "Level")[,-1])
  names(wines.df)[2] <- "Var"

  wineData <- reactive(
    switch(input$scaling,
           "no" = wines,
           "auto" = scale(wines),
           "mean" = scale(wines, scale = FALSE),
           "sqrt" = sqrt(wines),
           "log" = log(wines)))

  output$plot <- renderPlot({
    wines.df0 <-
      data.frame(variety = vintages,
                 wines = as.data.frame.table(wineData(),
                                             responseName = "Level"))
    switch(input$plottype,
           "intro" = NULL,
           "pairs" = splom(wineData(), groups = vintages, pscales = 0,
                           varname.cex = .5, pch = ".", cex = 2,
                           auto.key = list(space = "right")),
           "box" = bwplot(variety ~ wines.Level | wines.Var2,
                          xlab = "Level",
                          data = wines.df0, scales = list(x = "free")),
           "dot" = dotplot(variety ~ wines.Level | wines.Var2,
                           xlab = "Level", groups = variety,
                           jitter.y = TRUE,
                           data = wines.df0, scales = list(x = "free")),
           "par" = parallelplot(~ wineData() | vintages, groups = vintages))
  })
}

shinyApp(ui = ui, server = server)
