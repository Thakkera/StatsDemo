## Ik wil eigenlijk per PCA opgave een entry in de bar aan de
## linkerkant, en dan binnen iedere opgave ook nog deelopdrachten. Bv
## verschillende data sets. Een deelopdracht is bv op de knop met "doe
## PCA" drukken...

require(StatsDemo)
require(shiny)
require(shinydashboard)

sidebar <- dashboardSidebar(
  helpText("Understanding PCA"),
  sidebarMenu(
    menuItem("Exercise 1", tabName = "ex1"),
    menuSubItem("Synthetic data", tabName = "ex1Synth"),
    menuSubItem("Wine data", tabName = "ex1Wine"),
    menuItem("Exercise 2", tabName = "ex2")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "ex1",
            column(width = 12,
                   h2("\nExercise 1"),
                   box(p("Principal Component Analysis (PCA) is a projection method allowing the user to get na global overview of a data set. Here, we are going to simply apply PCA to a couple of data sets, to get a feel for what is possible, and how to interpret the plots.")))),
    tabItem(tabName = "ex1Synth",
            h2("\nExercise 1"),
            box(p("\nHere is a toy data set to start with, containing 2,967 objects and 10 variables. To the right you'll see the pairs plot.")),
            plotOutput(outputId = "plotPairs"),
            box(p("\nPush the button below to get the PCA representation. All info (well, most of it...) will be represented in one plot only!")),
            actionButton("doPCA", "Do PCA!"),
            plotOutput(outputId = "PCA")),
    tabItem(tabName = "ex1Wine",
            h2("\nExercise 1"),
            box(p("\nNow, consider the wine data.")),
            plotOutput(outputId = "plotPairs")),
    tabItem(tabName = "ex2",
            h2("\nExercise 2"))
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "PCA exercises"),
  sidebar,
  body
)

server <- function(input, output) {
  data(PCADATA)
  data(wines)

  output$plotPairs <- renderPlot({
    switch(input$tabs,
           ex1Synth = splom(PCADATA, groups = colors,
                            pch = ".", cex = 2, pscales = 0),
           ex1Wine = splom(wines, groups = vintages,
                                       pch = ".", cex = 2, pscales = 0,
                           auto.key = list(space = "right")))
  })

  observeEvent(input$doPCA, {
    output$PCA <- renderPlot({
      switch(input$tabs,
             ex1Synth = scoreplot(PCA(scale(PCADATA, scale = FALSE)),
                                  col = colors),
             ex1Wine = scoreplot(PCA(scale(wines)), col = vintages))
    })
  })
}

shinyApp(ui = ui, server = server)
