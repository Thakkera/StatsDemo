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
            h2("\nExercise 1"),
            box(p("Principal Component Analysis (PCA) is a projection method allowing the user to get na global overview of a data set. Here, we are going to simply apply PCA to a couple of data sets, to get a feel for what is possible, and how to interpret the plots."), width = 12)),
    tabItem(tabName = "ex1Synth",
            fluidRow(
              column(width = 6,
                     box(p("\nHere is a toy data set to start with, containing 2,967 objects and 10 variables. Below you'll see the pairs plot. Do you have any idea what you are looking at?"), width="100%"),
                     box(plotOutput(outputId = "plotPairs"), width="100%")),
              column(width = 6,
                     box(column(width = 9,
                                p("\nPush the button to get the PCA representation. All info (well, most of it...) will be represented in one plot only!")),
                         column(width = 3,
                                actionButton("doPCA", "Do PCA!")),
                         width="100%"),                     
                     box(plotOutput(outputId = "PCA"), width = "100%"))
            ),
            fluidRow(align = "center",
                     textOutput(outputId = "PCAquestion")
            )),
    tabItem(tabName = "ex1Wine",
            fluidRow(
              column(width = 6,
                     box(p("\nNow consider the wine data: 177 different bottles from three varieties. We have measured 13 variables for each bottle. Below you'll see the pairs plot - colors represent wine varieties. Can you find panels where all three varieties are well separated?"), width="100%"),
                     box(plotOutput(outputId = "plotPairsWine"), width="100%")),
              column(width = 6,
                     box(column(width = 9,
                                p("\nPush the button to get the PCA representation. Again, this should preserve most of the info!")),
                         column(width = 3,
                                actionButton("doPCAWine", "Do PCA!")),
                         width="100%"),
                     box(plotOutput(outputId = "PCAWines"), width = "100%"))
            ),
            fluidRow(align = "center",
                     textOutput(outputId = "PCAWinequestion")
            )),
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

  PCA.PCA <- PCA(scale(PCADATA, scale = FALSE))

  myscoreplot <- function(PCAobj, pcs = c(1,2),
                          groups = rep(1, nrow(PCAobj$scores))) {
    PCAscores <- data.frame(PCa = PCAobj$scores[,pcs[1]],
                            PCb = PCAobj$scores[,pcs[2]],
                            groups = groups)
    PCAperc <- round(100*PCAobj$var[pcs] / PCA.PCA$totalvar, 1)
    xyplot(PCb ~ PCa, data = PCAscores, groups = groups,
           xlab = paste("PC ", pcs[1], " (", PCAperc[1], "%)", sep = ""),
           ylab = paste("PC ", pcs[2], " (", PCAperc[2], "%)", sep = ""),
           panel = function(...) {
             panel.abline(h = 0, v = 0, col = "gray", lty = 2)
             panel.xyplot(...)
           })    
  }
  
  wine.PCA <- PCA(scale(wines))

  output$plotPairs <- renderPlot({
    splom(PCADATA, groups = colors,
          pch = ".", cex = 2, pscales = 0)
  })
  output$plotPairsWine <- renderPlot({
    splom(wines, groups = vintages,
          varnames = paste("V", 1:ncol(wines), sep = ""),
          pch = ".", cex = 2, pscales = 0,
          auto.key = list(space = "right"))
  })

  observeEvent(input$doPCA, {
    output$PCA <- renderPlot({
      myscoreplot(PCA.PCA, groups = colors)
    })
    output$PCAquestion <- renderText({
      "Question: you may (or may not) see the letters upside down, mirrored from left to right, or a combination of these. Any idea why?"})
  })
    
  observeEvent(input$doPCAWine, {
    output$PCAWines <- renderPlot({
      myscoreplot(wine.PCA, groups = vintages)
    })
    output$PCAWinequestion <- renderText({
      "Question: which of the three varieties are most different?"})
  })
}

shinyApp(ui = ui, server = server)
