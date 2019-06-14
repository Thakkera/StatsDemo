require(shiny)
require(shinydashboard)
require(StatsDemo)

sidebar <- dashboardSidebar(
  helpText("Understanding PCA: components and examples"),
  sidebarMenu(
    menuItem("1: Score plots", 
             menuSubItem("Introduction", tabName = "ex1"),
             menuSubItem("Synthetic data", tabName = "ex1Synth"),
             menuSubItem("Wine data", tabName = "ex1Wine")),
    menuItem("2: Loading plots",
             menuSubItem("Introduction", tabName = "ex2"),
             menuSubItem("Synthetic data", tabName = "ex2Synth"),
             menuSubItem("Wine data", tabName = "ex2Wine")),
    menuItem("3: Biplots", 
             menuSubItem("Introduction", tabName = "ex3"),
             menuSubItem("Synthetic data", tabName = "ex3Synth"),
             menuSubItem("Wine data", tabName = "ex3Wine")),
    menuItem("4: The effect of scaling",
             menuSubItem("Introduction", tabName = "ex4"),
             menuSubItem("Lambrusco!", tabName = "ex4Lamb"))
  )
)

tabEx1 <-
  tabItem(tabName = "ex1",
          h2("\nExercise 1: Score Plots"),
          box(p("Principal Component Analysis (PCA) is a projection method allowing the user to get na global overview of a data set. Here, we are going to simply apply PCA to a couple of data sets, to get a feel for what is possible, and how to interpret the plots."),
              p("Score plots show the positions of the samples (rows in the original data matrix) in the PC space. So this type of plot tells you something about which samples are similar (in this subspace!) and which are not."),
              p("Have a look at the data sets in the sidebar, and discuss the results. Some questions will appear below the plots"), width = 8))

tabEx1S <-
  tabItem(tabName = "ex1Synth",
          fluidRow(
            column(width = 6,
                   p("\nHere is a toy data set to start with, containing 2,967 objects and 10 variables. Below you'll see the pairs plot. Do you have any idea what you are looking at?")),
            column(width = 4,
                   p("\nPush the button to get the PCA representation. All info (well, most of it...) will be represented in one plot only!")),
            column(width = 2,
                   actionButton("doPCA", "Do PCA!"))),
          fluidRow(
            box(plotOutput(outputId = "plotPairs"), width=6),
            box(plotOutput(outputId = "PCA"), width = 6)),
          fluidRow(align = "center",
                   textOutput(outputId = "PCAquestion"))
          )

tabEx1W <- 
  tabItem(tabName = "ex1Wine",
          fluidRow(
            column(width = 6,
                   p("\nNow consider the wine data: 177 different bottles from three varieties. We have measured 13 variables for each bottle. Below you'll see the pairs plot - colors represent wine varieties. Can you find panels where all three varieties are well separated?")),
            column(width = 3,
                   p("\nPush the button to get the PCA representation. Again, this should preserve most of the info!")),
            column(width = 3,
                   actionButton("doPCAWine", "Do PCA!"))
          ),
          fluidRow(
            box(plotOutput(outputId = "plotPairsWine"), width=6),
            box(plotOutput(outputId = "PCAWines"), width = 6)),
          fluidRow(align = "center",
                   textOutput(outputId = "PCAWinequestion")
                   ))

tabEx2 <-
  tabItem(tabName = "ex2",
          h2("\nExercise 2: Loading Plots"),
          box(p("Where score plots show the positions of the samples (rows in the original data matrix) in the PC space, loading plots concentrate on the variables. Each loading vector presents the weights of the original variables for that particular component. One can look at individual loading vectors, but also at loadings for two components at a time. In that case, it is custom to show the loadings as arrows from the origin (for the largest loadings, at least)."),
              p("Have a look at the data sets in the sidebar, and discuss the results. Some questions will appear below the plots"), width = 8))

tabEx2S <-
  tabItem(tabName = "ex2Synth",
          fluidRow(
            column(width = 4,
                   p("\nHere you will look at the loadings for the toy data set containing 2,967 objects and 10 variables (the letters). Remember that you were asked about how this data set may be reconstructed? The loading plots could provide a hint...")),
            column(width = 4,
                   p("\nIn the dropdown boxes you can select one or two components for which loadings are shown.")),
            column(width = 4,
                   column(width = 6,
                          selectInput("PC1", label = "First PC",
                                      choices = paste(1:10))),
                   column(width = 6,
                          selectInput("PC2", label = "Second PC",
                                      choices = c("none", paste(1:10)))),
                   actionButton("showLoadings", "Show loadings!"))),
          fluidRow(box(plotOutput(outputId = "plotLoadings"),
                       align = "center", width=12)),
          fluidRow(align = "center",
                   textOutput(outputId = "PCALoadingquestion"))
          )

tabEx3 <-
  tabItem(tabName = "ex3",
          h2("\nExercise 3: Biplots"),
          box(p("Biplots show a scoreplot and a loading plot in the same panel, superimposed. This allows you to easily relate groupings or effects in the scores to particular variable. Once you got the idea it can be pretty powerful, but note that grouping structure is not always visible in a PCA, especially when it depends on a few variables only."),
              p("Have a look at the data sets in the sidebar, and discuss the results. Some questions will appear below the plots"), width = 8))

tabEx4 <- 
  tabItem(tabName = "ex4",
          h2("\nExercise 4: The Effects of Scaling"),
          box(p("So far, PCA seems easy enough - for visual inspection of a high-dimensional data set we usually concentrate on the first PCs only. However, scaling matters, and finding the most suitable scaling is usually hard to do without information on what the variables actually mean."),
              p("Here we present yet another wine data set from mass-spectrometry-based metabolomics. The variables are the intensities of the individual mass peaks. There are quite large differences between intensities, and this is going to influence the results"),
              p("Check out a couple of scalings, all commonly used in metabolomics, and discuss the effects on the scores and on the loadings. Note all scaling methods will be applied after mean-centering the data, which is mandatory before doing PCA."), width = 8))

body <- dashboardBody(
  tabItems(tabEx1, tabEx1S, tabEx1W,
           tabEx2, tabEx2S,
           tabEx3,
           tabEx4)
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
  wine.PCA <- PCA(scale(wines))

  myscoreplot <- function(PCAobj, pcs = c(1,2),
                          groups = rep(1, nrow(PCAobj$scores))) {
    PCAscores <- data.frame(PCa = PCAobj$scores[,pcs[1]],
                            PCb = PCAobj$scores[,pcs[2]],
                            groups = groups)
    PCAperc <- round(100*PCAobj$var[pcs] / PCAobj$totalvar, 1)
    xyplot(PCb ~ PCa, data = PCAscores, groups = groups,
           xlab = paste("PC ", pcs[1], " (", PCAperc[1], "%)", sep = ""),
           ylab = paste("PC ", pcs[2], " (", PCAperc[2], "%)", sep = ""),
           panel = function(...) {
             panel.abline(h = 0, v = 0, col = "gray", lty = 2)
             panel.xyplot(...)
           })    
  }
  
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
      "Discuss the result. How do you think the data were created? Additional question: you may (or may not) see the letters upside down, mirrored from left to right, or a combination of these. Any idea why?"})
  })
    
  observeEvent(input$doPCAWine, {
    output$PCAWines <- renderPlot({
      myscoreplot(wine.PCA, groups = vintages)
    })
    output$PCAWinequestion <- renderText({
      "How much information is contained in this plot? Which of the three varieties are most different?  The colors match the plot on the left."})
  })

  observeEvent(input$showLoadings, {
    output$PCALoadings <- renderPlot({
      if (input$PC2 == "none") {
        pcs <- input$PC1
      } else {
        pcs <- unique(c(input$PC1, input$PC2))
      }
      
      loadingplot(PCA.PCA, pc = pcs)
    })
  })
}

shinyApp(ui = ui, server = server)
