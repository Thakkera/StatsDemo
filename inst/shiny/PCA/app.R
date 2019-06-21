require(shiny)
require(shinydashboard)
require(StatsDemo)

sidebar <- dashboardSidebar(
  helpText("Understanding PCA"),
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
             menuSubItem("Wine data", tabName = "ex3Wine")),
    menuItem("4: The effect of scaling",
             menuSubItem("Introduction", tabName = "ex4"),
             menuSubItem("Lambrusco, lambrusco!", tabName = "ex4Lamb")),
    menuItem("5: Size matters",
             menuSubItem("Introduction", tabName = "ex5"),
             menuSubItem("No sparkles", tabName = "ex5Wine"),
             menuSubItem("Sparkles", tabName = "ex5Lamb"))
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
                   p("\nHere you will look at the loadings for the toy data set containing 2,967 objects and 10 variables (the letters). First inspect the individual loadings, and discuss how to interpret the plots; second, show two components at the same time.")),
            column(width = 4,
                   p("\nIn the dropdown boxes to the right you can select one or two components for which loadings are shown.")),
            column(width = 4,
                   column(width = 4,
                          selectInput("PC1", label = "First PC",
                                      choices = paste(1:10))),
                   column(width = 4,
                          selectInput("PC2", label = "Second PC",
                                      choices = c("none", paste(1:10)))),
                   column(width = 4,
                          actionButton("showLoadings", "Show loadings!")))),
          fluidRow(box(plotOutput(outputId = "PCALoadings"),
                       align = "center", width=12)),
          fluidRow(align = "center",
                   textOutput(outputId = "PCALoadingQuestion"))
          )

tabEx2W <-
  tabItem(tabName = "ex2Wine",
          fluidRow(
            column(width = 4,
                   p("\nHere we focus on the loadings of the wine data set. First inspect the individual loadings, and discuss how to interpret the plots; second, show two components at the same time.")),
            column(width = 4,
                   p("\nIn the dropdown boxes to the right you can select one or two components for which loadings are shown.")),
            column(width = 4,
                   column(width = 4,
                          selectInput("PC1W", label = "First PC",
                                      choices = paste(1:10))),
                   column(width = 4,
                          selectInput("PC2W", label = "Second PC",
                                      choices = c("none", paste(1:10)))),
                   column(width = 4,
                          actionButton("showLoadingsWine", "Show loadings!")))),
          fluidRow(box(plotOutput(outputId = "PCALoadingsWine"),
                       align = "center", width=12)),
          fluidRow(align = "center",
                   textOutput(outputId = "PCALoadingQuestionWine"))
          )

tabEx3 <-
  tabItem(tabName = "ex3",
          h2("\nExercise 3: Biplots"),
          box(p("Biplots show a scoreplot and a loading plot in the same panel, superimposed. This allows you to easily relate groupings or effects in the scores to particular variable. Once you got the idea it can be pretty powerful. General remark: grouping structure is not always visible in a PCA, especially when it depends on a few variables only."),
              p("Have a look at the data sets in the sidebar, and discuss the results. Some questions will appear below the plots"), width = 8))

tabEx3W <-
  tabItem(tabName = "ex3Wine",
          fluidRow(
            column(width = 6,
                   p("\nThe real power of PCA lies in combining score and loading plots. In the two leftmost plots below, you'll see both. Combining them in the biplot on the right leads in many cases to easier interpretation. ")),
            column(width = 4,
                   p("\nIn the dropdown boxes to the right you can select which components to visualize. (The lowest component will always be shown on the x axis - if you select the same component twice you'll be ignored ;P.)")),
            column(width = 1,
                   selectInput("PC1WB", label = "First PC",
                               choices = paste(1:10),
                               selected = "1")),
            column(width = 1,
                   selectInput("PC2WB", label = "Second PC",
                               choices = paste(1:10),
                              selected = "2"))),
          fluidRow(box(plotOutput(outputId = "PCABiplotWine"),
                       align = "center", width=12)),
          fluidRow(align = "center",
                   htmlOutput(outputId = "PCABiplotQuestionWine"))
          )

tabEx4 <- 
  tabItem(tabName = "ex4",
          h2("\nExercise 4: The Effects of Scaling"),
          box(p("So far, PCA seems easy enough - for visual inspection of a high-dimensional data set we usually concentrate on the first PCs only. However, scaling matters, and finding the most suitable scaling is usually hard to do without information on what the variables actually mean."),
              p("Here we present yet another wine data set from mass-spectrometry-based metabolomics. The variables are the intensities of the individual mass peaks. There are quite large differences between intensities, and this is going to influence the results"),
              p("Check out a couple of scalings, all commonly used in metabolomics, and discuss the effects on the scores and on the loadings. Note all scaling methods will be applied after mean-centering the data, which is mandatory before doing PCA."), width = 8))

tab4ExLS <-
  tabItem(tabName = "ex4Lamb",
          fluidRow(
            column(width = 6,
                   p("\nLambrusco is a sparkling red wine from Italy with a pretty bad reputation in the past: lemonade with alcohol was one of the more friendly connotations. However, there are some very good ones with distinct flavours and undeniable charm."),
                   p("This data set contains 76 samples, and we have 1,208 variables. The samples are divided into three groups, corresponding to the origin of the wine. A key feature of such data is that the majority of the feature intensities is comparable in size, but typically there are a few VERY LARGE numbers present.")),
            column(width = 4,
                   p("\nBelow, you'll see the score plot and the loading plot for a given scaling method. First, notice that the numbers at the axes of the score and loading plots are identical. Is this a coincidence?"),
                   p("In the dropdown box to the right you can select different options. This will lead to very different results below - the question to you is whether you understand the changes.")),
            column(width = 2,
                   selectInput("LScaling",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" = "logauto",
                                           "Sqrt scaling plus autoscaling" = "sqrtauto")),
                   actionButton("showLambrusco", "Go!"))),
          fluidRow(box(plotOutput(outputId = "LamboScores")),
                   box(plotOutput(outputId = "LamboLoadings"))),
          fluidRow(align = "center",
                   textOutput(outputId = "LambruscoQuestion"))
          )

tabEx5 <- 
  tabItem(tabName = "ex5",
          h2("\nExercise 5: Choosing the 'Correct' Number of Components"),
          box(p("We may wonder how many PCs to consider to obtain a good overview of the multivariate data. Of course, this depends on the data set. The original idea that it should be possible to distinguish 'significant' components from 'non-significant' components (signal versus noise) is more or less extinct, which is why we put the title word Correct in quotes. This is exploratory analysis! The question is: what plots show you something interesting?"),
              p("Nevertheless there are a couple of rules of thumb that can be used. First of all, we may decide that the first two components are enough, period. This is quite common in visualization applications. Secondly, we may require a certain percentage of variance explained - typically something like 80%, or, for data sets with more variables, 50%. Thirdly, we may look at scree plots."),
              p("Here you will look at a number of criteria for the wine data. Note that, again, the optimal number of components really depends on the scaling. In addition, the same exercise can be done for a data set with many more variables."), width = 8))

tab5ExW <-
  tabItem(tabName = "ex5Wine",
          fluidRow(
            column(width = 6,
                   p("\nThe wine data - just to remind you - 177 samples, three varieties, and 13 (very different) variables. You choose the scaling and you'll see the two types of screeplot (log variance, and cumulative percentage explained). Choose the number of components and you'll see the corresponding scores.")),
            column(width = 2,
                   selectInput("WineNComp", label = "Number of PCs",
                               choices = 1:13, 
                               selected = "2")),
            column(width = 2,
                   selectInput("WScalingScree",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" = "logauto",
                                           "Sqrt scaling plus autoscaling" = "sqrtauto"))),
            column(width = 2,
                   actionButton("showWineScree", "Go!"))
          ),
          fluidRow(
            column(width = 6,
                   box(plotOutput(outputId = "WineScree"), width = 12)),
            column(width = 6,
                   box(plotOutput(outputId = "WineScorePairs"), width = 12))),
          fluidRow(align = "center",
                   textOutput(outputId = "WineScreeQuestion"))
          )

tab5ExL <-
  tabItem(tabName = "ex5Lamb",
          fluidRow(
            column(width = 6,
                   p("\nThe Lambrusco data again: 76 samples, and 1,208 variables. You choose the scaling and you'll see the two types of screeplot (log variance, and cumulative percentage explained). Choose the number of components and you'll see the corresponding scores.")),
            column(width = 2,
                   selectInput("LambNComp", label = "Number of PCs",
                               choices = paste(1:10), 
                               selected = "2")),
            column(width = 2,
                   selectInput("LScalingScree",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" = "logauto",
                                           "Sqrt scaling plus autoscaling" = "sqrtauto"))),
            column(width = 2,
                   actionButton("showLambruscoScree", "Go!"))
          ),
          fluidRow(
            column(width = 6,
                   box(plotOutput(outputId = "LamboScree"), width = 12)),
            column(width = 6,
                   box(plotOutput(outputId = "LamboScorePairs"), width = 12))),
          fluidRow(align = "center",
                   textOutput(outputId = "LambruscoScreeQuestion"))
          )

server <- function(input, output) {
  data(PCADATA)
  data(wines)
  data(lambrusco)

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
    output$PCALoadingQuestion <- renderText({
      "Additional question: in the PC 1 vs PC 2 plot, which variables show high correlation?"
      })
    output$PCALoadings <- renderPlot({
      if (input$PC2 == "none") {
        pcs <- as.numeric(input$PC1)
      } else {
        pcs <- as.numeric(unique(c(input$PC1, input$PC2)))
      }
      
      loadingplot(PCA.PCA, pc = pcs, show.names = TRUE)
    })
  })

  observeEvent(input$showLoadingsWine, {
    output$PCALoadingQuestionWine <- renderText({
      "Additional question: in the PC 1 vs PC 2 plot, which variables show high correlation?"
      })
    output$PCALoadingsWine <- renderPlot({
      if (input$PC2W == "none") {
        pcs <- as.numeric(input$PC1W)
      } else {
        pcs <- as.numeric(unique(c(input$PC1W, input$PC2W)))
      }
      
      loadingplot(wine.PCA, pc = pcs, show.names = TRUE)
    })
  })

  output$PCABiplotWine <- renderPlot({
    pcs <- as.numeric(unique(c(input$PC1WB, input$PC2WB)))
    if (length(pcs) == 1)
      pcs <- c(pcs, min(setdiff(1:10, pcs)))
    pcs <- sort(pcs)

    par(mfrow = c(1,3))
    scoreplot(wine.PCA, pc = pcs, col = as.integer(vintages))
    loadingplot(wine.PCA, pc = pcs, show.names = TRUE)
    biplot(wine.PCA, pc = pcs, score.col = as.integer(vintages), show.names = "loadings")
    })
  
  output$PCABiplotQuestionWine <- renderText({
    "Note that the percentage of variance explained at the axes is equal in all plots. Also note that the loading axes in the biplot have been adapted to fit the loadings comfortably in the plot.<br><b>Question</b>: which variables are most discriminative for each wine? That is, which do you expect to have high values in a particular wine, and which have low values?"})
  
  observeEvent(input$showLambrusco, {
    Lambo.PCA <- reactive(
      switch(input$LScaling,
             "mean" = PCA(scale(X, scale = FALSE)),
             "auto" = PCA(scale(X)),
             "pareto" = PCA(scale(X, scale = apply(X, 2, function(x) sqrt(sd(x))))),
             "log" = PCA(scale(log(X + 1), scale = FALSE)),
             "sqrt" = PCA(scale(sqrt(X), scale = FALSE)),
             "logauto" = PCA(scale(log(X + 1))),
             "sqrtauto" = PCA(scale(sqrt(X))))
    )
    
    output$LambruscoQuestion <- renderText({
      "Which scaling separates the groups best? One of the scalings leads to a very strange division in two groups - have you found it?"
    })
    
    output$LamboLoadings <- renderPlot({
      loadingplot(Lambo.PCA(), pc = 1:2)
    })
    output$LamboScores <- renderPlot({
      myscoreplot(Lambo.PCA(), pc = 1:2, groups = sample.labels)
    })

  })

  observeEvent(input$showWineScree, {
    output$WineScreeQuestion <- renderText({
      "How many PCs would you pick?"})
    
    Wine.PCAScr <- reactive({
      switch(input$WScalingScree,
             "mean" = PCA(scale(wines, scale = FALSE)),
             "auto" = PCA(scale(wines)),
             "pareto" = PCA(scale(wines, scale =
                                       apply(wines, 2, function(x) sqrt(sd(x))))),
             "log" = PCA(scale(log(wines + 1), scale = FALSE)),
             "sqrt" = PCA(scale(sqrt(wines), scale = FALSE)),
             "logauto" = PCA(scale(log(wines + 1))),
             "sqrtauto" = PCA(scale(sqrt(wines))))
    })
    
    output$WineScree <- renderPlot({
      par(mfrow = c(2,1), mar = c(4, 4.2, 0, 1))
      screeplot(Wine.PCAScr())
      abline(h = 0, col = "gray", lty = 2)
      screeplot(Wine.PCAScr(), ylim = c(0, 100), type = "percentage")
      abline(h = c(0, 100), col = "gray", lty = 2)
    })
    
    output$WineScorePairs <- renderPlot({
      if (as.integer(input$WineNComp) < 3) {
        scoreplot(Wine.PCAScr(), col = wine.classes, pch = wine.classes)
      } else {
        splom(scores(Wine.PCAScr(), as.integer(input$WineNComp)),
              pscales = 0, groups = vintages, pch = ".", cex = 2)
      }
    })
  })  

  observeEvent(input$showLambruscoScree, {
    output$LambruscoScreeQuestion <- renderText({
      "Here we limit your choice to the first ten PCs. There are more - how many PCs could you choose at most?"})
    
    Lambo.PCAScr <- reactive({
      switch(input$LScalingScree,
             "mean" = PCA(scale(X, scale = FALSE)),
             "auto" = PCA(scale(X)),
             "pareto" = PCA(scale(X, scale =
                                       apply(X, 2, function(x) sqrt(sd(x))))),
             "log" = PCA(scale(log(X + 1), scale = FALSE)),
             "sqrt" = PCA(scale(sqrt(X), scale = FALSE)),
             "logauto" = PCA(scale(log(X + 1))),
             "sqrtauto" = PCA(scale(sqrt(X))))
    })
    
    output$LamboScree <- renderPlot({
      par(mfrow = c(2,1), mar = c(4, 4.2, 0, 1))
      screeplot(Lambo.PCAScr(), npc = 10)
      abline(h = 0, col = "gray", lty = 2)
      screeplot(Lambo.PCAScr(), npc = 10,
                ylim = c(0, 100), type = "percentage")
      abline(h = c(0, 100), col = "gray", lty = 2)
    })
    
    output$LamboScorePairs <- renderPlot({
      if (as.integer(input$LambNComp) < 3) {
        scoreplot(Lambo.PCAScr(), col = as.integer(sample.labels),
                  pch = as.integer(sample.labels))
      } else {
        splom(scores(Lambo.PCAScr(), as.integer(input$LambNComp)),
              pscales = 0, groups = sample.labels, pch = ".", cex = 2)
      }
    })
  })  
}

body <- dashboardBody(
  tabItems(tabEx1, tabEx1S, tabEx1W,
           tabEx2, tabEx2S, tabEx2W,
           tabEx3, tabEx3W,
           tabEx4, tab4ExLS,
           tabEx5, tab5ExW, tab5ExL)
)

ui <- dashboardPage(
  dashboardHeader(title = "PCA exercises"),
  sidebar,
  body
)

shinyApp(ui = ui, server = server)
