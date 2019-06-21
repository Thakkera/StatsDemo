require(shiny)
require(shinydashboard)
require(StatsDemo)

sidebar <- dashboardSidebar(
  helpText("Understanding PLS"),
  sidebarMenu(
    menuItem("Introduction", tabName = "intro"),
    menuItem("1: Score plots", 
             menuSubItem("Introduction", tabName = "ex1"),
             menuSubItem("Wine data", tabName = "ex1Wine")),
    menuItem("2: Regression coefficients",
             menuSubItem("Introduction", tabName = "ex2"),
             menuSubItem("Wine data", tabName = "ex2Wine"),
             menuSubItem("Lambrusco data", tabName = "ex2Lambo")),
    menuItem("3: Prediction", 
             menuSubItem("Introduction", tabName = "ex3"),
             menuSubItem("Wine data", tabName = "ex3Wine")),
    menuItem("4: Size matters",
             menuSubItem("Introduction", tabName = "ex5"),
             menuSubItem("No sparkles", tabName = "ex5Wine"),
             menuSubItem("Sparkles", tabName = "ex5Lamb"))
  )
)

tabIntro <-
  tabItem(tabName = "intro",
          h2("Introduction"),
          box(p("We have seen that PCA can be a really useful visualization method useful to find outliers or see groupings, also unexpected ones... However, PCA will only show this to you if the structure is present globally, that is: if a large part of the variation in the data is associated with the structure you would like to see. In many cases, the structure is present in only a small part of the data, and PCA will not be the best method to tackle the data.")),
          box(p("But: very often we know the structure, at least for a limited 
number of samples, and we can use this to make predictive models that are more powerful that the ones based on PCA. Prime example: PLS. The acronym is usually taken to stand for 'Partial Least Squares' regression, although there are people who think otherwise.")))

tabEx1 <-
  tabItem(tabName = "ex1",
          h2("\nExercise 1: Score Plots"),
          box(p("Just like PCA, PLS does dimension reduction: new variables are created (linear combinations of the original ones) but PLS takes into account the dependent variable, i.e., the variable we want to predict, too!"),
              p("The next exercise compares PCA and PLS scores."), width = 8))

tabEx1W <- 
  tabItem(tabName = "ex1Wine",
          fluidRow(
            column(width = 6,
                   p("\nNow consider the wine data: 177 different bottles from three varieties. We have measured 13 variables for each bottle. Left you'll see the PCA scores - right the PLS scores (we are predicting class label here). We are concentrating on the first two components here. What differences do you observe? Try out different scaling methods.")),
            column(width = 4,
                   selectInput("WScaling",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" =
                                             "logauto",
                                           "Sqrt scaling plus autoscaling" =
                                             "sqrtauto"))),
           column(width = 2,
                  actionButton("showPCAPLSscores", "Go!"))
          ),
          fluidRow(
            box(plotOutput(outputId = "WineScores"), width=6),
            box(plotOutput(outputId = "WinePLSScores"), width = 6)),
          fluidRow(align = "center",
                   textOutput(outputId = "PLSWinequestion")
                   ))

tabEx2 <-
  tabItem(tabName = "ex2",
          h2("\nExercise 2: regression coefficients"),
          box(p("PLS loadings have more or less the same interpretation as PCA loadings. However, since PLS is a regression technique in most cases one considers the loadings as technical issues (although opinions differ here, too), and concentrates on the regression vector. One definite advantage of the regression vector is that it is, well, a vector. It can be visualized pretty easily, whereas the loadings of a model with say four components are more difficult. What is more: PLS models not only have loadings for the dependent variables (the X matrix) but if Y is a matrix there are also Y loadings."), 
              p("In this exercise we'll focus on the regression vectors rather than on the loadings. We'll be looking at two wine data sets."), width = 8))

tabEx2W <-
  tabItem(tabName = "ex2Wine",
          fluidRow(
            column(width = 6,
                   p("\nIn this exercise we'll look at the regression vectors predicting the alcohol content from the other 12 variables in the wine data (so we'll disregard variety for the moment). See how the regression vector changes when changing scaling and the number of latent variables in the model.")),
            column(width = 4,
                   selectInput("WScalingC",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" = "logauto",
                                           "Sqrt scaling plus autoscaling" = "sqrtauto"))),
            column(width = 1,
                   selectInput("WnLV", label = "Nr of LVs", choices = 1:8)),
            column(width = 1,
                   actionButton("winePLScoefs", "Go!"))),
          fluidRow(box(plotOutput(outputId = "WinePLSCoefs"),
                       align = "center", width=12)),
          fluidRow(align = "center",
                   textOutput(outputId = "PLSCoefQuestionWine")))

tabEx2L <-
  tabItem(tabName = "ex2Lambo",
          fluidRow(
            column(width = 6,
                   p("\nPLS can also be used in a classification context. With two classes we only need use labels like 0 and 1 (with a cutoff of 0.5) or -1 and 1 (with a cutoff of 0). For more than three classes, PLS is basically modelling class memberships, so for each of the classes the probability of belonging to that class. Here, we are predicting origin (three possibilities) for the Lambrusco data.")),
            column(width = 3,
                   selectInput("LScalingC",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" = "logauto",
                                           "Sqrt scaling plus autoscaling" = "sqrtauto")),
                   selectInput("LnLV", label = "Nr of LVs", choices = 1:15)),
            column(width = 2,
                   selectInput("VMin", label = "Min",
                               choices = 100*0:11 + 1,
                               selected = 1),
                   selectInput("VMax", label = "Max",
                               choices = 100*0:11 + 108,
                               selected = 1208)),
            column(width = 1,
                   actionButton("lamboPLScoefs", "Go!"))),
          fluidRow(box(plotOutput(outputId = "LamboPLSCoefs"),
                       align = "center", width=12)),
          fluidRow(align = "center",
                   textOutput(outputId = "PLSCoefQuestionLambo")))

## tabEx3 <-
##   tabItem(tabName = "ex3",
##           h2("\nExercise 3: Biplots"),
##           box(p("Biplots show a scoreplot and a loading plot in the same panel, superimposed. This allows you to easily relate groupings or effects in the scores to particular variable. Once you got the idea it can be pretty powerful. General remark: grouping structure is not always visible in a PCA, especially when it depends on a few variables only."),
##               p("Have a look at the data sets in the sidebar, and discuss the results. Some questions will appear below the plots"), width = 8))

## tabEx3W <-
##   tabItem(tabName = "ex3Wine",
##           fluidRow(
##             column(width = 6,
##                    p("\nThe real power of PCA lies in combining score and loading plots. In the two leftmost plots below, you'll see both. Combining them in the biplot on the right leads in many cases to easier interpretation. ")),
##             column(width = 4,
##                    p("\nIn the dropdown boxes to the right you can select which components to visualize. (The lowest component will always be shown on the x axis - if you select the same component twice you'll be ignored ;P.)")),
##             column(width = 1,
##                    selectInput("PC1WB", label = "First PC",
##                                choices = paste(1:10),
##                                selected = "1")),
##             column(width = 1,
##                    selectInput("PC2WB", label = "Second PC",
##                                choices = paste(1:10),
##                               selected = "2"))),
##           fluidRow(box(plotOutput(outputId = "PCABiplotWine"),
##                        align = "center", width=12)),
##           fluidRow(align = "center",
##                    htmlOutput(outputId = "PCABiplotQuestionWine"))
##           )

## tabEx4 <- 
##   tabItem(tabName = "ex4",
##           h2("\nExercise 4: The Effects of Scaling"),
##           box(p("So far, PCA seems easy enough - for visual inspection of a high-dimensional data set we usually concentrate on the first PCs only. However, scaling matters, and finding the most suitable scaling is usually hard to do without information on what the variables actually mean."),
##               p("Here we present yet another wine data set from mass-spectrometry-based metabolomics. The variables are the intensities of the individual mass peaks. There are quite large differences between intensities, and this is going to influence the results"),
##               p("Check out a couple of scalings, all commonly used in metabolomics, and discuss the effects on the scores and on the loadings. Note all scaling methods will be applied after mean-centering the data, which is mandatory before doing PCA."), width = 8))

## tab4ExLS <-
##   tabItem(tabName = "ex4Lamb",
##           fluidRow(
##             column(width = 6,
##                    p("\nLambrusco is a sparkling red wine from Italy with a pretty bad reputation in the past: lemonade with alcohol was one of the more friendly connotations. However, there are some very good ones with distinct flavours and undeniable charm."),
##                    p("This data set contains 76 samples, and we have 1,208 variables. The samples are divided into three groups, corresponding to the origin of the wine. A key feature of such data is that the majority of the feature intensities is comparable in size, but typically there are a few VERY LARGE numbers present.")),
##             column(width = 4,
##                    p("\nBelow, you'll see the score plot and the loading plot for a given scaling method. First, notice that the numbers at the axes of the score and loading plots are identical. Is this a coincidence?"),
##                    p("In the dropdown box to the right you can select different options. This will lead to very different results below - the question to you is whether you understand the changes.")),
##             column(width = 2,
##                    selectInput("LScaling",
##                                label = "Choose scaling",
##                                choices = c("Mean centering" = "mean",
##                                            "Autoscaling" = "auto",
##                                            "Pareto scaling" = "pareto",
##                                            "Log scaling" = "log",
##                                            "Sqrt scaling" = "sqrt",
##                                            "Log scaling plus autoscaling" = "logauto",
##                                            "Sqrt scaling plus autoscaling" = "sqrtauto")),
##                    actionButton("showLambrusco", "Go!"))),
##           fluidRow(box(plotOutput(outputId = "LamboScores")),
##                    box(plotOutput(outputId = "LamboLoadings"))),
##           fluidRow(align = "center",
##                    textOutput(outputId = "LambruscoQuestion"))
##           )

## tabEx5 <- 
##   tabItem(tabName = "ex5",
##           h2("\nExercise 5: Choosing the 'Correct' Number of Components"),
##           box(p("We may wonder how many PCs to consider to obtain a good overview of the multivariate data. Of course, this depends on the data set. The original idea that it should be possible to distinguish 'significant' components from 'non-significant' components (signal versus noise) is more or less extinct, which is why we put the title word Correct in quotes. This is exploratory analysis! The question is: what plots show you something interesting?"),
##               p("Nevertheless there are a couple of rules of thumb that can be used. First of all, we may decide that the first two components are enough, period. This is quite common in visualization applications. Secondly, we may require a certain percentage of variance explained - typically something like 80%, or, for data sets with more variables, 50%. Thirdly, we may look at scree plots."),
##               p("Here you will look at a number of criteria for the wine data. Note that, again, the optimal number of components really depends on the scaling. In addition, the same exercise can be done for a data set with many more variables."), width = 8))

## tab5ExW <-
##   tabItem(tabName = "ex5Wine",
##           fluidRow(
##             column(width = 6,
##                    p("\nThe wine data - just to remind you - 177 samples, three varieties, and 13 (very different) variables. You choose the scaling and you'll see the two types of screeplot (log variance, and cumulative percentage explained). Choose the number of components and you'll see the corresponding scores.")),
##             column(width = 2,
##                    selectInput("WineNComp", label = "Number of PCs",
##                                choices = 1:13, 
##                                selected = "2")),
##             column(width = 2,
##                    selectInput("WScalingScree",
##                                label = "Choose scaling",
##                                choices = c("Mean centering" = "mean",
##                                            "Autoscaling" = "auto",
##                                            "Pareto scaling" = "pareto",
##                                            "Log scaling" = "log",
##                                            "Sqrt scaling" = "sqrt",
##                                            "Log scaling plus autoscaling" = "logauto",
##                                            "Sqrt scaling plus autoscaling" = "sqrtauto"))),
##             column(width = 2,
##                    actionButton("showWineScree", "Go!"))
##           ),
##           fluidRow(
##             column(width = 6,
##                    box(plotOutput(outputId = "WineScree"), width = 12)),
##             column(width = 6,
##                    box(plotOutput(outputId = "WineScorePairs"), width = 12))),
##           fluidRow(align = "center",
##                    textOutput(outputId = "WineScreeQuestion"))
##           )

## tab5ExL <-
##   tabItem(tabName = "ex5Lamb",
##           fluidRow(
##             column(width = 6,
##                    p("\nThe Lambrusco data again: 76 samples, and 1,208 variables. You choose the scaling and you'll see the two types of screeplot (log variance, and cumulative percentage explained). Choose the number of components and you'll see the corresponding scores.")),
##             column(width = 2,
##                    selectInput("LambNComp", label = "Number of PCs",
##                                choices = paste(1:10), 
##                                selected = "2")),
##             column(width = 2,
##                    selectInput("LScalingScree",
##                                label = "Choose scaling",
##                                choices = c("Mean centering" = "mean",
##                                            "Autoscaling" = "auto",
##                                            "Pareto scaling" = "pareto",
##                                            "Log scaling" = "log",
##                                            "Sqrt scaling" = "sqrt",
##                                            "Log scaling plus autoscaling" = "logauto",
##                                            "Sqrt scaling plus autoscaling" = "sqrtauto"))),
##             column(width = 2,
##                    actionButton("showLambruscoScree", "Go!"))
##           ),
##           fluidRow(
##             column(width = 6,
##                    box(plotOutput(outputId = "LamboScree"), width = 12)),
##             column(width = 6,
##                    box(plotOutput(outputId = "LamboScorePairs"), width = 12))),
##           fluidRow(align = "center",
##                    textOutput(outputId = "LambruscoScreeQuestion"))
##           )

server <- function(input, output) {
  data(wines)
  wines.cl <- classvec2classmat(vintages)
  data(lambrusco)
  lambo.cl <- classvec2classmat(sample.labels)

  myscoreplot <- function(PCAobj, pcs = c(1,2),
                          groups = rep(1, nrow(PCAscores))) {
    pcascores <- scores(PCAobj)
    PCAscores <- data.frame(PCa = pcascores[,pcs[1]],
                            PCb = pcascores[,pcs[2]],
                            groups = groups)
    if (!is.null(PCAobj$totalvar)) {
      PCAperc <- round(100*PCAobj$var[pcs] / PCAobj$totalvar, 1)
    } else { ## PLS model
      PCAperc <- round(100*PCAobj$Xvar / PCAobj$Xtotvar, 1)
    }
    
    xyplot(PCb ~ PCa, data = PCAscores, groups = groups,
           xlab = paste("PC ", pcs[1], " (", PCAperc[1], "%)", sep = ""),
           ylab = paste("PC ", pcs[2], " (", PCAperc[2], "%)", sep = ""),
           panel = function(...) {
             panel.abline(h = 0, v = 0, col = "gray", lty = 2)
             panel.xyplot(...)
           })    
  }
  
  observeEvent(input$showPCAPLSscores, {
    WinesX <- reactive(
      switch(input$WScaling,
             "mean" = scale(wines, scale = FALSE),
             "auto" = scale(wines),
             "pareto" = scale(wines, scale = apply(wines, 2, function(x) sqrt(sd(x)))),
             "log" = scale(log(wines + 1), scale = FALSE),
             "sqrt" = scale(sqrt(wines), scale = FALSE),
             "logauto" = scale(log(wines + 1)),
             "sqrtauto" = scale(sqrt(wines))))
    
    
    output$WineScores <- renderPlot({
      wine.PCA <- PCA(WinesX())
      myscoreplot(wine.PCA, groups = vintages)
    })
    
    output$WinePLSScores <- renderPlot({
      wine.PLS <- plsr(wines.cl ~ WinesX())
      myscoreplot(wine.PLS, groups = vintages)
    })
    
    output$PLSWinequestion <- renderText({
      "In most cases the amount of variation covered in the PCA scores will be larger than in the PLS scores. Any idea why?"})
  })

  observeEvent(input$winePLScoefs, {
    WinesX <- reactive(
      switch(input$WScalingC,
             "mean" = scale(wines, scale = FALSE),
             "auto" = scale(wines),
             "pareto" = scale(wines, scale = apply(wines, 2, function(x) sqrt(sd(x)))),
             "log" = scale(log(wines + 1), scale = FALSE),
             "sqrt" = scale(sqrt(wines), scale = FALSE),
             "logauto" = scale(log(wines + 1)),
             "sqrtauto" = scale(sqrt(wines))))
    
    
    output$WinePLSCoefs <- renderPlot({
      wine.PLS <- plsr(alcohol ~ ., data = as.data.frame(WinesX()))
      plot(coef(wine.PLS, as.integer(input$WnLV)), type = "h", col = 4,
           ylab = "Coefficient size")
      abline(h = 0, col = "gray")
    })
    
    output$PLSCoefQuestionWine <- renderText({
      "Which variables are most important in the predictions? Which are least important? Is this consistent for all scaling methods? Can you (to some extent, at least) understand the influence of the scaling methods on the coefficient sizes?"}) 
  })

  observeEvent(input$lamboPLScoefs, {
    output$LamboPLSCoefs <- renderPlot({
      LamboX <- 
        switch(input$LScalingC,
               "mean" = scale(X, scale = FALSE),
               "auto" = scale(X),
               "pareto" = scale(X, scale = apply(X, 2, function(x) sqrt(sd(x)))),
               "log" = scale(log(X + 1), scale = FALSE),
               "sqrt" = scale(sqrt(X), scale = FALSE),
               "logauto" = scale(log(X + 1)),
               "sqrtauto" = scale(sqrt(X)))
      
      lambo.PLS <- plsr(lambo.cl ~ LamboX, ncomp = 15)
      plscoefs <- coef(lambo.PLS, as.integer(input$LnLV))[,,1]
      plscoefs.df <- data.frame("coef" = c(plscoefs),
                                "origin" = rep(colnames(lambo.cl),
                                               each = ncol(LamboX)),
                                "var" = 1:ncol(LamboX))
      xyplot(coef ~ var | origin, data = plscoefs.df, groups = origin,
             subset = var >= min(c(input$VMin, input$VMax)) &
               var <= max(c(input$VMin, input$VMax)),
             panel = panel.superpose,
             panel.groups = function(...) {
               panel.abline(h = 0, col = "gray")
               panel.xyplot(...)
             },
             xlim = c(sort(as.numeric(c(input$VMin, input$VMax)))),
             scale = list(y = "free"),
             type = "h", layout = c(1,3), ylab = "Coefficient size")
    })
    
    output$PLSCoefQuestionLambo <- renderText({
      "For each of the three classes a separate regression vector is obtained. Which variables are specific for which classes? What characteristics are important?"}) 
  })
}

    
  ## observeEvent(input$showLoadingsWine, {
  ##   output$PCALoadingQuestionWine <- renderText({
  ##     "Additional question: in the PC 1 vs PC 2 plot, which variables show high correlation?"
  ##     })
  ##   output$PCALoadingsWine <- renderPlot({
  ##     if (input$PC2W == "none") {
  ##       pcs <- as.numeric(input$PC1W)
  ##     } else {
  ##       pcs <- as.numeric(unique(c(input$PC1W, input$PC2W)))
  ##     }
      
  ##     loadingplot(wine.PCA, pc = pcs, show.names = TRUE)
  ##   })
  ## })

  ## output$PCABiplotWine <- renderPlot({
  ##   pcs <- as.numeric(unique(c(input$PC1WB, input$PC2WB)))
  ##   if (length(pcs) == 1)
  ##     pcs <- c(pcs, min(setdiff(1:10, pcs)))
  ##   pcs <- sort(pcs)

  ##   par(mfrow = c(1,3))
  ##   scoreplot(wine.PCA, pc = pcs, col = as.integer(vintages))
  ##   loadingplot(wine.PCA, pc = pcs, show.names = TRUE)
  ##   biplot(wine.PCA, pc = pcs, score.col = as.integer(vintages), show.names = "loadings")
  ##   })
  
  ## output$PCABiplotQuestionWine <- renderText({
  ##   "Note that the percentage of variance explained at the axes is equal in all plots. Also note that the loading axes in the biplot have been adapted to fit the loadings comfortably in the plot.<br><b>Question</b>: which variables are most discriminative for each wine? That is, which do you expect to have high values in a particular wine, and which have low values?"})
  
  ## observeEvent(input$showLambrusco, {
  ##   Lambo.PCA <- reactive(
  ##     switch(input$LScaling,
  ##            "mean" = PCA(scale(X, scale = FALSE)),
  ##            "auto" = PCA(scale(X)),
  ##            "pareto" = PCA(scale(X, scale = apply(X, 2, function(x) sqrt(sd(x))))),
  ##            "log" = PCA(scale(log(X + 1), scale = FALSE)),
  ##            "sqrt" = PCA(scale(sqrt(X), scale = FALSE)),
  ##            "logauto" = PCA(scale(log(X + 1))),
  ##            "sqrtauto" = PCA(scale(sqrt(X))))
  ##   )
    
  ##   output$LambruscoQuestion <- renderText({
  ##     "Which scaling separates the groups best? One of the scalings leads to a very strange division in two groups - have you found it?"
  ##   })
    
  ##   output$LamboLoadings <- renderPlot({
  ##     loadingplot(Lambo.PCA(), pc = 1:2)
  ##   })
  ##   output$LamboScores <- renderPlot({
  ##     myscoreplot(Lambo.PCA(), pc = 1:2, groups = sample.labels)
  ##   })

  ## })

  ## observeEvent(input$showWineScree, {
  ##   output$WineScreeQuestion <- renderText({
  ##     "How many PCs would you pick?"})
    
  ##   Wine.PCAScr <- reactive({
  ##     switch(input$WScalingScree,
  ##            "mean" = PCA(scale(wines, scale = FALSE)),
  ##            "auto" = PCA(scale(wines)),
  ##            "pareto" = PCA(scale(wines, scale =
  ##                                      apply(wines, 2, function(x) sqrt(sd(x))))),
  ##            "log" = PCA(scale(log(wines + 1), scale = FALSE)),
  ##            "sqrt" = PCA(scale(sqrt(wines), scale = FALSE)),
  ##            "logauto" = PCA(scale(log(wines + 1))),
  ##            "sqrtauto" = PCA(scale(sqrt(wines))))
  ##   })
    
  ##   output$WineScree <- renderPlot({
  ##     par(mfrow = c(2,1), mar = c(4, 4.2, 0, 1))
  ##     screeplot(Wine.PCAScr())
  ##     abline(h = 0, col = "gray", lty = 2)
  ##     screeplot(Wine.PCAScr(), ylim = c(0, 100), type = "percentage")
  ##     abline(h = c(0, 100), col = "gray", lty = 2)
  ##   })
    
  ##   output$WineScorePairs <- renderPlot({
  ##     if (as.integer(input$WineNComp) < 3) {
  ##       scoreplot(Wine.PCAScr(), col = wine.classes, pch = wine.classes)
  ##     } else {
  ##       splom(scores(Wine.PCAScr(), as.integer(input$WineNComp)),
  ##             pscales = 0, groups = vintages, pch = ".", cex = 2)
  ##     }
  ##   })
  ## })  

  ## observeEvent(input$showLambruscoScree, {
  ##   output$LambruscoScreeQuestion <- renderText({
  ##     "Here we limit your choice to the first ten PCs. There are more - how many PCs could you choose at most?"})
    
  ##   Lambo.PCAScr <- reactive({
  ##     switch(input$LScalingScree,
  ##            "mean" = PCA(scale(X, scale = FALSE)),
  ##            "auto" = PCA(scale(X)),
  ##            "pareto" = PCA(scale(X, scale =
  ##                                      apply(X, 2, function(x) sqrt(sd(x))))),
  ##            "log" = PCA(scale(log(X + 1), scale = FALSE)),
  ##            "sqrt" = PCA(scale(sqrt(X), scale = FALSE)),
  ##            "logauto" = PCA(scale(log(X + 1))),
  ##            "sqrtauto" = PCA(scale(sqrt(X))))
  ##   })
    
  ##   output$LamboScree <- renderPlot({
  ##     par(mfrow = c(2,1), mar = c(4, 4.2, 0, 1))
  ##     screeplot(Lambo.PCAScr(), npc = 10)
  ##     abline(h = 0, col = "gray", lty = 2)
  ##     screeplot(Lambo.PCAScr(), npc = 10,
  ##               ylim = c(0, 100), type = "percentage")
  ##     abline(h = c(0, 100), col = "gray", lty = 2)
  ##   })
    
  ##   output$LamboScorePairs <- renderPlot({
  ##     if (as.integer(input$LambNComp) < 3) {
  ##       scoreplot(Lambo.PCAScr(), col = as.integer(sample.labels),
  ##                 pch = as.integer(sample.labels))
  ##     } else {
  ##       splom(scores(Lambo.PCAScr(), as.integer(input$LambNComp)),
  ##             pscales = 0, groups = sample.labels, pch = ".", cex = 2)
  ##     }
  ##   })
  ## })  

body <- dashboardBody(
  tabItems(tabIntro,
           tabEx1, tabEx1W,
           tabEx2, tabEx2W, tabEx2L))

ui <- dashboardPage(
  dashboardHeader(title = "PLS exercises"),
  sidebar,
  body)

shinyApp(ui = ui, server = server)
