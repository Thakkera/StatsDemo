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
             menuSubItem("Wine data", tabName = "ex3Wine"),
             menuSubItem("Lambrusco data", tabName = "ex3Lambrusco"),
             menuSubItem("Mystery data", tabName = "ex3Mystery"))
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
                   p("\nPLS can also be used in a classification context. With two classes we only need use labels like 0 and 1 (with a cutoff of 0.5) or -1 and 1 (with a cutoff of 0). For more than three classes, PLS is basically modelling class memberships, so for each of the classes the probability of belonging to that class. Here, we are predicting origin (three possibilities) for the Lambrusco data. Choose the minimal and maximal variable number to zoom in on particular areas in the regression vector.")),
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

tabEx3 <-
  tabItem(tabName = "ex3",
          h2("\nExercise 3: PLS Predictions"),
          box(p("Nothing is easier than getting predictions out of a PLS model. Simply pick the number of latent variables you want, and hit the 'calculate' button. However, this does not give you any idea good your predictions will be. The normal strategy is to use crossvalidation for determining the optimal number of latent variables, and then to apply the model (created using the complete training set) to an unseen test set (if available...)."),
              p("We'll have a look at three data sets. The wine data present an example of numerical prediction, the Lambrusco data of classification, and then there is the Mystery data set!"), width = 8))

tabEx3W <-
  tabItem(tabName = "ex3Wine",
          fluidRow(
            column(width = 6,
                   p("We'll predict the alcohol content of the individual wines based on the other twelve variables. Below you see the crossvalidation results for the chosen scaling. You can change the scaling and choose the number of latent variables to in the prediction, and in the panel on the right you will see the performance on a randomly selected test set of 50 samples (the others are in the training set)."),
                   plotOutput(outputId = "PLScvW", height = '300px'),
                   selectInput("PLSScalingW",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" =
                                             "logauto",
                                           "Sqrt scaling plus autoscaling" =
                                             "sqrtauto")),
                   selectInput("PLSWnLV", label = "Nr of LVs",
                               selected = 2, choices = 1:10)),
            column(width = 6,
                   box(plotOutput(outputId = "PLSpredictionsW"),
                       align = "center", width=NULL),
                   htmlOutput(outputId = "PLSPredictionQuestionWine")
                   ))
          )

tabEx3L <-
  tabItem(tabName = "ex3Lambrusco",
          fluidRow(
            column(width = 4,
                   p("Here we are going to repeat the previous exercise, but now in a classification context. Validation and prediction error measures now are given as the number of misclassified cases, rather than as a sum of squares. You can change the scaling and choose the number of latent variables to in the prediction, and you will see the performance on a randomly selected test set of 25 samples (the others are in the training set).")),
            column(width = 6,
                   box(plotOutput(outputId = "PLScvL"),
                       align = "center", width=12, height = 200)),
            column(width = 2,
                   selectInput("PLSScalingL",
                               label = "Choose scaling",
                               choices = c("Mean centering" = "mean",
                                           "Autoscaling" = "auto",
                                           "Pareto scaling" = "pareto",
                                           "Log scaling" = "log",
                                           "Sqrt scaling" = "sqrt",
                                           "Log scaling plus autoscaling" = "logauto",
                                           "Sqrt scaling plus autoscaling" = "sqrtauto")),
                   selectInput("PLSLnLV", label = "Nr of LVs", choices = 1:10)
                   )
          ),
          fluidRow(box(plotOutput(outputId = "PLSpredictionsL"),
                       align = "center", width=8),
                   box(tableOutput("PLSpredictionsLcrosstab"),
                       align = "center", width = 4)),
          fluidRow(align = "center",
                   htmlOutput(outputId = "PLSPredictionQuestionLambo"))
          )

tabEx3M <-
  tabItem(tabName = "ex3Mystery",
          fluidRow(
            column(width = 4,
                   p("Again a classification problem, a two-class problem this time (control vs treatment). There are 40 samples, 20 for each class, and 2000 variables. Mean-centering is applied since the variables are measured on the same scales. You can choose the number of latent variables to in the prediction. You will see the prediction results for the training data as well as the cross-validated results.")),
            column(width = 8,
                   box(plotOutput(outputId = "PLScvM"),
                       align = "center", width=8, height = 200),
                   box(selectInput("PLSMnLV", label = "Nr of LVs",
                                   choices = 1:10),
                       align = "center", width = 2))
          ),
          fluidRow(box(plotOutput(outputId = "PLSpredictionsM"),
                       align = "center", width=6)),
          fluidRow(align = "center",
                   htmlOutput(outputId = "PLSPredictionQuestionMystery"))
)

server <- function(input, output) {
  data(wines)
  wines.cl <- classvec2classmat(vintages)
  set.seed(7)
  winesTest.idx <- sort(sample(nrow(wines), 50))
  winesTraining.idx <- (1:nrow(wines))[-winesTest.idx]

  data(lambrusco)
  lambo.cl <- classvec2classmat(sample.labels)
  set.seed(7)
  lamboTest.idx <- sample(nrow(X), 25)
  lamboTraining.idx <- (1:nrow(X))[-lamboTest.idx]

  myscoreplot <- function(PCAobj, pcs = c(1,2),
                          groups = rep(1, nrow(PCAscores)),
                          main = "") {
    pcascores <- scores(PCAobj)
    PCAscores <- data.frame(PCa = pcascores[,pcs[1]],
                            PCb = pcascores[,pcs[2]],
                            groups = groups)
    if (!is.null(PCAobj$totalvar)) {
      PCAperc <- round(100*PCAobj$var[pcs] / PCAobj$totalvar, 1)
    } else { ## PLS model
      PCAperc <- round(100*PCAobj$Xvar / PCAobj$Xtotvar, 1)
    }
    
    xyplot(PCb ~ PCa, data = PCAscores, groups = groups, main = main,
           xlab = paste("LV ", pcs[1], " (", PCAperc[1], "%)", sep = ""),
           ylab = paste("LV ", pcs[2], " (", PCAperc[2], "%)", sep = ""),
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
      myscoreplot(wine.PCA, groups = vintages, main = "PCA")
    })
    
    output$WinePLSScores <- renderPlot({
      wine.PLS <- plsr(wines.cl ~ WinesX())
      myscoreplot(wine.PLS, groups = vintages, main = "PLS")
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
      "Which variables are most important in the predictions? Which are least important? Is this consistent for all scaling methods? How does the number of LVs affect your conclusions? Can you (to some extent, at least) understand the influence of the scaling methods on the coefficient sizes?"}) 
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

  observeEvent(input$PLSScalingW, {
    WinePLSX <- reactive(
      switch(input$PLSScalingW,
             "mean" = scale(wines, scale = FALSE),
             "auto" = scale(wines),
             "pareto" = scale(wines,
                              scale = apply(wines, 2,
                                            function(x)
                                              sqrt(sd(x)))),
             "log" = scale(log(wines + 1), scale = FALSE),
             "sqrt" = scale(sqrt(wines), scale = FALSE),
             "logauto" = scale(log(wines + 1)),
             "sqrtauto" = scale(sqrt(wines))))
  })

  PLSmodW <- plsr(alcohol ~ ., data = as.data.frame(WinePLSX()),
                  subset = winesTraining.idx,
                  ncomp = 10, validation = "LOO")
  
  output$PLScvW <- renderPlot({
    plot(PLSmodW, "validation", col = 4, lwd = 2, type = "h",
         main = paste("PLS: LOO -", input$PLSScalingW),
         ylim = c(0, max(c(RMSEP(PLSmodW, "CV")$val))))
  })
  
  output$PLSpredictionsW <- renderPlot({
    plspredictions <-
      predict(PLSmodW,
                newdata = WinesXPLS2[winesTest.idx,],
                ncomp = input$PLSWnLV)[,1,1]
      plot(plspredictions)
      ## plot(WinesXPLS[winesTest.idx, "alcohol"], plspredictions,
      ##      xlab = "True values", ylab = "Predicted values",
      ##      main = paste("Wines, test set -", input$PLSWnLV, "components"))
      ## abline(0, 1, col = 4)
      ## myR2 <- mean((plspredictions - WinesXPLS[winesTest.idx, "alcohol"])^2)
      ## legend("topleft", pch = -1, legend = paste("R2 =", round(myR2 , 2)),
      ##        bty = "n")
    })
    
    output$PLSPredictionQuestionWine <- renderText({"How close is the error estimate from the test data (right panel) to the crossvalidation estimate on the training data (left panel)?"})
##  })
}

body <- dashboardBody(
  tabItems(tabIntro,
           tabEx1, tabEx1W,
           tabEx2, tabEx2W, tabEx2L,
           tabEx3, tabEx3W, tabEx3L, tabEx3M))

ui <- dashboardPage(
  dashboardHeader(title = "PLS exercises"),
  sidebar,
  body)

shinyApp(ui = ui, server = server)
