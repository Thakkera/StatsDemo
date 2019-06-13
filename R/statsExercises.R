StatsQuiz <- function() {
  ## general multiple-choice questions on statistical testing
  getAnswer <- function(correct.id, nanswers) {
    ans <- counter <- 0
    while (!ans %in% 1:nanswers) {
      ans <- readline("\nWhat is your reply? ")
      counter <- counter + 1 
      if (ans %in% (1:nanswers)[-correct.id]) {
        cat("Incorrect answer!")
        cat("\nDiscuss the other possibilities and try again...")
        ans <- 0
      }
    }
    
    counter
  }
  
  cat("\nJust a couple (four, to be precise) of easy-peasy stats questions...")
  cat("\nThe goal is not to get it right, but to discuss the pros",
      "\nand cons of each answer with your colleague. First to finish",
      "\nthe exercise: no prizes!")
  
  readline("\n\nHere goes: hit return to continue.")
  
  cat("\nQuestion 1.")
  cat("\nYou want to determine whether the resveratrol level in",
      "\nFuji apples is the same as in Golden Delicious apples. You",
      "\nanalyse ten apples from each variety. Choose the best test",
      "\nfrom the list below:")
  cat("\n  A one-sample t-test, one-tailed (answer 1)")
  cat("\n  A one-sample t-test, two-tailed (answer 2)")
  cat("\n  A two-sample t-test, one-tailed (answer 3)")
  cat("\n  A two-sample t-test, two-tailed (answer 4)")
  cat("\n  A paired t-test, one-tailed (answer 5)")
  cat("\n  A paired t-test, two-tailed (answer 6)")
  counter <- getAnswer(4, 6)
  cat("\nYou got the correct answer in", counter,
      ifelse(counter == 1, "attempt. ", "attempts."))
  if (counter == 1) cat("Very good!")
  
  readline("\n\nReady for the next question? Hit return to continue.")
  
  cat("\nQuestion 2.")
  cat("\nYou are comparing quantification of a particular metabolite",
      "\nin targeted and untargeted methods. You take ten apples and",
      "\nanalyse them using both approaches. Choose the best test",
      "\nfrom the list below:")
  cat("\n  A one-sample t-test, one-tailed (answer 1)")
  cat("\n  A one-sample t-test, two-tailed (answer 2)")
  cat("\n  A two-sample t-test, one-tailed (answer 3)")
  cat("\n  A two-sample t-test, two-tailed (answer 4)")
  cat("\n  A paired t-test, one-tailed (answer 5)")
  cat("\n  A paired t-test, two-tailed (answer 6)")
  counter <- getAnswer(6, 6)
  cat("\nYou got the correct answer in", counter,
      ifelse(counter == 1, "attempt. ", "attempts."))
  if (counter == 1) cat("Very good!")
  cat("\n")
  
  readline("\n\nReady for the next question? Hit return to continue.")
  
  cat("\nQuestion 3.")
  cat("\nA commerical herbal extract promises to have at least a particular",
      "\nconcentration level of compound A. Using a targeted method",
      "\nyou are testing this statement, analysing ten replicates.",
      "\nChoose the best test from the list below:")
  cat("\n  A one-sample t-test, one-tailed (answer 1)")
  cat("\n  A one-sample t-test, two-tailed (answer 2)")
  cat("\n  A two-sample t-test, one-tailed (answer 3)")
  cat("\n  A two-sample t-test, two-tailed (answer 4)")
  cat("\n  A paired t-test, one-tailed (answer 5)")
  cat("\n  A paired t-test, two-tailed (answer 6)")
  counter <- getAnswer(1, 6)
  cat("\nYou got the correct answer in", counter,
      ifelse(counter == 1, "attempt. ", "attempts."))
  if (counter == 1) cat("Very good!")
  cat("\n")
  
  readline("\n\nOne more! Hit return to continue.")
  
  cat("\nQuestion 4.")
  cat("\nYou investigate two groups of patients following different",
      "\ndiets, analyzing their urine. The question is whether diet X",
      "\nleads to higher levels of compound A.",
      "\nChoose the best test from the list below:")
  cat("\n  A one-sample t-test, one-tailed (answer 1)")
  cat("\n  A one-sample t-test, two-tailed (answer 2)")
  cat("\n  A two-sample t-test, one-tailed (answer 3)")
  cat("\n  A two-sample t-test, two-tailed (answer 4)")
  cat("\n  A paired t-test, one-tailed (answer 5)")
  cat("\n  A paired t-test, two-tailed (answer 6)")
  counter <- getAnswer(3, 6)
  cat("\nYou got the correct answer in", counter,
      ifelse(counter == 1, "attempt. ", "attempts."))
  if (counter == 1) cat("Very good!")
  cat("\n")
  
  invisible()
}

## statsQuiz()

ExPlot <- function() {
  graphics.off()
  
  ## use some visualization techniques on the wine data. Examples:
  ## boxplots, dotplots, visualize correlation matrices and distance
  ## matrices... Ask about the advantages and disadvantages, and
  ## discuss afterwards. show the effect of scaling.
  cat("\nIn this short exercise we will take a look at one data set",
      "\nof 177 Italian wines - for each wine, 13 variables have been",
      "\nmeasured. This is by all modern standards a tiny data set",
      "\nbut you will see that already it presents difficulties in",
      "\nvisualization. The effect of different forms of scaling",
      "\ncan be seen nicely.")

  require(lattice, quiet = TRUE)
  data(wines)
  wines.df <- data.frame(variety = vintages,
                         as.data.frame.table(wines,
                                             responseName = "Level")[,-1])
  names(wines.df)[2] <- "Var"
  wines.df0 <- data.frame(variety = vintages, wines = wines)

  readline("\n\nHit return to continue.")

  cat("\nMany different types of plots give a glimpse of what the",
      "\ndata look like: each plotting type has advantages and disadvantages.",
      "\nTry out the possibilities below, and discuss what bits of",
      "\ninformation come out loud and clear, and what parts are",
      "\nhidden under the carpet...")

  ans <- 5
  while (ans != 0) {
    cat("\n  Pairs plot (answer 1)")
    cat("\n  Boxplot (answer 2)")
    cat("\n  Dotplot (answer 3)")
    cat("\n  Parallel coordinates (answer 4)")
    cat("\n  Stop (answer 0)")
    ans <- readline("\n\nChoose a plot: ")

    if (ans == 1)
        print(splom(~ wines.df0[,-1], data = wines.df0,
                    pch = ".", cex = 2, groups = variety, pscales = 0))
    
    if (ans == 2)
        print(bwplot(variety ~ Level | Var, data = wines.df,
                     scale = list(x = "free")))
  
    if (ans == 3)
        print(dotplot(variety ~ Level | Var, data = wines.df,
                      groups = variety, scale = list(x = "free"),
                      jitter.y = TRUE))

    if (ans == 4)
        print(parallelplot(~ wines.df0[,-1] | variety, data = wines.df0,
                           groups = wines.df0[,1]))
  }

  cat("\nThese four examples are by no means the only possibilities for",
      "\nvisualization: but it is a start.",
      "\nWe can now investigate the effect of scaling using these plots.",
      "\nYou will be shown plots using both the original data,",
      "\nand the data scaled according to your choice.")

  readline("\n\nHit return to continue.")

  ans <- 5
  while (ans != 0) {
    graphics.off()
    
    cat("\n  Pairs plot (answer 1)")
    cat("\n  Boxplot (answer 2)")
    cat("\n  Dotplot (answer 3)")
    cat("\n  Parallel coordinates (answer 4)")
    cat("\n  Stop (answer 0)")
    ans <- readline("\n\nChoose a plot: ")

    if (ans %in% 1:4) {
      ans2 <- 5
      while (!(ans2 %in% 0:4)) {
        cat("\n  Auto-scaling (answer 1)")
        cat("\n  Mean-centering (answer 2)")
        cat("\n  Square root scaling (answer 3)")
        cat("\n  Log scaling (answer 4)")
        cat("\n  Stop (answer 0)")
        ans2 <- readline("\n\nChoose a scaling method: ")
      }
      
      ## scaling is more conveniently done on the original data
      ## matrix, so we will in the first two cases recreate the two
      ## data frames every time. Inefficient, but efficient enough.
      
      if (ans2 == 1) {
        wines.df0.sc <- data.frame(variety = vintages,
                                   wines = scale(wines))
        wines.df.sc <-
            data.frame(variety = vintages,
                       as.data.frame.table(scale(wines),
                                           responseName = "Level")[,-1])
        names(wines.df.sc)[2] <- "Var"
      }
      
      if (ans2 == 2) {
        wines.df0.sc <- data.frame(variety = vintages,
                                   wines = scale(wines, scale = FALSE))
        wines.df.sc <-
            data.frame(variety = vintages,
                       as.data.frame.table(scale(wines, scale = FALSE),
                                           responseName = "Level")[,-1])
        names(wines.df.sc)[2] <- "Var"
      }
      
      if (ans2 == 3) {
        wines.df.sc <- wines.df
        wines.df.sc$Level <- sqrt(wines.df.sc$Level)
        wines.df0.sc <- wines.df0
        wines.df0.sc[,-1] <- sqrt(wines.df0.sc[,-1])
      }
      
      if (ans2 == 4) {
        wines.df.sc <- wines.df
        wines.df.sc$Level <- log(wines.df.sc$Level)
        wines.df0.sc <- wines.df0
        wines.df0.sc[,-1] <- log(wines.df0.sc[,-1])
      }
      
      if (ans == 1) {
        print(splom(wines.df0[,-1], groups = vintages, pscales = 0,
                    pch = ".", cex = 2, main = "Original data"))
        dev.new()
        print(splom(wines.df0.sc[,-1], groups = vintages, pscales = 0,
                    pch = ".", cex = 2, main = "Scaled data"))
      }
      
      if (ans == 2) {
        print(bwplot(variety ~ Level | Var, data = wines.df,
                     scale = list(x = "free"),
                     main = "Original data"))
        dev.new()
        print(bwplot(variety ~ Level | Var, data = wines.df.sc,
                     scale = list(x = "free"),
                     main = "Scaled data"))
      }
      
      if (ans == 3) {
        print(dotplot(variety ~ Level | Var, data = wines.df,
                      groups = variety, scale = list(x = "free"),
                      jitter.y = TRUE,
                      main = "Original data"))
        dev.new()
        print(dotplot(variety ~ Level | Var, data = wines.df.sc,
                      groups = variety, scale = list(x = "free"),
                      jitter.y = TRUE,
                      main = "Scaled data"))
      }
      
      if (ans == 4) {
        print(parallelplot(~ wines.df0[,-1] | wines.df0[,1],
                           groups = wines.df0[,1],
                           main = "Original data"))
        dev.new()
        print(parallelplot(~ wines.df0.sc[,-1] | wines.df0.sc[,1],
                           groups = wines.df0.sc[,1],
                           main = "Scaled data"))
      }

      readline("\nHit return to continue...")
    }
  }

  cat("\nCan you see why some forms of scaling show up more clearly",
      "\nin some plots than in others?\n\nThat's it folks...")
  cat("\n")

  invisible()
}

## ExPlot()

ExPCA <- function(){
  graphics.off()
  
  cat("\nPCA is a projection method allowing the user to get",
      "\na global overview of a data set. Here, we are going to",
      "\nsimply apply PCA to a couple of data sets, to get a feel",
      "\nfor what is possible, and how to interpret the plots.",
      "\n\nHere is a toy data set to start with, containing 2,967",
      "\nobjects and 10 variables.")
  
  data(PCADATA)
  
  readline("\n\nHit return to show a pairs plot of the data:")
  
  pairs(PCADATA, pch = ".", cex = 2, col = colors)
  
  cat("\nCan you guess what you are looking at?")
  
  readline("\n\nHit return to look at the PCA projection in two dimensions...")
  
  X.PCA <- PCA(scale(PCADATA, scale = FALSE))
  scoreplot(X.PCA, col = colors)
  
  cat("\nFunny, eh?")
  cat("\nQuestion for you: you may (or may not) see the letters upside",
      "\ndown, mirrored from left to right, or a combination of these.",
      "\nAny idea why?")
  
  readline("\n\nHit return to go to more serious data...")
  
  cat("\nThe wine data from the previous exercise! Remember, we had",
      "\n177 wines from three different varieties, in which 13 variables",
      "\nwere measured. We will look at the score plots and loading plots.")
  
  data(wines)
  wine.classes <- as.integer(vintages)
  wines.PCA <- PCA(scale(wines))
  
  readline("\n\nHit return to see both plots...")
  
  dev.new(width = 12)
  par(mfrow = c(1,2))
  scoreplot(wines.PCA, col = wine.classes, pch = wine.classes,
            main = "Score plot")
  legend("bottomleft", legend = levels(vintages), col = 1:3, pch = 1:3)
  loadingplot(wines.PCA, show.names = TRUE,
              main = "Loading plot")
  
  cat("\nCan you see what are the two most different types of wine?",
      "\nCan you see what variables are most discriminating for these",
      "\ntwo types?")
  
  readline("\n\nHit return to continue...")
  
  cat("\nNote that in both plots the percentage of variance explained",
      "\n(the percentages at the axes) is the same: that is because",
      "\nwe are talking about the SAME subspace, defined by a particular",
      "\nlinear combination of the original variables. The linear",
      "\ncombination is shown in the loading plot (on the right), and",
      "\nthe projections of the samples in this subspace is shown",
      "\non the left.")
  ##  browser()
  data(lambrusco)
  X <- X[,apply(X, 2, sd) > 0]
  labs <- factor(sample.labels)
  
  readline("\n\nWhen you have digested this, hit return for the last data set.")
  
  cat("\nThis is an untargeted metabolomics data set of Lambrusco wines,",
      "\nmeasured on a GCxGC-MS (thanks Georg!). Total number of samples: 76,",
      "\ntotal number of variables: 1,208. The samples are divided into",
      "\nthree groups, corresponding to the origin of the wine.")
  
  lambr.PCA <- PCA(scale(X, scale = FALSE))
  readline("\n\nFirst: the usual PCA plot (mean-centered data) - hit return.")
  
  graphics.off()
  dev.new(width = 12)
  par(mfrow = c(1,2))
  scoreplot(lambr.PCA, col = as.integer(labs), pch = as.integer(labs),
            main = "Score plot")
  legend("bottomleft", legend = paste("Region", 1:3), col = 1:3, pch = 1:3)
  loadingplot(lambr.PCA, show.names = FALSE, main = "Loading plot",
              min.length = 0.005)
  
  cat("\nLooking at the plots, we can notice several things. First of all,",
      "\nthe three regions seem to be different: we can clearly distinguish",
      "\nthem in the score plot. Also, look at the scale of the score plot:",
      "\nyou would not see these values had the data been autoscaled...",
      "\nThe loading plot shows the biggest contributions as arrows, and",
      "\nthe smaller ones as open circles. Even though there are 1,210",
      "\nvariables, only eight apparently are dominating the first two PCs.",
      "\nFinally (for the moment), note that only two PCs already explain",
      "\n nearly 50% of the total variance!")
  
  cat("\n\nQuestion: why do we see so few big contributions in the",
      "\nloading plot? Discuss this.")
  
  lambrSc.PCA <- PCA(scale(X, scale = TRUE))
  lambrLog.PCA <- PCA(scale(log(X+1), scale = FALSE))
  lambrSqrt.PCA <- PCA(scale(sqrt(X), scale = FALSE))
  lambrLogSc.PCA <- PCA(scale(log(X+1), scale = TRUE))
  lambrSqrtSc.PCA <- PCA(scale(sqrt(X), scale = TRUE))
  
  readline("\n\nHit return when you think you know the answer...")
  
  cat("\nYou know how some metabolites are present in concentrations",
      "\nthat are much higher than others? Those are the ones that",
      "\ndefine the variance of the data. And since we did not scale",
      "\nthe data, apart from mean-centering, these variables will",
      "\ndominate the first two PCs.",
      "\n\nNow, try some other forms of scaling, and see if you",
      "\n'understand' the results. Are there differences in the",
      "\nability to distinguish between regions?")
  
  readline("\n\nHit return to try some other forms of scaling...")
  
  graphics.off()
  dev.new(width = 12)
  par(mfrow = c(1,2))
  ans <- 5
  while (ans != 0) {
    cat("\n  Mean-centering, again, for reference (answer 1)")
    cat("\n  Autoscaling (answer 2)")
    cat("\n  Log-scaling (answer 3)")
    cat("\n  Log-scaling plus autoscaling (answer 4)")
    cat("\n  Sqrt-scaling (answer 5)")
    cat("\n  Sqrt-scaling plus autoscaling (answer 6)")
    cat("\n  Stop (answer 0)")
    ans <- readline("\n\nChoose a plot: ")
    
    if (ans %in% 1:6) {
      show.PCA <- switch(ans,
                         "1" = lambr.PCA,
                         "2" = lambrSc.PCA,
                         "3" = lambrLog.PCA,
                         "4" = lambrLogSc.PCA,
                         "5" = lambrSqrt.PCA,
                         "6" = lambrSqrtSc.PCA)
      
      scoreplot(show.PCA, col = as.integer(labs), pch = as.integer(labs),
                main = "Score plot")
      legend("bottomleft", legend = paste("Region", 1:3), col = 1:3, pch = 1:3)
      loadingplot(show.PCA, show.names = FALSE, main = "Loading plot",
                  min.length = 0.005)
    }      
  }
  
  cat("\n\nInteresting, isn't it? Pick the one you like! And there are",
      "\nmany more scaling methods! Or should we do something else? Hmmm...")
  
  readline("\n\nHit return to finish this exercise!")
  invisible()
}

ExPLS <- function() {
  graphics.off()
  cat("\nWe have seen that PCA can be a really useful visualization method",
      "\nuseful to find outliers or see groupings, also unexpected ones...",
      "\nHowever, PCA will only show this to you if the structure is present",
      "\n_globally_, that is: if a large part of the variation in the data",
      "\nis associated with the structure you would like to see.",
      "\nIn many cases, the structure is present in only a small part of",
      "\nthe data, and PCA will _not_ be the best method to tackle",
      "\nthe data.",
      "\n\nBut: very often we know the structure, at least for a limited",
      "\nnumber of samples, and we can use this to make predictive models",
      "\nthat are more powerful that the ones based on PCA. Prime example:",
      "\nPLS. First we are going to compare the scores of the first two",
      "\nPCA scores with the first two PLS scores.")

  ## Stole the next function from (my own) kohonen package...
  classvec2classmat <- function (yvec) 
      {
        yvec <- factor(yvec)
        nclasses <- nlevels(yvec)
        outmat <- matrix(0, length(yvec), nclasses)
        dimnames(outmat) <- list(NULL, levels(yvec))
        for (i in 1:nclasses) outmat[which(as.integer(yvec) == i), 
                                     i] <- 1
        outmat
      }
  
  data(wines)
  wines.PCA <- PCA(scale(sqrt(wines), scale = FALSE))
  wines.cl <- classvec2classmat(vintages)
  wines.PLS <- plsr(wines.cl ~ sqrt(wines), ncomp = 2, validation = "LOO")

  readline("\n\nHit return to see the plot:")

  dev.new(width = 12)
  par(mfrow = c(1,2), pty = "s")
  scoreplot(wines.PCA, pch = wine.classes, col = wine.classes,
            main = "Wine data: PCA scores",
            sub = "Sqrt transformation")
  pls:::scoreplot.default(wines.PLS, pch = wine.classes, col = wine.classes,
            main = "Wine data: PLS scores",
            sub = "Sqrt transformation")
  abline(h = 0, v = 0, col = "gray", lty = 2)

  cat("\n\nIn general, the first PLS components will show a better spread",
      "\nthan the first PCA components, since class labels are taken",
      "\ninto account when defining the low-dimensional projections.",
      "\nIt is also different from PCA in that it fits a specific model",
      "\nthat can be used to make predictions.")

  classmat2classvec <- function (ymat, threshold = 0) 
      {
        class.names <- dimnames(ymat)[[2]]
        if (is.null(class.names)) 
            class.names <- 1:ncol(ymat)
        classes <- apply(ymat, 1, function(x) which(x == max(x))[1])
        classes[apply(ymat, 1, max) < threshold] <- NA
        class.names[classes]
      }

  readline("\n\nHit return to see the confusion matrix for the wine data:")

  predictions <- classmat2classvec(wines.PLS$validation$pred[,,2])
  print(table(vintages, predictions))
  
  cat("\n\nDo you remember that you chose the two wines that were",
      "\nmost different? Is that reflected in the prediction table?",
      "\nNote that these are cross-validated results, so for each",
      "\nsample the prediction has been done by a model built",
      "\n_without_ that particular sample.")

  readline("\n\nHit return to continue...")

  cat("\n\nNow, on to more difficult cases: the lambrusco data.",
      "\nHere we have a huge number of variables, and a much smaller",
      "\nnumber of samples. So-called 'fat' data. First we have to",
      "\noptimize the number of PLS components. We choose autoscaling",
      "\nas an appropriate scaling.")

  data(lambrusco)
  Y <- classvec2classmat(factor(sample.labels))
  lamb.df <- data.frame(Y = I(Y), X = I(X))
  lamb.PLS <- plsr(Y ~ X, ncomp = 10, data = lamb.df, validation = "LOO")
  lamb.pred <- apply(lamb.PLS$validation$pred, 3, classmat2classvec)
  
  readline("\n\nHit return to see the cross-validated RMSE values:")

  graphics.off()
  dev.new(width = 10, height = 5.5)
  ##  validationplot(lamb.PLS, estimate = "CV")
  barplot(apply(lamb.pred, 2, function(x) sum(x != sample.labels)),
          main = "Number of prediction errors (crossvalidated)",
          xlab = "Number of components", col = "peachpuff")

  cat("\n\nNow we face the difficult task to select the 'optimal'",
      "\nnumber. The temptation is to simply take the one with the",
      "\nfewest errors, but that may be dangerous... we will come",
      "\nback to this. The usual rule-of-thumb is to take the lowest",
      "\nnumber of components after which you do not see a big",
      "\nimprovement. Now, which one would you choose here?")

  ncomp <- 11
  while (!(ncomp %in% 1:10)) 
      ncomp <- readline("\n\nYour answer (a number between 1 and 10): ")

  readline("\n\nHit return to see the corresponding regression vector:")
  ncomp <- as.numeric(ncomp)
  
  matplot(coef(lamb.PLS, ncomp = ncomp)[,,1], type = "h", lty = 1,
          xlab = "Variable number", ylab = "Coefficient")
  legend("topright", legend = levels(vintages), lty = 1, col = 1:3)
  
  cat("\n\nAnything in particular that you notice?")

  readline("\n\nFinal application: the mystery data! Hit return...")

  RX <- matrix(rnorm(2000*40), nrow = 40)
  RY <- rep(c(0, 1), each = 20)
  R.df <- data.frame(Y = RY, X = I(RX))
  Rpls <- plsr(RY ~ RX, ncomp = 2, scale = TRUE, validation = "CV")

  cat("\n\nWe present you data from two classes, 'treated' and 'control'",
      "\nwith 2,000 variables measured for each of the 20 samples per class.",
      "\nPLS is used to make a predictive model, using 2 components.",
      "\nScaling used: only mean-centering, since the variables are",
      "\nall more or less on the same scale.")

  readline("\n\nHit return to see the results of predictions of the model:")

  RY.labelled <- c("Control", "Treatment")[RY+1]
  pred.labelled <- c("Control", "Treatment")[(predict(Rpls)[,1,2]> 0.5) + 1]

  print(table(RY.labelled, pred.labelled))

  readline("\n\nAre you happy? Hit return to hear why you shouldn't be...")
  
  cat("\n\nGood results, eh? However, these data are generated randomly.",
      "\nSo PLS has been able to 'predict' a random number generator,",
      "\nwhich of course is impossible. Rather, it has learned the",
      "\nexamples by heart... But these are the predictions of the final",
      "\nmodel. It is instructive to look at the validation predictions,",
      "\nwhere the samples-to-be-predicted have been left out of the",
      "\nmodel-building phase.")

  readline("\n\nHit return when ready...")

  val.labelled <- c("Control", "Treatment")[(Rpls$validation$pred[,1,2]> 0.5) + 1]
  print(table(RY.labelled, val.labelled))
  
  cat("\n\nIf all is well, this should be more like the 50/50 prediction",
      "\nwe would expect. Phew.")

  readline("\n\nHit return to finish this exercise.")
  invisible()
}

## ExPLS()
