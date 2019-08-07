require(shiny)
require(shinydashboard)
require(StatsDemo)

## function getQuestions returns a list with question objects
## consisting of questions, choices, replies, and TRUE/FALSE labels.
getQuestions <- function() {
  list(
    ## Q1
    list(name = "Q1",
         question =
           "You want to determine whether the resveratrol level in Fuji apples is the same as in Golden Delicious apples. You analyse ten apples from each variety. What is the best test in the list below?",
         choices = list(
           "A one-sample t-test, one-tailed",
           "A one-sample t-test, two-tailed",
           "A two-sample t-test, one-tailed",
           "A two-sample t-test, two-tailed",
           "A paired t-test, one-tailed",
           "A paired t-test, two-tailed"),
         labels = c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)),
    ## Q2
    list(name = "Q2",
         question =
           "You are comparing quantification of a particular metabolite in targeted and untargeted methods. You take ten apples and analyse them using both approaches. What is the best test in the list below?",
         choices = list(
           "A one-sample t-test, one-tailed",
           "A one-sample t-test, two-tailed",
           "A two-sample t-test, one-tailed",
           "A two-sample t-test, two-tailed",
           "A paired t-test, one-tailed",
           "A paired t-test, two-tailed"),
         labels = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE)),
    ## Q3
    list(name = "Q3",
         question =
           "A commerical herbal extract promises to have at least a particular concentration level of a certain compound. Using a targeted method you are testing this statement, analysing ten biological replicates. What is the best test in the list below?",
         choices = list(
           "A one-sample t-test, one-tailed",
           "A one-sample t-test, two-tailed",
           "A two-sample t-test, one-tailed",
           "A two-sample t-test, two-tailed",
           "A paired t-test, one-tailed",
           "A paired t-test, two-tailed"),
         labels = c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)),
    ## Q4, for now the same as Q1
    list(name = "Q4",
         question =
           "You investigate two groups of patients following different diets, analyzing their urine. The question is whether diet X leads to higher levels of compound A. What is the best test in the list below?",
         choices = list(
           "A one-sample t-test, one-tailed",
           "A one-sample t-test, two-tailed",
           "A two-sample t-test, one-tailed",
           "A two-sample t-test, two-tailed",
           "A paired t-test, one-tailed",
           "A paired t-test, two-tailed"),
         labels = c(FALSE, FALSE, TRUE, FALSE, FALSE, FALSE))
    )
}

getQuestion <- function(qnumber) {
  allQuestions <- getQuestions()
  nquestions <- length(allQuestions)
  if (qnumber > nquestions)
    stop("Unknown question")
  
  allQuestions[[qnumber]]
}

qIntro <-
  tabItem(tabName = "qIntro",
          h2("Introduction"),
          box(p("Just a couple (four, to be precise) of easy-peasy questions on applying t tests... Basically you will have to decide whether a paired, a one-sample or a two-sample test is appropriate, and whether the test should be one-sided or two-sided. The goal is not so much to get it right, but to discuss the pros and cons of each answer with your neighbour. So think first, and then make your choice.")),
          box(p("First to finish the exercise: no prizes!")))

## shiny stuff only from here
sidebar <- dashboardSidebar(
  helpText("The t-test Quiz!"),
  sidebarMenu(
    menuItem("Introduction", tabName = "qIntro"),
    menuItem("Q1", tabName = "Q1"),
    menuItem("Q2", tabName = "Q2"),
    menuItem("Q3", tabName = "Q3"),
    menuItem("Q4", tabName = "Q4"))
)

Q2Tab <- function(question) {
  tabItem(tabName = question$name,
          h2(question$name),
          box(question$question),
          radioButtons(paste("Choice", question$name, sep = ""),
                       label = NULL,
                       selected = character(0),
                       choiceNames = question$choices,
                       choiceValues = ifelse(question$labels,
                                             "Correct! You may proceed...",
                                             "Incorrect! Try again...")),
#          actionButton(paste("Check", question$name, sep = ""), "Go!"),
          textOutput(outputId = paste("Feedback", question$name, sep = "")))
}

server <- function(input, output) {
#  observeEvent("CheckQ1", {
    output$FeedbackQ1 <- 
      renderText({
        input$ChoiceQ1
      })
##  })
##  observeEvent("CheckQ2", {
    output$FeedbackQ2 <- 
      renderText({
        input$ChoiceQ2
      })
##  })
##  observeEvent("CheckQ3", {
    output$FeedbackQ3 <- 
      renderText({
        input$ChoiceQ3
      })
##  })
##  observeEvent("CheckQ4", {
    output$FeedbackQ4 <- 
      renderText({
        input$ChoiceQ4
      })
##  })
}

body <- dashboardBody(
  tabItems(qIntro,
           Q2Tab(getQuestion(1)),
           Q2Tab(getQuestion(2)),
           Q2Tab(getQuestion(3)),
           Q2Tab(getQuestion(4)))
)

ui <- dashboardPage(
  dashboardHeader(title = "The t-test Quiz!"),
  sidebar,
  body
)

shinyApp(ui = ui, server = server)
