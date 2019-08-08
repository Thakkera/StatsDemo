runEx <- function(example) {
  ## locate all the shiny app examples that exist
  validExamples <-
    list.files(system.file("shiny", package = "StatsDemo"))
  validExamples <- validExamples[validExamples != "test"]

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  ## if an invalid example is given, stop with feedback message
  if (missing(example) || !nzchar(example) || !example %in% validExamples) {
    message(
      'Please run `runEx()` with a valid example app as an argument.\n',
      validExamplesMsg)
  } else {
    ## find and launch the app
    appDir <- system.file("shiny", example, package = "StatsDemo")
    shiny::runApp(appDir, display.mode = "normal")
  }

  invisible()
}
