shinyApp(
  ui = dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(
        id = "sidebar_menu",
        menuItemOutput("dynamic_menu"),
        menuItem("Menu2", tabName = "menu2")
      )
    ),
    dashboardBody(
      uiOutput("menu1_content"),
      uiOutput("menu2_content")
    ),
    title = "Example"
  ),
  server = function(input, output, session) {
    output$dynamic_menu <- renderMenu({
      menu_list <- lapply(letters[1:5], function(x) {
        menuSubItem(x, tabName = paste0("menu1-", x))
      })
      menuItem(
        text = "Menu1",
        startExpanded = TRUE,
        do.call(tagList, menu_list)
      )
    })

    output$menu1_content <- renderUI({
      sidebar_menu <- tstrsplit(input$sidebar_menu, "-")
      if (sidebar_menu[[1]] == "menu1") box(sidebar_menu[[2]])
    })

    output$menu2_content <- renderUI({
      sidebar_menu <- tstrsplit(input$sidebar_menu, "-")
      if (sidebar_menu[[1]] == "menu2") box("I am menu2")
    })
  }
)

