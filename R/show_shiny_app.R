#' Launch Shiny Dashboard for Mortality Data
#'
#' This function launches a Shiny dashboard that displays the bar plot of
#' mortality data. Users can select the causes of death to display and toggle
#' the vertical line marking Nightingale's improvements.
#'
#' @details
#' The dashboard provides an interactive interface for visualizing mortality
#' data from the Crimean War. Users can:
#' - Select specific causes of death to display in the bar plot.
#' - Toggle the visibility of a vertical line that marks the implementation
#'   of Nightingale's sanitary improvements in February 1855.
#'
#' The bar plot is generated using the `show_barplot()` function, and the
#' data is dynamically filtered based on user input.
#'
#' @param mortality_data A data frame containing mortality data with columns
#'   for date and various causes of death. Defaults to the `mortality` dataset.
#'
#' @seealso
#' \code{\link{show_barplot}} for the function that generates the bar plot.
#'
#' @examples
#' if (interactive()) {
#'   show_shiny_app()
#' }
#'
#' @export
show_shiny_app <- function(mortality_data = mortality) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Mortality Data Dashboard"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "selected_causes",
          "Select Causes of Death:",
          choices = colnames(mortality)[-1],
          # Exclude the 'date' column
          selected = colnames(mortality)[-1],
          size = 4,
          selectize = FALSE,
          multiple = TRUE
        ),
        shiny::checkboxInput(
          "show_vline",
          "Show date of Nightingale's Improvements",
          value = TRUE
        )
      ),
      shiny::mainPanel(shiny::plotOutput("barplot"))
    )
  )

  server <- function(input, output, session) {
    output$barplot <- shiny::renderPlot({
      show_barplot(
        mortality_data = mortality,
        metrics = input$selected_causes,
        highlight_intervention = input$show_vline
      )
    })
  }

  shiny::shinyApp(ui, server)
}
