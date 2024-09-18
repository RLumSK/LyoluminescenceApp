#' @title About UI
#'
#' @param id [character] (**required**) vector to be used for the namespace.
#'
#' @family modules
#'
#' @keywords internal
#'
#' @md
#' @export
ui_module_static_news <- function(id) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  # PANEL - News ------------------------------------------------------------------------------
  shiny::tabPanel(
    title = "News",
    shiny::fluidRow(
      shiny::column(
        width = 10,
        offset = 1,
       shiny::uiOutput(outputId = ns('news')))),
    icon = shiny::icon("list-alt", lib = "glyphicon")
  )# end news

}
