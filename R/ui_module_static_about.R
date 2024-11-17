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
ui_module_static_about <- function(id) {
  # Create a namespace function using the provided id
  ns <- shiny::NS(id)

  # PANEL - About ------------------------------------------------------------------------------
  shiny::tabPanel(
    title = "About",
    shiny::fluidRow(
      shiny::column(
        width = 10,
        offset = 1,
        shiny::uiOutput(outputId = ns("about")),
        shiny::includeMarkdown("static/about.md"))),
      icon = shiny::icon("info-sign", lib = "glyphicon")
  )# end about
}
