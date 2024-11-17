#' @title Run the Lyoluminescence App
#'
#' @description Controls the shiny app start
#'
#' @param browser [logical] (*with default*): enable/disable app start in browser
#'
#' @param display [character] (*with default*): set display mode of the app [shiny::runApp]
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
#'
#' @return A [shiny::shiny-package] application object.
#'
#' @family shiny
#'
#' @seealso [shiny::shinyAppDir]
#'
#' @author Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)
#'
#' @md
#' @export
run_app <- function(browser = TRUE, display = "auto") {
  shiny::shinyAppDir(
    appDir = system.file("shiny", package = "LyoluminescenceApp", mustWork = TRUE),
    options = list(launch.browser = browser, display.mode = display)
  )
}
