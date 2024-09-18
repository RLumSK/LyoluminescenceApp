#'@title Server module static pages
#'
#'@param id module id
#'
#'@param user_settings
#'
#'@family modules
#'
#'@keywords internal
#'
#'@md
#'@export
server_module_static <- function(id, user_settings) {
  shiny::moduleServer(id, function(input, output, session) {

  # Static pages --------------------------------------------------------------------------------
  output$news <- shiny::renderUI({
    shiny::HTML(
      markdown::markdownToHTML(
        knitr::knit(
          input = system.file('shiny/static/news.Rmd', package = "LyoluminescenceApp"),
          quiet = FALSE,
          output = tempfile()),
         fragment.only = TRUE,
        title = "news"))
  })

  # output$about_static <- shiny::renderUI({
  #   shiny::HTML(
  #     markdown::markdownToHTML(
  #       knitr::knit(
  #         input = system.file('shiny/static/about.Rmd', package = "LyoluminescenceApp"),
  #         quiet = TRUE,
  #         output = tempfile()),
  #        fragment.only = TRUE,
  #       title = "about"))
  # })


 })
}
