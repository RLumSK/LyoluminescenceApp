shiny_ui <- function(request) {
  shiny::navbarPage(
    title = shiny::HTML(paste0("Lyoluminescence App")),
    windowTitle = "Lyoluminescence App",
    LyoluminescenceApp::ui_module_measurement("measurement"),
    LyoluminescenceApp::ui_module_static_news("news"),
    LyoluminescenceApp::ui_module_static_about("static_about"),
    footer = shiny::HTML(
      "<hr>
            <div align = 'center'><small>This software comes WITHOUT ANY WARRANTY.</small>
            </div>")
  )
}

