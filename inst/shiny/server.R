shiny_server <- function(input, output, session) {
  LyoluminescenceApp::server_module_measurement("measurement")
  LyoluminescenceApp::server_module_static("static")
  session$onSessionEnded(shiny::stopApp)
}
