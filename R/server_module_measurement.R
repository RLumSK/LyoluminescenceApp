#'@title Server module
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
server_module_measurement <- function(id, user_settings) {
  ## set multisession
  future::plan(future::multisession)

  shiny::moduleServer(id, function(input, output, session) {
    ## define plot environment
    initial_plot <- plotly::plot_ly(
      x = 0,
      y = 0,
      type = 'scatter',
      mode = 'lines+markers',
      height = 600
    ) |>
    plotly::layout(
      autosize = TRUE,
      yaxis = list(title = "Luminescence [a.u.]"),
      xaxis = list(title = "Time [s]"))

    output$linePlot <- plotly::renderPlotly({
      initial_plot
    })

    lineProxy <- plotly::plotlyProxy("linePlot", session)

    x_plot_data <- shiny::reactiveVal(0)
    y_plot_data <- shiny::reactiveVal(0)

    ## define a few variables
    con_status <- shiny::reactiveValues(
      con = "unknown",
      mode = "unknown")

       ## set data file and preset file
       data_file <- tempfile(fileext = ".csv")
       write.table(data.frame(NA), file = data_file, row.names = FALSE, append = FALSE)

       ## set data reader, every 1 s we extract data
       data_reader <- shiny::reactiveFileReader(
         intervalMillis = 100,
         filePath = data_file,
         session = NULL,
         readFunc = read.csv)

       # set status File
       status_file <- tempfile()

       get_status <- function() {
         scan(status_file, what = "character", sep = "\n")
       }

       set_status <- function(msg){
         write(msg, status_file, append = FALSE)
       }

       fire_interrupt <- function(){
         set_status("interrupt")
       }

       interrupted <- function(){
         get_status() == "interrupt"
       }

       # Delete file at end of session
       shiny::onStop(function(){
         if (file.exists(status_file))
           unlink(status_file)

         if (file.exists(data_file))
           unlink(data_file)

         shiny::stopApp()
       })

  ## update input selection for port list
  shiny::observeEvent(input$port_refresh, {
    x <- list_ports()

     shiny::updateSelectInput(
       inputId = "port",
       choices = x)
  })

  ## initialise the connection
  shiny::observeEvent(input$init_start,{
    con <<- try({init_con(input$port)})
      if (!inherits(con, "try-error")) {
        con_status$con <- con$usr_status
        con_status$mode <- con$usr_mode
        shiny::showNotification(paste0("Connecting to ", con$port))

      } else {
        shiny::showNotification(con, type = "error")

      }
  })

  ## stop connection
  shiny::observeEvent(input$init_stop,{
    con <<- try({ close(con) }, silent = TRUE)
    if (!inherits(con, "try-error")) {
      con_status$con <- "disconnected"
      con_status$mode <- "unknown"
      shiny::showNotification(paste0("Disconnecting connection"))

    } else {
      shiny::showNotification("Nothing to disconnect!", type = "error")

    }
  })

  ## start reading input ---------
  shiny::observeEvent(input$start,{
      ## conditions
      ## set modes
      con_status$mode <- "reading"
      set_status("reading")

      ## close connection (we have to reopen it in the promise)
      if(exists("con"))
        close(con)

      ## set port from input
      port <- input$port

      ## set P
      PMT_P <- input$PMT_P

      ## set preset duration
      if(is.null(input$PMT_nCH) || is.na(input$PMT_nCH) || input$PMT_nCH == 0)
        PMT_nCH <- Inf
      else
        PMT_nCH <- input$PMT_nCH

      ## set counter
      cnt <- 1

      ## flash file
      write.table(
        x = data.frame(NA),
        file = data_file,
        row.names = FALSE,
        append = FALSE)

      ## flash plot
      x_plot_data(0)
      y_plot_data(NULL)

      output$linePlot <- plotly::renderPlotly({
        initial_plot
      })

      ## decide whether to simulate or to measure
      ## this is useful for testing the functionality
      ## of the app without using the PMT connection
      if (input$meas_mode == "Simulation") {
        ### simulation --------
        promises::future_promise(seed = TRUE, {

        ## set simulation interval
        sim_pause <- PMT_P/100

        ## readout loop
        while (cnt <= PMT_nCH) {
          if (interrupted())
            break()

            m <- stats::runif(1)
            write.table(
              x = m,
              file = data_file,
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE,
              sep = ",")

            ##simulate delay
            Sys.sleep(sim_pause)

            ## update counter
            cnt <- cnt + 1
          }
       }) |>
          promises::then(
            onFulfilled = function(value) {
              message("[SUCCESS] Simulation stopped manually!")
              con_status$mode <- "idle"

            },
            onRejected = function(err) {
              warning("An error occurred: ", err)
              con_status$mode <- "error"
              NULL
            })

      } else {
      ### measurement -------
      promises::future_promise({
        ## init connections and start reading
        con <- init_con(port)
        Sys.sleep(0.25)

        ## make sure the PMT is not in continuous reading mode
        send_cmd(con = con, cmd = "\n")

        send_cmd(con = con, cmd = "D")
        send_cmd(con = con, cmd = paste0("P", PMT_P))
        send_cmd(con = con, cmd = "C")

        while (cnt <= PMT_nCH) {
          if (interrupted()) {
            send_cmd(con = con, cmd = "\n")
            Sys.sleep(0.1)
            break()
          }

          m <- read_count_data(con = con)

          if (cnt > 2) {
            write.table(
              x = m,
              file = data_file,
              append = TRUE,
              quote = FALSE,
              col.names = FALSE,
              row.names = FALSE,
              sep = ",")

          }

          ## update counter
          cnt <- cnt + 1
        }
      }) |>
        promises::then(
          onFulfilled = function(value) {
          message("[SUCCESS] Reading stopped manually!")

          con_status$mode <- "idle"

          if (serial::isOpen(con)) {
            flush(con)
            close(con)

          }
          value
        },
        onRejected = function(err) {
          warning("An error occurred: ", err)
          con_status$mode <- "idle"
          con$translation <- "binary"
          serial::read.serialConnection(con)
          flush(con)
          close(con)
          NULL
        })
      }
    NULL
  })

  ## stop reading input -------
  shiny::observeEvent(input$stop,{
      fire_interrupt()

  })

  ## show real count interval
  output$meas_interval <- shiny::renderText({
    paste0("[counts/", input$PMT_P/100, " s]")
  })

  ## show measurement duration
  output$meas_duration <- shiny::renderText({
    if(is.null(input$PMT_nCH) || is.na(input$PMT_nCH) || input$PMT_nCH == 0)
      "meas. duration: no restriction"
    else
      paste0("meas. duration: ", input$PMT_nCH * input$PMT_P/100, " s")

  })

  ## render status
  output$con_status <- shiny::renderText({
    paste0("Status: ", con_status$con, " | Mode: ", con_status$mode)
  })

  ## Plotting ------------
  shiny::observe({
    ## read data
    df <- data_reader()

    ## check for overexposure
    if (nrow(df) > 1 && any(is.na(df[-1, 1])))
      output$overexposure <- shiny::renderText("PMT OVEREXPOSURE!")
    else
      output$overexposure <- shiny::renderText(NULL)

    ### create plotly ------
    if (con_status$mode == "reading" & nrow(df) > 1) {
      x_plot_data((nrow(df) - 1) * input$PMT_P/100)
      y_plot_data(df[[1]][length(df[[1]])])

      ##update yaxis
      plotly::plotlyProxyInvoke(
        p = lineProxy,
        method = "relayout",
        yaxis = list(
          title = paste0("Luminescence [cts/", input$PMT_P/100, " s]")

        ))

      ## extend traces
      plotly::plotlyProxyInvoke(
        p = lineProxy,
        method = "extendTraces",
        list(
          x = list(list(x_plot_data())),
          y = list(list(y_plot_data()))),
        list(0))
    }

  })

  ### update plot --------
  shiny::observeEvent(eventExpr = input$plot_update, {
  plotly::plotlyProxyInvoke(
    p = lineProxy,
    method = "update",
    list(
      mode = input$plot_marker_style
    ),
    list(
      title = input$plot_title,
      xaxis = list(
        type = if(input$x_log) "log" else "lin",
        title = "Time [s]"),
      yaxis = list(
        type = if(input$y_log) "log" else "lin",
        title = paste0("Luminescence [cts/", input$PMT_P/100, " s]"))))
  })

  # Static pages --------------------------------------------------------------------------------
  output$news <- shiny::renderUI({
    shiny::HTML(
      markdown::markdownToHTML(
        knitr::knit(
          input = system.file('shiny/static/news.Rmd', package = "LyoluminescenceApp"),
          quiet = TRUE,
          output = tempfile()),
         fragment.only = TRUE,
        title = "news"))
  })

  output$about <- shiny::renderUI({
    print(system.file('shiny/static/about.Rmd', package = "LyoluminescenceApp"))
    shiny::HTML(
      markdown::markdownToHTML(
        knitr::knit(
          input = system.file('shiny/static/about.Rmd', package = "LyoluminescenceApp"),
          quiet = FALSE,
          output = tempfile()),
         fragment.only = TRUE,
        title = "about"))
  })

  # Download handler ---------------------------------------------------------
  output$download_data <- shiny::downloadHandler(
    filename =  "PMT_Data.csv",
    contentType = "text/csv",
    content = function(file) {
      tmp_file <- tempfile(fileext = ".csv")
      tmp <- read.csv(data_file)[-1,]
      tmp <- data.frame(
        time = seq(input$PMT_P, length(tmp) * input$PMT_P, input$PMT_P)/100,
        counts = tmp)

      ## replace NA with no values
      if (input$rm_NA)
        tmp[is.na(tmp[[2]]),2] <- ""

      write.table(
        x = tmp,
        file = tmp_file,
        append = FALSE,
        quote = FALSE,
        sep = ",",
        row.names = FALSE)
      file.copy(tmp_file, file, overwrite = TRUE)
    }
  )
 })
}
