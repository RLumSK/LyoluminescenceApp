shinyServer(function(input, output) {
    ## define a few variables
    con_status <- reactiveValues(
      con = "unknown",
      mode = "unkown")
    
       ## set data file and preset file
       data_file <- tempfile(fileext = ".csv")  
       write.table(data.frame(NA), file = data_file, row.names = FALSE, append = FALSE)
    
       ## set data reader, every 1 s we extract data
       data_reader <- reactiveFileReader(
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
       onStop(function(){
         if (file.exists(status_file))
           unlink(status_file)
         
         if (file.exists(data_file))
           unlink(data_file)
         
         stopApp()
       })
    
  ## initialise the connection
  observeEvent(input$init_start,{
    con <<- try({init_con(input$port)})
      if (!inherits(con, "try-error")) {
        con_status$con <- con$usr_status
        con_status$mode <- con$usr_mode
        showNotification(paste0("Connecting to ", con$port))
        
      } else {
        showNotification(con, type = "error")
        
      }
  })
  
  ## stop connection
  observeEvent(input$init_stop,{
    con <<- try({ close(con) }, silent = TRUE)
    if (!inherits(con, "try-error")) {
      con_status$con <- "disconnected"
      con_status$mode <- "unknown"
      showNotification(paste0("Disconnecting connection"))
      
    } else {
      showNotification("Nothing to disconnect!", type = "error")
      
    }
  })
  
  ## start reading input ---------
  observeEvent(input$start,{
      ## conditions
      ## set modes
      con_status$mode <- "reading"
      set_status("reading")
      
      ## close connection (we have to reopen it in the promise)
      close(con)
      
      ## set port from input
      port <- input$port
      
      ## set P
      PMT_P <- input$PMT_P
      
      ## set readout frequency
      if (PMT_P <= 10) {
        freq <- 10
      } else if (PMT_P > 10 & PMT_P <= 50) {
        freq <- 5
        
      } else {
        freq <- 1
        
      }
      
      future_promise({
        ## init connections and start reading
        con <- init_con(port)
        Sys.sleep(1)
        
        ## flash file
        write.table(data.frame(NA), file = data_file, row.names = FALSE, append = FALSE)
        
        ## check status of reading, to make sure the PMT is not in continuous reading mode
        ## if we do not get VA we have to stop the mode first
        if (send_cmd(con = con, cmd = "C")$resp != "VA")
          send_cmd(con = con, cmd = "C")
        
        send_cmd(con = con, cmd = "D")
        send_cmd(con = con, cmd = paste0("P", PMT_P))
        send_cmd(con = con, cmd = "C")
        
        while (TRUE) {
          if (interrupted()) {
            send_cmd(con = con, cmd = "C")
            Sys.sleep(1)
            close(con)
            break()
          }
          m <- read_count_data(con = con, freq = freq)
          write.table(
            x = m,
            file = data_file,
            append = TRUE,
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE,
            sep = ",")
        }
      }) %>%
        then(
          onFulfilled = function(value) {
          message("[SUCCESS] Reading stopped manually!")
          value
        },
        onRejected = function(err) {
          warning("An error occurred: ", err)
          con$translation <- "binary"
          serial::read.serialConnection(con)
          flush(con)
          close(con)
          NULL
        })
    NULL
  })
  
  ## stop reading input -------
  observeEvent(input$stop,{
      fire_interrupt()
      con_status$mode <- "idle"
      
  })
  
  ## show real count interval
  output$meas_interval <- renderText({
    paste0("[counts/", input$PMT_P/100, " s]")
  })
  
  ## render status
  output$con_status <- renderText({
    paste0("Status: ", con_status$con, " | Mode: ", con_status$mode)
  })
  
  ## observe plot changes
  observe({
    ## read data
    df <- data_reader()
    
    ## check for overexposure
    if (nrow(df) > 1 && any(is.na(df[-1, 1]))) 
      output$overexposure <- renderText("PMT OVEREXPOSURE!")
    else
      output$overexposure <- renderText("")
    
    ## render plot
    output$plot <- renderPlot({
      if (nrow(df) > 1)
        plot(
          x = seq(input$PMT_P, (nrow(df) - 1) * input$PMT_P, input$PMT_P)/100, 
          y = df[[1]][-1], 
          type = "l", 
          xlab = "Time [s]",
          ylab = paste0("Counts [1/", input$PMT_P/100, " s]"))
 

    })
  })

  # Static pages --------------------------------------------------------------------------------
  output$news <- renderUI({
    HTML(
      markdown::markdownToHTML(
        knit('static/news.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE, title = "news"))
  })

  output$about <- renderUI({
    HTML(
      markdown::markdownToHTML(
        knit('static/about.Rmd', quiet = TRUE, output = tempfile()), fragment.only = TRUE, title = "about"))
  })

  # Download handler ---------------------------------------------------------
  output$download_data <- downloadHandler(
    filename =  "PMT_Data.csv",
    contentType = "text/csv",
    content = function(file) {
      tmp_file <- tempfile(fileext = ".csv")
      tmp <- read.csv(data_file)[-1,]
      tmp <- data.frame(
        time = seq(input$PMT_P, length(tmp) * input$PMT_P, input$PMT_P)/100, 
        counts = tmp)
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
  
  
}) # end shinyServer
