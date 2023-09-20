## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Title:   Lyoluminescence PMT App 
## Author:  Sebastian Kreutzer, Institute of Geography, Ruprecht-Karl-University of Heidelberg (DE)
## Contact: sebastian.kreutzer@uni-heidelberg.de
## Date:    Wed Aug  9 10:39:24 2023
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# State required R packages -----------------------------------------------
require(shiny)
require(shinyjs)
require(serial)
require(markdown)
require(knitr)
require(promises)
require(future)
plan(multisession)

# Define helper functions -------------------------------------------------
#' Initiate the connection
#' 
#' @export
init_con <- function(port) {
  con <- serial::serialConnection(
    name = "Lyoluminescence_ASCII",
    port = port,
    mode = "9600,n,8,1",
    newline = 1,
    translation = "cr",
    handshake = "none") 
  
  ## check text
  text_ <- paste0("Connecting to ", con$port)
  
  ## only open if not opened 
  if (!serial::isOpen(con)) {
    open(con)
    
    ## write response 
    if (serial::isOpen(con)) 
      cli::cli_alert_success(text_)
    else
      cli::cli_alert_danger(text_)
    
    ## communicate with connection ...otherwise it will not work
    ## the "D" (default PMT), appears to be the safest character
    write.serialConnection(con, "D")
    read.serialConnection(con)
    Sys.sleep(1)
    
  } else {
    cli::cli_alert_info(paste0(text_, " ... already connected!"))
    
  }
  
  ## set mode
  con$usr_status <- "connected"
  con$usr_mode <- "initialised"
  
  return(con)
}


#' Define function for general command sending and return value reading
#' 
#' @export
send_cmd <- function(con, cmd) {
  ## treat characters ---------------
  cmd <- toupper(cmd)
  
  ## extract characters and numbers
  cmd_char <- regmatches(x = cmd, regexec("[A-Z]", text = cmd))[[1]]
  cmd_num <- regmatches(x = cmd, regexec("[0-9]+", text = cmd))[[1]]
  
  ## check allowed commands
  if (!cmd_char %in% c("P", "R", "D", "S", "C")) {
    stop("[send_cmd()] Unsupported command! Supported are: 
         P\t->\t set the integration interval in 10 ms increments (1 to 100)
         R\t->\t set sequence of readings (1 to 255)
         D\t->\t reset HV to default
         S\t->\t start reading sequence
         C\t->\t start/stop continous reading 
         ", call. = FALSE)
  }
  
  ## translate numbers into ASCII characters
  if (length(cmd_num) > 0) {
    ## TODO ... do better split with > 127
    cmd_num <- rawToChar(as.raw(cmd_num), multiple = TRUE)
    cmd_char <- paste0(cmd_char, cmd_num)
  }
  
  ## construct call
  cmd <- cmd_char
  
  ## write commands ------------------
    ## flush connection
    con$translation <- "binary"
    serial::read.serialConnection(con)
    
    ## set connection to ASCII mode
    con$translation <- "cr"
  
  ## write command
  serial::write.serialConnection(con, cmd)
  
  ## set connection to binary mode
  con$translation <- "binary"
  
  ## read response ... we let the system wait until we have something
  while(serial::nBytesInQueue(con)[1] < 2) 
    Sys.sleep(0.01)
  
  ## read response
  resp <- serial::read.serialConnection(con, n = 2) |> 
    readBin(what = "character", size = 2, n = 1) 
  
  ## make sure that linebreaks are shown cmd
  cmd <- gsub('\n', '\\n', cmd, fixed = TRUE)
  
  if (resp == "VA" || (cmd == "C" && resp == "")) {
    cli::cli_alert_success(paste0("sent: <", as.character(cmd), "> | received: ", resp), wrap = TRUE)
    
  } else {
    cli::cli_alert_danger(paste0("sent: <", as.character(cmd), "> | received: ", resp), wrap = TRUE)
    
  }
  
  ## set mode
  con$mode <- cmd
  
  return(list(con = con, resp = resp))
}

#'
#'
#'@export
read_count_data <- function(con, freq = 10) {
  serial::nBytesInQueue(con)
  con$translation <- "binary"
  
  ## read buffer (and throw it away)
  serial::read.serialConnection(con)
  
  ## set list
  m <- list()
  
  # read stuff
  cnt <- 1
  while (serial::nBytesInQueue(con)[1] <= 2048 && cnt <= freq) {
    in_queue <- serial::nBytesInQueue(con)[1]
    if (in_queue >= 4) {
      bytes <- serial::read.serialConnection(con = con, n = in_queue - (in_queue %% 4))
      n_points <- length(bytes) / 4
      
      ## write into vector
      ## with the condition we make sure that nothing odd happens
      if(length(bytes) >= 4) {
        ## read information
        tmp <- readBin(
          con = bytes,
          what = "integer",
          size = 4,
          n = n_points,
          endian = "big",
          signed = TRUE)
        
        ## make sure that we catch overexposure 
        ## and set it NA
        tmp[tmp < 0] <- NA_integer_
        
        ## write to matrix
        m[cnt:(cnt - 1 + n_points)] <- tmp
      }
      
      ## update counter
      cnt <- cnt + n_points
    }
  }
  
  ## remove NA
  return(unlist(m))
}


