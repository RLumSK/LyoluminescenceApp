#'@title Send Commands to the Hamamatsu PMT
#'
#'@description Send commands to the PMT and formats the input following the documentation
#'and takes care of the particularities of this PMT.
#'
#'@param con [serial::serialConnection] (**required**): a `serialConnection`
#'object returned by either [init_con] or [serial::serialConnection]
#'
#'@param cmd [character] (**required**): command to send to the PMT following the documentation
#'
#'@returns [list] object with the connection and the response from the PMT
#'
#'@author Sebastian Kreutzer,  Institute of Geography, Heidelberg University (DE)
#'
#'@examples
#' #TODO
#'
#'@keywords internal
#'
#'@md
#'@export
send_cmd <- function(con, cmd) {
  ## treat characters ---------------
  cmd <- toupper(cmd)

  ## extract characters and numbers
  cmd_char <- regmatches(x = cmd, regexec("[A-Z]", text = cmd))[[1]]
  cmd_num <- regmatches(x = cmd, regexec("[0-9]+", text = cmd))[[1]]

  ## the newline character (what Hamamatsu calls carriage return) is used
  ## to stop a continous reading
  CR <- "\n"
  if (length(cmd_char) == 0 && cmd == CR)
    cmd_char <- CR

  ## check allowed commands
  if (!cmd_char %in% c("P", "R", "D", "S", "C", CR)) {
    stop("[send_cmd()] Unsupported command! Supported are:
         P\t->\t set the integration interval in 10 ms increments (1 to 100)
         R\t->\t set sequence of readings (1 to 255)
         D\t->\t reset HV to default
         S\t->\t start reading sequence
         C\t->\t start continuous reading
         ", deparse(CR), "\t->\t stop continuous reading
         ", call. = FALSE)
  }

  ## the set/reset and stop commands produce a 2-bytes response, but the
  ## start commands receive a 4-byte stream directly
  expects_response <- cmd_char %in% c("P", "R", "D", CR)

  cmd_report <- cmd_char

  ## translate numbers into ASCII characters
  if (length(cmd_num) > 0) {
    ## TODO ... do better split with > 255
    num_raw <- as.raw(cmd_num)
    cmd_char <- paste0(cmd_char, rawToChar(num_raw, multiple = TRUE))
    cmd_report<- paste0(cmd_report, as.character(num_raw), " (", cmd_num, ")")
  }

  ## construct call
  cmd <- cmd_char

  ## write commands ------------------
  ## flush connection
  con$translation <- "binary"
#  flush(con)

  ## set connection to ASCII mode
  con$translation <- "cr"
#  flush(con)

  ## write command
  serial::write.serialConnection(con, cmd)
#  flush(con)

  ## set connection to binary mode
  con$translation <- "binary"

  ## read response ... we let the system wait until we have something
  resp <- NA
  if (expects_response) {
    while(serial::nBytesInQueue(con)[1] < 2) {
      Sys.sleep(0.01)
    }

    ## read response
    resp <- serial::read.serialConnection(con, n = 2) |>
      readBin(what = "character", size = 2, n = 1)
#    flush(con)

    ## chose how to report the response
    report <- cli::cli_alert_danger
    if (resp == "VA" || (cmd == CR && resp == "")) {
      report <- cli::cli_alert_success
    }

    ## make sure that linebreaks are shown
    cmd <- gsub('\n', '\\n', cmd, fixed = TRUE)
    cmd_report <- gsub('\n', '\\n', cmd_report, fixed = TRUE)
    report(paste0("sent: <", cmd_report, "> | received: '", resp, "'"),
           wrap = TRUE)
  }

  ## set mode
  con$mode <- cmd

  return(list(con = con, resp = resp))
}

## copied from serial:::flush.serialConnection, as that function is not
## exported
flush <- function(con) {
  if (!serial::isOpen(con))
    stop(simpleError(paste(con$port, "is not open!")))
  tryCatch({
    tcltk::.Tcl(paste("flush ${sdev_", con$var, "}", sep = ""))
  }, error = function(e) stop(simpleError(e$message)))
  invisible("DONE")
}

## copied from serial:::close.serialConnection, as that function is not
## exported
close <- function (con, ...) {
  if (serial::isOpen(con)) {
    tryCatch({
       tcltk::.Tcl(paste("flush ${sdev_", con$var, "}", sep = ""))
       tcltk::.Tcl(paste("close ${sdev_", con$var, "}", sep = ""))
    }, error = function(e) stop(simpleError(e$message)))
     tcltk::.Tcl(paste("unset sdev_", con$var, sep = ""))
  }
  invisible("DONE")
}
