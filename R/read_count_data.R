#'@title Reads PMT count data
#'
#'@description Reads and processes the PMT count data
#'
#'@param con [serial::serialConnection] (**required**): a `serialConnection` object
#'returned by either [init_con] or [serial::serialConnection]
#'
#'@param freq [integer] (*with default*): readout frequency to control the cycles
#'of the readout
#'
#'@returns [numeric] vector with count data
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
read_count_data <- function(con, freq = 1) {
  ## reformat connection for connection string
  con_str <- gsub(".", "_", x = con$port, fixed = TRUE) |>
    gsub("-", "_", x = _, fixed = TRUE)

  ## set buffer
  buffer <- logical()

  ## set count
  cnt <- 1

  ## set results
  res <- numeric()

  con$translation <- "binary"

  # read stuff
  while (length(res) < freq) {

    ## we wait until we have at least 8 bytes in the queue before reading
    ## (in theory for us 4 should suffice, but 8 seems to be more reliable)
    ## see https://majenko.co.uk/blog/reading-serial-arduino
    queue.bytes <- 8
    queue <- serial::nBytesInQueue(con)[1]
    while (queue < queue.bytes) {
      Sys.sleep(0.005)
      queue <- serial::nBytesInQueue(con)[1]
    }

    ## read bits
    com_str <- paste0('binary scan [read ${sdev_', con_str, '}] I tcl_tmp_', con_str)

    tcltk::tclvalue(tcltk::.Tcl(com_str))
    raw_bits <- tcltk::tclvalue(paste0('tcl_tmp_', con_str))

    ## we may receive a VAVA value at the start if we don't sleep() enough
    ## when the measurement starts: perhaps some responsed don't get cleared
    ## from the stream?
    ## ##               V       A       V       A
    ## if (raw_bits == "01010110010000010101011001000001") { # when using B32
    if (raw_bits == "1447122497") { # when using I
      cat("'VAVA' -- if you see this, perhaps increase the sleep times\n")
      next()
    }

    int_value <- as.integer(raw_bits)
    res <- c(res, int_value)

    ## debugging
    if (int_value > 1000) {
      cat("reading:", raw_bits, ", queue:", serial::nBytesInQueue(con)[1], "\n")
    }
  }

  ## return list
  return(res)
}
