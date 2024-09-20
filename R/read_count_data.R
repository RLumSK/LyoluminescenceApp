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
  while (serial::nBytesInQueue(con)[1] <= 65536 && length(res) <= freq) {
    ## limit the readout speed to half the minimum gate time
    Sys.sleep(0.005)

    ## read bits
    com_str <- paste0('binary scan [read ${sdev_', con_str, '}] I tcl_tmp_', con_str)

    tcltk::tclvalue(tcltk::.Tcl(com_str))
    raw_bits <- tcltk::tclvalue(paste0('tcl_tmp_', con_str))

    ## jump to next cycle if empty
    if (raw_bits == "") next()

    ## we may receive a VAVA value at the start, perhaps something from
    ## the send_cmd hasn't removed those responses from the stream
    ## ##               V       A       V       A
    ## if (raw_bits == "01010110010000010101011001000001") { # when using B32
    if (raw_bits == "1447122497") { # when using I
      cat("VAVA\n")
      next()
    }

    ## debugging
    if (as.integer(raw_bits) > 1000) {
      cat("reading:", raw_bits, ", queue:", serial::nBytesInQueue(con)[1], "\n")
    }

    int_value <- as.integer(raw_bits)
    res <- c(res, int_value)

    ## this commented out block is unnecessary when reading data with the `I`
    ## format flag
    if (FALSE) {

    ## split the bits and create logical values
    raw_bits <- strsplit(raw_bits, split = "", fixed = TRUE)[[1]] |>
      as.numeric()

    ## add to buffer if we have more than 0 bit
    if (length(raw_bits) > 0)
      buffer <- c(buffer, raw_bits)

    ## evaluate buffer
    if (length(buffer) >= 32) {
      ## transform to logical
      bits <- as.logical(buffer[1:32])

      ## create integer
      int_value <- sum(2^rev(seq_along(bits) - 1)[bits])

      ## results
      res <- c(res, int_value)

      ## clear buffer of the used bits
      buffer <- buffer[-c(1:32)]

    }

    } # end if FALSE
  }

  ## return list
  return(res)
}
