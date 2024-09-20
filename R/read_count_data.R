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

  # read stuff
  while (serial::nBytesInQueue(con)[1] <= 65536 && length(res) <= freq) {
    ## limit the readout speed to the maximum gate time
    Sys.sleep(0.01)

    ## read bits
    com_str <- paste0('binary scan [read ${sdev_', con_str, '}] B* tcl_tmp_', con_str)

    tcltk::tclvalue(tcltk::.Tcl(com_str))
    raw_bits <- tcltk::tclvalue(paste0('tcl_tmp_', con_str))

    ## jump to next cycle if empty
    if (raw_bits == "") next()

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

  }

  ## return list
  return(res)
}
