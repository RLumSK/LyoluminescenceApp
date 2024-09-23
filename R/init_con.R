#'@title Initialise Serial Connection to PMT
#'
#'@description Establish the serial connect via the RS232 interface to the
#'PMT using [serial::serialConnection] with preset values.
#'
#'@param port [character] (**required**): port name to be used for the connection
#'
#'@returns Returns object of class `'serialConnection'`.
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
init_con <- function(port) {
  con <- serial::serialConnection(
    name = "Lyoluminescence_ASCII",
    port = port,
    mode = "9600,n,8,1",
    newline = 1,
    buffering = 'none',
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

    ## communicate with connection, otherwise it will not work
    serial::write.serialConnection(con, "\n")

    ## ensure the queue is empty
    while (serial::nBytesInQueue(con)[1] > 0) {
      serial::read.serialConnection(con)
    }
    stopifnot(serial::nBytesInQueue(con)[1] == 0)
  } else {
    cli::cli_alert_info(paste0(text_, " ... already connected!"))
  }

  ## set mode
  con$usr_status <- "connected"
  con$usr_mode <- "initialised"

  return(con)
}
