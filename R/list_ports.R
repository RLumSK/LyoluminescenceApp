#'@title List available ports
#'
#'@description A convenience wrapper around [serial::listPorts]
#'
#'@returns [character] vector with available ports
#'
#'@author Sebastian Kreutzer,  Institute of Geography, Heidelberg University (DE)
#'
#'@examples
#'list_ports()
#'
#'@keywords internal
#'
#'@md
#'@export
list_ports <- function() {
  tryCatch(suppressMessages(serial::listPorts()),
           error = function(e) "ttyUSB0")
}
