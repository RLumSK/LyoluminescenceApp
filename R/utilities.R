#' Empty plot
#'
#' @param ... arguments to be passed to [plot.default]
#'
#' @keywords internal
#'
#' @md
#' @export
plot_empty <- function(...) {
  plot(
    x = NA,
    y = NA,
    xlim = c(0, 100),
    ylim = c(0, 1),
    ...
  )
  text(
    x = (par()$usr[2] - par()$usr[1])/2,
    y = (par()$usr[4] - par()$usr[3])/2,
    "NO DATA")

}
