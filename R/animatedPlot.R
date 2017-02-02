#' Creates an animated plot function
#'
#' Create an animated plot function from a plot function. Pass a function which
#' generates your plots (eg \link{plot}, \link{hist}) and the maximum number of
#' plots per second and this function returns a function which will ensure the
#' function plots no more than \code{plotsPerSecond} times per second.
#'
#' You then call this function instead of your original function, passing the
#' same parameters you would pass to the original.
#'
#' @param plotFun a plotting function
#' @param plotsPerSecond maximum number of plots per second
#' @param adapt if \code{TRUE}, adapt the plotting interval to ensure plotting
#'   takes no more than half of all processing time.
#'
#' @return a animated plotting function
#' @export
#'
#' @examples
#' aplot <- animatedPlot(plot, 5)
#' walk <- 0
#' xlim <- c(0,10000)
#' for (i in 1:9999) {
#'   Sys.sleep(0.001)
#'   walk <- c(walk, walk[i] + runif(1) - 0.5)
#'   aplot(seq_along(walk), walk, type="l", xlim=xlim)
#' }
animatedPlot <- function(plotFun, plotsPerSecond=5, adapt=T) {
  interval <- 1 / plotsPerSecond
  originalInterval <- interval
  lastPlotTime <- proc.time()[3]
  function(...) {
    calledTime <- proc.time()[3]
    if (calledTime - lastPlotTime > interval) {
      plotFun(...)
      finishTime <- proc.time()[3]
      lastPlotTime <<- finishTime
      if (adapt) {
        plotTime <- calledTime - finishTime
        if (plotTime > originalInterval / 2) {
          interval <<- plotTime * 2
        } else if (interval > originalInterval) {
          interval <<- originalInterval
        }
      }
    }
  }
}
