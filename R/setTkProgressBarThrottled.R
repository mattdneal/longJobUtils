#' Create a throttled Tk progress bar
#'
#' @param title
#' @param label
#' @param min
#' @param max
#' @param initial
#' @param width
#' @param updateFreq
#'
#' @return
#' @export
#'
#' @import tcltk
tkProgressBarThrottled <- function(title = "R progress bar", label = "", min = 0, max = 1,
                                   initial = 0, width = 300, updateFreq=1) {
  out <- list()
  out$progressBar <- tkProgressBar(title, label, min, max,
                          initial, width)
  out$max <- max
  updateCreator <- function(updateFreq) {
    lastUpdated <- proc.time()[3]
    function() {
      if (proc.time()[3] - lastUpdated > updateFreq) {
        lastUpdated <<- proc.time()[3]
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  }
  out$update <- updateCreator(updateFreq)
  return(out)
}

#' A Tk Progress Bar with Throttled Updates
#'
#' @param pb
#' @param value
#' @param title
#' @param label
#' @param updateFreq
#'
#' @return
#' @export
#'
#' @import tcltk
#'
#' @examples
#' pb <- tkProgressBar("Example", min=0, max=300, initial=0)
#' for (i in 1:300) {
#'   Sys.sleep(0.1)
#'   setTkProgressBarThrottled(pb, i)
#' }
#' close(pb)
setTkProgressBarThrottled <- function(pb, value, title = NULL, label = NULL) {
  if (pb$update() | value==pb$max) {
      setTkProgressBar(pb$progressBar, value, title, label)
  } else {
  }
}

#' Close a throttled Tk progress bar
#'
#' @param pb
#'
#' @return
#' @export
closeTkProgressBarThrottled <- function(pb) {
  close(pb$progressBar)
}
