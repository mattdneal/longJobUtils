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
setTkProgressBarThrottled <- function(pb, value, title = NULL, label = NULL, updateFreq=1) {
  current.time <- proc.time()[3]
  if (is.null(pb$lastUpdate)) {
    setTkProgressBar(pb, value, title, label)
    pb$lastUpdate <- current.time
  } else {
    if (current.time - pb$lastUpdate > 1) {
      setTkProgressBar(pb, value, title, label)
      pb$lastUpdate <- proc.time()[3]
    }
  }
}
