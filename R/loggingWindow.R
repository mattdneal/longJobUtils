
#' Create a logging window using TCL/TK
#'
#' @param height
#' @param width
#' @param timestamp
#'
#' @return
#' @export
#'
#' @import tcltk
#'
#' @examples
#' win <- loggingWindow()
#' updateLog(win, "Hello World!")
loggingWindow <- function(height=30, width=100, timestamp=TRUE, title="Logging window") {
  win <- tktoplevel()
  tktitle(win) <- title
  win$env$scrv <- ttkscrollbar(win, orient="vertical", command = function(...) tkyview(win$env$txt, ...))
  win$env$txt <- tktext(win,
                        yscrollcommand=function(...) tkset(win$env$scrv, ...),
                        height=height, width=width)
  tkgrid(win$env$txt, win$env$scrv, sticky='ns')

  win$env$timestamp <- timestamp
  return(win)
}

#' Add a line to a logging window
#'
#' @param win
#' @param message
#'
#' @return
#' @export
#'
#' @import tcltk
#'
#' @examples
#' win <- loggingWindow()
#' updateLog(win, "Hello World!")
updateLog <- function(win, message) {
  if(win$env$timestamp) message <- paste("[", Sys.time(), "] ", message, sep="")
  tkinsert(win$env$txt, "0.1", paste(message, "\n"))
  invisible(NULL)
}
