# txtProgressBar Demo
if (FALSE) {
  txtProgressBarDemo <- function(print.i=FALSE, ...) {
    pb <- txtProgressBar(max=100, ...)
    for(i in 1:100) {
      if (print.i) print(i)
      Sys.sleep(0.01)
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  #text progress bar style 1 - no redraw
  txtProgressBarDemo(style=1)

  #text progress bar style 2 - redraw
  txtProgressBarDemo(style=2)

  #text progress bar style 1 - no redraw, print i
  txtProgressBarDemo(style=1, print.i=T)

  #text progress bar style 2 - redraw, print i
  txtProgressBarDemo(style=2, print.i=T)

  #text progress bar style 3 - redraw, fancy
  txtProgressBarDemo(style=3, print.i=F)
}

#tkProgressBar Demo
if (FALSE) {
  tkProgressBarDemo <- function(max=100, updateLabel=F, ...) {
    interval <- 2 / max
    if (!updateLabel) {
      pb <- tcltk::tkProgressBar(max=max, ...)
    } else {
      pb <- tcltk::tkProgressBar(max=max, label=paste(0, "/", max, sep=""), ...)
    }
    for(i in 1:max) {
      Sys.sleep(interval)
      if (!updateLabel) {
        tcltk::setTkProgressBar(pb, i)
      } else {
        tcltk::setTkProgressBar(pb, i, label=paste(i, "/", max, sep=""))
      }
    }
    close(pb)
  }

  noProgressBarDemo <- function(max=100, updateLabel=F, ...) {
    interval <- 2 / max
    if (!updateLabel) {
      NULL
    } else {
      NULL
    }
    for(i in 1:max) {
      Sys.sleep(interval)
      if (!updateLabel) {
        NULL
      } else {
        NULL
      }
    }
  }

  tkProgressBarDemo()

  tkProgressBarDemo(title="A title", label="A label")

  tkProgressBarDemo(title="A title", updateLabel=T)

  system.time(noProgressBarDemo(title="A title", updateLabel=T, max=10000)) #2.9
  system.time(tkProgressBarDemo(title="A title", updateLabel=T, max=10000)) #14

  system.time(noProgressBarDemo(title="A title", updateLabel=T, max=100000)) #9.4
  system.time(tkProgressBarDemo(title="A title", updateLabel=T, max=100000)) #120

  system.time(tkProgressBarDemo(title="A title", updateLabel=F, max=10000)) #14


  # Throttled progress bars:
  tkProgressBarThrottledDemo <- function(max=100, updateLabel=F, ...) {
    interval <- 2 / max
    if (!updateLabel) {
      pb <- longJobUtils::tkProgressBarThrottled(max=max, ...)
    } else {
      pb <- longJobUtils::tkProgressBarThrottled(max=max, label=paste(0, "/", max, sep=""), ...)
    }
    for(i in 1:max) {
      Sys.sleep(interval)
      if (!updateLabel) {
        longJobUtils::setTkProgressBarThrottled(pb, i)
      } else {
        longJobUtils::setTkProgressBarThrottled(pb, i, label=paste(i, "/", max, sep=""))
      }
    }
    closeTkProgressBarThrottled(pb)
  }

  tkProgressBarThrottledDemo(updateFreq=0.5)
  tkProgressBarThrottledDemo(updateFreq=0.1)

  system.time(tkProgressBarThrottledDemo(title="A title", updateLabel=T, max=10000)) #3.5
  system.time(noProgressBarDemo(title="A title", updateLabel=T, max=10000)) #2.9
  #unthrottled: 14
}

# loggingWindow
if (FALSE) {
  win <- loggingWindow(title="Hello!")
  updateLog(win, "Hello world!")
}

# animatedPlot
if (FALSE) {
  dev.off()
  walk <- 0
  xlim <- c(0,10000)
  for (i in 1:9999) {
    Sys.sleep(0.001)
    walk <- c(walk, walk[i] + runif(1) - 0.5)
    plot(seq_along(walk), walk, type="l", xlim=xlim, ylab="Position", xlab="Iteration")
  }

  dev.off()
  aplot <- animatedPlot(plot, 5)
  walk <- 0
  xlim <- c(0,10000)
  for (i in 1:9999) {
    Sys.sleep(0.001)
    walk <- c(walk, walk[i] + runif(1) - 0.5)
    aplot(seq_along(walk), walk, type="l", xlim=xlim, ylab="Position", xlab="Iteration")
  }
}

#closures
if (FALSE) {
  counterCreator <- function(start=0) {
    counter <- 0
    function() {
      counter <<- counter + 1
      return(counter)
    }
  }

  environment(counterCreator)

  countFun <- counterCreator()
  countFun2 <- counterCreator()

  environment(countFun)
  environment(countFun2)
  countFun()
  countFun()
  countFun()
  countFun()

  countFun2()

}
