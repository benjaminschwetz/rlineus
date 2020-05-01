# py_lineus <- NULL
# .onLoad <- function(libname, pkgname) {
#   py_lineus <<- reticulate::import("lineus", delay_load = TRUE)
# }
lineus_dev <- function(){
  plotter <- load_plotter()
  devout::rdevice(
    rfunction = plotter_callback,
    device_name = "line_us",
    pl = plotter
  )
}
plotter_callback <- function(
  device_call,
  args,
  state
) {
  state <- switch( device_call,
    open = .open(args, state),
    close = .close(args, state),
    onExit = .abort(args, state),
    line = .line(args, state)
  )
  state
}
.open <- function(args, state){
  state$rdata$pl$connect()
  state$rdata$pl$g01(0,0)
  state
}
.close <- function(args, state){
  state$rdata$pl$disconnect()
  state
}
.abort <- function(args, state){
  state
}
.line<- function(args, state){
  state
}
min_x <- 650
max_x <- 1775
min_y <- -1000
max_y <- 1000
