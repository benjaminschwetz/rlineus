# py_lineus <- NULL
# .onLoad <- function(libname, pkgname) {
#   py_lineus <<- reticulate::import("lineus", delay_load = TRUE)
# }
lineus_dev <- function(){
  plotter <- load_plotter()
  devout::rdevice(
    rfunction = plotter_callback,
    device_name = "line_us",
    pl = plotter,
    canvas = c(0,0,0,0)
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
    clip = .clip(args, state),
    circle = .circle(args, state),
    line = .line(args, state)
  )
  state
}
.open <- function(args, state){
  state$rdata$pl$connect()
  state$rdata$pl$g01(0,0,z_up)
  state
}
.close <- function(args, state){
  state$rdata$pl$disconnect()
  state
}
.abort <- function(args, state){
  state
}
.clip <- function(args, state){
  state$rdata$canvas <- c(
    args$x0,
    args$x1,
    args$y0,
    args$y1
  )
  state
}
.circle <- function(args, state){
  sv <- function(x = NULL, y = NULL, canvas = state$rdata$canvas){
    scale_value(x, y, canvas)
  }
  n_points <-  round((2 * pi) / acos((args$r - 0.5) / args$r)) * 10
  angles <- seq(0, 2*pi, length.out = n_points)
  shape <- list(
    x = cos(angles) * args$r + args$x,
    y = sin(angles) * args$r + args$y
  )
  state$rdata$pl$g01(sv(x=shape$x[1]),sv(y=shape$y[1]))
  state
}
.line<- function(args, state){
  sv <- function(x = NULL, y = NULL, canvas = state$rdata$canvas){
    scale_value(x, y, canvas)
  }
  state$rdata$pl$g01(sv(x=args$x1),sv(y=args$y1))
  state$rdata$pl$g01(z=z_dn)
  state$rdata$pl$g01(sv(x=args$x2),sv(y=args$y2))
  state$rdata$pl$g01(z=z_up)
  state
}
scale_value <- function(x= NULL, y = NULL,
               canvas,
               area = c(
                 min_x = 650,
                 max_x = 1775,
                 min_y = -1000,
                 max_y = 1000
               )
){
  if(!is.null(x)){
   val <- x
   range <- canvas[1:2]
   limits <- area[1:2]
  } else{
    if(!is.null(y)){
      val <- y
      range <- canvas[3:4]
      limits <- area[3:4]
    } else {
      stop("No value")
    }
  }
  out <- ((limits[2]-limits[1])*(val - range[1])/(range[2]-range[1]))+limits[1]
  return(out)
}
z_up <- 600
z_dn <- 250
