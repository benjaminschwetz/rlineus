lineus_dev <- function(){
  plotter <- load_plotter()
  plotter$connect()
  z_mat <- render_canvas(plotter, keep_con = TRUE)
  z_mat$z <- round(z_mat$z)
  devout::rdevice(
    rfunction = plotter_callback,
    device_name = "line_us",
    pl = plotter,
    zm = z_mat
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
    line = .line(args, state),
    polygon = .polygon(args, state),
    rect = .rect(args, state)
  )
  state
}
.open <- function(args, state){
  state$dd$bottom <- -1000
  state$dd$top <- 1000
  state$dd$left <- 650
  state$dd$right <- 1775
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
  state
}
.circle <- function(args, state){
  n_points <-  round((2 * pi) / acos((args$r - 0.5) / args$r)) * 5
  angles <- seq(0, 2*pi, length.out = n_points)
  x_s =  cos(angles) * args$r * 3 + args$x
  y_s =  sin(angles) * args$r * 3 + args$y
  state$rdata$pl$g01(z=z_dn)
  mapply(
    draw,
    x= x_s,
    y = y_s,
    MoreArgs=
      list(
        rdata = state$rdata
      )
    )
  state$rdata$pl$g01(z=z_up)
  state
}
.line<- function(args, state){
  state$rdata$pl$g01(args$x1,args$y1)
  state$rdata$pl$g01(z=retrieve_z(args$x1,args$y1, state$rdata$zm))
  draw(args$x2,args$y2,state$rdata)
  state$rdata$pl$g01(z=z_up)
  state
}
.rect <- function(args, state){
  x_s  <-  c(args$x0, args$x1)[c(1, 2, 2, 1)]
  y_s <- c(args$y0, args$y1)[c(1, 1, 2, 2)]
  state$rdata$pl$g01(args$x0,args$y0)
  state$rdata$pl$g01(z=z_dn)
  state$rdata$pl$g01(z=z_dn)
  mapply(
    draw,
    x= x_s,
    y = y_s,
    MoreArgs=
      list(
        rdata = state$rdata
      )
    )
  state$rdata$pl$g01(z=z_up)
  state
}
.polygon <- function(args, state){
  state$rdata$pl$g01(args$x[args$n],args$y[args$n])
  state$rdata$pl$g01(z=retrieve_z(args$x[args$n],args$y[args$n],state$rdata$zm))
  mapply(
    draw,
    x= args$x,
    y = args$y,
    MoreArgs=
      list(
        rdata = state$rdata
      )
  )
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
z_up <- 1000
z_dn <- 0
# z_array <-
# z_01 <- 210
# z_11 <- 350
# z_00 <- 520
# z_10 <- 860
min_x = 650
max_x = 1775
min_y = -1000
max_y = 1000
# z_mat <- akima::bilinear.grid(
#   x = c(min_x, max_x),
#   y = c(min_y, max_y),
#   z = matrix(c(z_00, z_01, z_10, z_11), nrow = 2),
#   xlim = c(min_x, max_x),
#   ylim= c(min_y, max_y),
#   # nx= length(min_x:max_x),
#   # ny= length(min_y:max_y)
#   dx = 1,
#   dy = 1
# )
fetch_z_map <- function(plotter){
  str_z_map <- plotter$get_info()$ZMap
  v_z_map <- strsplit(str_z_map, ";")[[1]]
  num_z_map <- as.numeric(v_z_map)
  matrix(num_z_map, nrow = 2)
}
interpolate_z_matrix <- function(x_limits, y_limits, z_corners){
  akima::bilinear.grid(
    x = x_limits,
    y = y_limits,
    z = z_corners,
    xlim = x_limits,
    ylim= y_limits,
    dx = 1,
    dy = 1
  )
}
retrieve_z <- function(x, y, matrix){
  x_index <- which(matrix$x == round(x))
  y_index <- which(matrix$y == round(y))
  z <- matrix$z[x_index, y_index]
  return(z)
}
draw <-     function(x,y, rdata){
  rdata$pl$g01(
    x,
    y,
    retrieve_z(x,y,rdata$zm))
}
