#' @name device
#' @rdname device
#' @title graphics device for line-us plotter
#' @description readme for how to use it
#' @param portrait set orientation. \code{TRUE} for portrait, \code{FALSE} for landscape
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#'  lineus_dev()
#'  boxplot(data = iris, Sepal.Length ~Species)
#'  dev.off()
#' }
NULL
#' @rdname device
#' @export
lineus_dev <- function(portrait = TRUE, debug = FALSE){
  if(debug) {
    plotter <- debugPlotter$new(portrait = portrait)
  } else  plotter <- LineUsPlotter$new(portrait = portrait)
  Sys.sleep(.5)
  z_mat <- render_canvas(plotter,
                         portrait = portrait)
  z_mat$z <- round(z_mat$z)
  devout::rdevice(
    rfunction = plotter_callback,
    device_name = "line_us",
    pl = plotter,
    zm = z_mat,
    portrait = portrait,
    debug = debug
  )
}

#' callback function for plotting
#'
#' @param device_call call
#' @param args arguments
#' @param state state
plotter_callback <- function(
  device_call,
  args,
  state
) {
  # message("[[", device_call,"]]")
  # dput(args)
  state <- switch( device_call,
    open = .open(args, state),
    close = .close(args, state),
    onExit = .abort(args, state),
    clip = .clip(args, state),
    circle = .circle(args, state),
    line = .line(args, state),
    polygon = .polygon(args, state),
    polyline = .polyline(args, state),
    path = .path(args, state),
    rect = .rect(args, state)
  )
  state
}
.open <- function(args, state){
  #set canvas
  if(state$rdata$portrait){
    state$dd$bottom <- -1000
    state$dd$top <- 1000
    state$dd$left <- 650
    state$dd$right <- 1775
  } else {
    state$dd$left <- -1000
    state$dd$right <- 1000
    state$dd$top <- 650
    state$dd$bottom <- 1775
  }
  #move plotter up
  up(state$rdata$pl)
  state
}
.close <- function(args, state){
  if(state$rdata$debug){
    tmp_file <- tempfile(fileext = ".png")
    p <- ggplot(state$rdata$pl$moves_table()) +
      aes(X,Y) +
      geom_path(aes(
        alpha = Z > 0,
        color = Z > 0)) +
      geom_point(shape = 1) +
      scale_alpha_manual(
        values = c(1, .3),
        guide = FALSE
      )
    ggsave(tmp_file, plot = p)
    rstudioapi::viewer(tmp_file)
  }
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
  fly(state$rdata$pl, x_s[1], y_s[1])
  mapply(
    crawl,
    x= x_s,
    y = y_s,
    MoreArgs=
      list(
        lineus = state$rdata$pl,
        canvas = state$rdata$zm
      )
    )
  up(state$rdata$pl)
  state
}
.line<- function(args, state){
  draw(
    state$rdata$pl,
    from = c(args$x1,args$y1),
    to = c(args$x2,args$y2),
    canvas = state$rdata$zm
  )
  up(state$rdata$pl)
  state
}
.rect <- function(args, state){
  x_s  <-  c(args$x0, args$x1)[c(1, 2, 2, 1)]
  y_s <- c(args$y0, args$y1)[c(1, 1, 2, 2)]
  fly(args$x0,args$y0)
  mapply(
    crawl,
    x= x_s,
    y = y_s,
    MoreArgs=
      list(
        lineus = state$rdata$pl,
        canvas = state$rdata$zm
      )
  )
  up(state$rdata$pl)
  state
}
.polygon <- function(args, state){
  fly(state$rdata$pl, args$x[args$n],args$y[args$n])
  down(state$rdata$pl, x = args$x[args$n], y = args$y[args$n], canvas = state$rdata$zm)
  mapply(
    crawl,
    x= args$x,
    y = args$y,
    MoreArgs=
      list(
        lineus = state$rdata$pl,
        canvas = state$rdata$zm
      )
  )
  up(state$rdata$pl)
  state
}
.polyline <- function(args, state) {
  fly(state$rdata$pl, args$x[args$n],args$y[args$n])
  down(state$rdata$pl, x = args$x[args$n], y = args$y[args$n], canvas = state$rdata$zm)
  mapply(
    crawl,
    x= args$x,
    y = args$y,
    MoreArgs=
      list(
        lineus = state$rdata$pl,
        canvas = state$rdata$zm
      )
  )
  up(state$rdata$pl)
  state
}
.path <- function(args, state){
  path_id <- rep(seq_len(args$npoly), args$nper)
  x <- split(args$x, path_id)
  y <- split(args$y, path_id)
  paths <- lapply(seq_along(x), function(i) {
    list(x = x[[i]], y = y[[i]])
  })
  lapply(paths,
         function(curve){
           fly(state$rdata$pl, curve$x[1],curve$y[1])
           down(state$rdata$pl, x = curve$x[1], y = curve$y[1], canvas = state$rdata$zm)
           mapply(
             crawl,
             x= curve$x,
             y = curve$y,
             MoreArgs=
               list(
                 lineus = state$rdata$pl,
                 canvas = state$rdata$zm
               )
           )
           up(state$rdata$pl)
         }
  )
  state
}

