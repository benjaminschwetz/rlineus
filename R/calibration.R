#' @name calibration
#' @rdname calibration
#'
#' @title Paper height calibration
#'
#' @description This family of functions deals with the fact that the paper plane is not at \code{z=0}
#'
#' @param lineus plotter (R6-Class)
#' @param x_limits minimum and maximum value of x-coordinates
#' @param y_limits minimum and maximum value of y-coordinates
#' @param z_corners 2x2 matrix of calibration results (use phone app)
#' @param x x coordinate to retrieve z value for
#' @param y y coordinate to retrieve z value for
#' @param matrix matrix of interpolated z-coordinates
#' @param portrait set orientation. \code{TRUE} for portrait, \code{FALSE} for landscape
NULL
#' @rdname callibration
#' @export
fetch_z_map <- function(lineus){
  lineus$zmap()
}
#' @rdname callibration
#' @export
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
#' @rdname callibration
#' @export
render_canvas <- function(lineus,
                          x_lim = c(650,1775),
                          y_lim = c(-1000, 1000),
                          portrait = TRUE
){
  z <- lineus$zmap()
  m <- interpolate_z_matrix(x_lim, y_lim, z)
  if(portrait){
    out <- m
  } else {
    out <- t(m[nrow(m):1,])
  }
  return(out)
}
#' @rdname callibration
#' @export
retrieve_z <- function(x, y, matrix){
  x_index <- which(matrix$x == round(x))
  y_index <- which(matrix$y == round(y))
  z <- matrix$z[x_index, y_index]
  return(z)
}
