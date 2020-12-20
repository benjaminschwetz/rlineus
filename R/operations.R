#' @name operations
#' @rdname operations
#'
#' @title Operations
#'
#' @description Functions to move the pen around
#'
#' @param lineus plotter (R6-Class)
#' @param x new x coordinate to move to
#' @param y new y coordinate to move to
#' @param z new z coordinate to move to
#' @param canvas Matrix with interpolated x coordinates, usually output of \code{\link[rlineus]{interpolate_z_matrix}}
#' @param cur start from current position?
#'
#' @return nothing
NULL
#' @rdname operations
#' @export
up <- function(lineus){
  move(lineus, z=1000)
}
#' @rdname operations
#' @export
down <- function(lineus, x = NULL, y = NULL, z = NULL, canvas = NULL){
  z_val <- ifelse(
    !is.null(x) & !is.null(y) & !is.null(canvas),
    retrieve_z(x,y, canvas),
    ifelse(
      !is.null(z),
      z,
      0)
  )
  move(lineus, x, y, z_val)
}
#' @rdname operations
#' @export
move <- function(lineus, x = NULL, y = NULL, z = NULL){
  text <- lineus$g01(x, y, z)
}
#' @rdname operations
#' @export
fly <- function(lineus,
                x,
                y){
  up(lineus)
  move(lineus, x, y)
}
#' @rdname operations
#' @export
crawl <- function(lineus, x, y, canvas = NULL){
  z <- ifelse(!is.null(canvas), retrieve_z(x,y, canvas), 0)
  move(lineus, x,y,z)
}
#' @rdname operations
#' @export
draw <- function(lineus, from, to, cur=FALSE, canvas = NULL){
  if(!cur) fly(lineus, from[1], from[2])
  crawl(lineus, from[1], from[2], canvas)
  crawl(lineus, to[1], to[2], canvas)
}
