fly <- function(lineus,
                x,
                y){
  up(lineus)
  move(lineus, x, y)
}
crawl <- function(lineus, x, y, canvas = NULL){
  z <- ifelse(!is.null(canvas), retrieve_z(x,y, canvas), 0)
  move(lineus, x,y,z)
}
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
up <- function(lineus){
  move(lineus, z=1000)
}
move <- function(lineus, x = NULL, y = NULL, z = NULL, verbose = FALSE){
  text <- lineus$g01(x, y, z)
  if(verbose) message(text)
}
draw <- function(lineus, from, to, cur=FALSE, canvas = NULL){
  if(!cur) fly(lineus, from[1], from[2])
  crawl(lineus, from[1], from[2], canvas)
  crawl(lineus, to[1], to[2], canvas)
}
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
