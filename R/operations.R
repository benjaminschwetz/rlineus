fly <- function(lineus,
                x,
                y){
  up(lineus)
  move(lineus, x, y)
}
crawl <- function(lineus, x, y){
  z <- retrieve_z(x,y, lineus$cv$z)
  move(lineus, x,y,z)
}
down <- function(lineus, x = NULL, y = NULL, z = NULL){
  z_val <- ifelse(
    !is.null(x) & !is.null(y),
    retrieve_z(x,y, lineus$cv$z),
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
move <- function(lineus, x = NULL, y = NULL, z = NULL){
  lineus$pl$g01(x, y, z)
}
draw <- function(lineus, from, to, cur=FALSE){
  if(!cur) fly(lineus, from[1], from[2])
  crawl(lineus, from[1], from[2])
  crawl(lineus, to[1], to[2])
}
