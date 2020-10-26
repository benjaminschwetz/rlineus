render_canvas <- function( plotter,
                           x_lim = c(650,1775),
                           y_lim = c(-1000, 1000)
){
  z <- plotter$zmap()
  out <- interpolate_z_matrix(x_lim, y_lim, z)
  return(out)
}




