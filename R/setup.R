render_canvas <- function( plotter,
                           x_lim = c(650,1775),
                           y_lim = c(-1000, 1000),
                           keep_con = FALSE
){
  if(plotter$readyState()!=1 ) plotter$connect()
  z <- fetch_z_map(plotter)
  out <- interpolate_z_matrix(x_lim, y_lim, z)
  if(!keep_con) plotter$close()
  return(out)
}




