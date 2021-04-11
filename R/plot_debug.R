test <- data.frame(
  G_code = NA,
  X = 10,
  Y = 1000,
  Z = 2000
)

assign_line_color <- function(s,t){
  ifelse(
    s+t > 0,
    "grey",
    "red")
}
draw_moves <- function(moves_table){
  home_pos <- data.frame(
    G_code = NA,
    X = 1000,
    Y = 1000,
    Z = 2000
  )
  start_frame <- rbind(
    home_pos,
    moves_table
  )
  stop_frame <- rbind(
    moves_table,
    home_pos
  )
  line_table <- cbind(start_frame, stop_frame)
  names(line_table) <- c(
    "G_code.source",
    "X.source",
    "Y.source",
    "Z.source",
    "G_code.target",
    "X.target",
    "Y.target",
    "Z.target"
  )
  line_table$color <- assign_line_color(line_table$Z.source, line_table$Z.target)

  # plot tha tthing
}

