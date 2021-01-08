# Idea: same operation but put all the moves in a data frame:
# GCODE X Y Z
debugPlotter <- R6::R6Class("debugPlotter",
                             public = list(
                               initialize = function(url = "",
                                                     portrait = FALSE){
                                 private$portrait <- portrait
                               },
                               catch_message = function(message){
                                 private$pending_message <- message
                               },
                               retrieve_message = function(timeout=Inf){
                                 message <- private$pending_message
                                 private$pending_message <- NULL
                                 return(message)
                               },
                               g01 = function(x=NULL,y=NULL, z=NULL) {
                                 if(private$portrait) {
                                   x_draw <- x
                                   y_draw <- y
                                 } else {
                                   x_draw <- y
                                   y_draw <- x
                                 }
                                 if(!is.null(x_draw)) {
                                   if(x_draw < 650) x_draw <- 650 else if(x_draw>1775) x_draw <- 1750
                                 }
                                 if(!is.null(y_draw)) {
                                   if(y_draw < -1000) y_draw <- -1000 else if(y_draw>1000) y_draw <- 1000
                                 }
                                 if(!is.null(z)) {
                                   if(z < 0) z <- 0 else if(z>2000) z <- 2000
                                 }
                                 out <- list(
                                   G_code = "G01",
                                   X = x_draw,
                                   Y = y_draw,
                                   Z = z
                                 )
                                 nulls <- sapply(out, is.null)
                                 out[nulls] <- private$cur_position[nulls[-1]]
                                 private$moves <- rbind(private$moves, out)
                                 private$cur_position <- out[-1]
                                 return(invisible())
                               },
                               moves_table = function(){
                                private$moves
                                 },
                               zmap = function(timeout = Inf){
                                 return(matrix(0,2,2))
                               },
                               clear_log = function(){
                                 private$log <- NULL
                               },
                               connect = function() {
                                 private$connect_state <- TRUE
                               },
                               disconnect = function() {
                                 private$connect_state <- FALSE
                               },
                               connected = function() {
                                 private$connect_state
                               },
                               arm_position = function(){
                                 private$cur_position
                               }
                             ),
                             private = list(
                               websocket = NULL,
                               pending_message = NULL,
                               portrait = NULL,
                               connect_state = FALSE,
                               moves = data.frame(
                                 G_code = NA,
                                 X = 0,
                                 Y = 0,
                                 Z = 2000
                               ),
                               cur_position = list(
                                 X = 0,
                                 Y = 0,
                                 Z = 2000
                               )

                             )
)

