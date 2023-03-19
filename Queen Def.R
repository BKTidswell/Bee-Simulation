# Queen

#New Way of Running Queen

Queen <- R6Class("Queen",
                 public = list(
                   cur_X = NULL,
                   cur_Y = NULL,
                   cur_X_ind = NULL,
                   cur_Y_ind = NULL,
                   initialize = function(cur_X = NA, cur_Y = NA) {
                     self$cur_X <- cur_X
                     self$cur_Y <- cur_Y
                     
                     near_hex <- nearest_hex(self$cur_X,self$cur_Y)
                     self$cur_X_ind <- max(min(near_hex[[1]],MAX_COLS),0)
                     self$cur_Y_ind <- max(min(near_hex[[2]],MAX_ROWS),0)
                   },
                   move = function(){
                     #This makes a random angle of 0 move to the center of the comb
                     angle_move <- atan2(CENTER_Y-self$cur_Y,CENTER_X-self$cur_X) + runif(1,-pi,pi)
                     
                     #If on the edge restrict movement to the half facing the center
                     if(self$cur_X_ind == 1 || self$cur_X_ind == MAX_COLS ||
                        self$cur_Y_ind == 1 || self$cur_Y_ind == MAX_ROWS){
                       angle_move <- atan2(CENTER_Y-self$cur_Y,CENTER_X-self$cur_X) + runif(1,-pi,pi)
                     }
                     
                     #So we calculate the next X and Y Values
                     self$cur_X <- max(min(self$cur_X + cos(angle_move),MAX_X),0)
                     self$cur_Y <- max(min(self$cur_Y + sin(angle_move),MAX_Y),0)
                     
                     #Then we find the nearest hex
                     near_hex <- nearest_hex(self$cur_X,self$cur_Y)
                     self$cur_X_ind <- max(min(near_hex[[1]],MAX_COLS),0)
                     self$cur_Y_ind <- max(min(near_hex[[2]],MAX_ROWS),0)
                     
                     #And *THEN* we set x and y to the center of the current hex, to prevent skipping or drift  
                     self$cur_X <- (hexdat_centers %>% filter(Xind == self$cur_X_ind & Yind == self$cur_Y_ind))$x
                     self$cur_Y <- (hexdat_centers %>% filter(Xind == self$cur_X_ind & Yind == self$cur_Y_ind))$y
                     
                   },
                   move_to_center = function(){
                     dist_to_center <- sqrt((CENTER_X-self$cur_X)^2+(CENTER_Y-self$cur_Y)^2)
                     #This makes a random angle of 0 move to the center of the comb
                     #This biases movement more towards the center, so that the further the queen is out
                     #The more likely she is to move inwards. Numbers from paper
                     angle_move <- atan2(CENTER_Y-self$cur_Y,CENTER_X-self$cur_X) + 
                       rnorm(1,0, 5 - (5 - 2.828)*(dist_to_center/sqrt(22^2+37^2)))
                     
                     #If on the edge restrict movement to the half facing the center
                     if(self$cur_X_ind == 1 || self$cur_X_ind == MAX_COLS ||
                        self$cur_Y_ind == 1 || self$cur_Y_ind == MAX_ROWS){
                       angle_move <- atan2(CENTER_Y-self$cur_Y,CENTER_X-self$cur_X) + runif(1,-pi,pi)
                     }
                     
                     #So we calculate the next X and Y Values
                     self$cur_X <- max(min(self$cur_X + cos(angle_move),MAX_X),0)
                     self$cur_Y <- max(min(self$cur_Y + sin(angle_move),MAX_Y),0)
                     
                     #Then we find the nearest hex
                     near_hex <- nearest_hex(self$cur_X,self$cur_Y)
                     self$cur_X_ind <- max(min(near_hex[[1]],MAX_COLS),0)
                     self$cur_Y_ind <- max(min(near_hex[[2]],MAX_ROWS),0)
                     
                     #And *THEN* we set x and y to the center of the current hex, to prevent skipping or drift  
                     self$cur_X <- (hexdat_centers %>% filter(Xind == self$cur_X_ind & Yind == self$cur_Y_ind))$x
                     self$cur_Y <- (hexdat_centers %>% filter(Xind == self$cur_X_ind & Yind == self$cur_Y_ind))$y
                     
                   },
                   move_from_heat = function(){
                     dist_to_center <- sqrt((HEAT_CENTER_X-self$cur_X)^2+(HEAT_CENTER_Y-self$cur_Y)^2)
                     #This makes a random angle of 0 move away from the center of the heat
                     #This biases movement away from the heat, so that the closer the queen is
                     #The more likely she is to move outwards
                     
                     #This should reverse it, but check the math
                     # angle_move <- atan2(self$cur_Y-HEAT_CENTER_Y,self$cur_X-HEAT_CENTER_X) + 
                     #   rnorm(1,0, 2.828 - (2.828 - 5)*(dist_to_center/sqrt(22^2+37^2)))
                     
                     #Trying to change it to see if changing these values helps
                     #Also changing the max distance to be right
                     angle_move <- atan2(self$cur_Y-HEAT_CENTER_Y,self$cur_X-HEAT_CENTER_X) + 
                       rnorm(1,0, QUEEN_HEAT_MOVEMENT_SD - (QUEEN_HEAT_MOVEMENT_SD - 5)*(dist_to_center/sqrt(34^2+48^2)))
                    
                     #If on the edge restrict movement to the half facing the center
                     if(self$cur_X_ind == 1 || self$cur_X_ind == MAX_COLS ||
                        self$cur_Y_ind == 1 || self$cur_Y_ind == MAX_ROWS){
                       angle_move <- atan2(CENTER_Y-self$cur_Y,CENTER_X-self$cur_X) + runif(1,-pi,pi)
                     }
                     
                     #So we calculate the next X and Y Values
                     self$cur_X <- max(min(self$cur_X + cos(angle_move),MAX_X),0)
                     self$cur_Y <- max(min(self$cur_Y + sin(angle_move),MAX_Y),0)
                     
                     #Then we find the nearest hex
                     near_hex <- nearest_hex(self$cur_X,self$cur_Y)
                     self$cur_X_ind <- max(min(near_hex[[1]],MAX_COLS),0)
                     self$cur_Y_ind <- max(min(near_hex[[2]],MAX_ROWS),0)
                     
                     #And *THEN* we set x and y to the center of the current hex, to prevent skipping or drift  
                     self$cur_X <- (hexdat_centers %>% filter(Xind == self$cur_X_ind & Yind == self$cur_Y_ind))$x
                     self$cur_Y <- (hexdat_centers %>% filter(Xind == self$cur_X_ind & Yind == self$cur_Y_ind))$y
                     
                   },
                   lay_brood = function(current_hive){
                     curr_fill <- current_hive[self$cur_Y_ind,self$cur_X_ind,1]
                     curr_val <- current_hive[self$cur_Y_ind,self$cur_X_ind,2]
                     
                     #Rounding errors killed me here
                     
                     #If you end up on an empty hive
                     if(curr_fill == EMPTY && 
                       hexes_dist_to_nearest_contents(self$cur_X_ind,self$cur_Y_ind,BROOD,current_hive) <= BROOD_RADIUS){
                       current_hive[self$cur_Y_ind,self$cur_X_ind,1] <- BROOD
                       current_hive[self$cur_Y_ind,self$cur_X_ind,2] <- 0
                       current_hive[self$cur_Y_ind,self$cur_X_ind,3] <- sample(1:24, 1)
                     }
                     return(current_hive)
                   }
                 )
)
