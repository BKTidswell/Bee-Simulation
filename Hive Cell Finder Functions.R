# Minor Useful Functions

#To find the nearest hex to a point
nearest_hex <- function(xLoc,yLoc){
  hex_dist <- hexdat_centers %>% mutate(dist = round(sqrt((xLoc-x)**2+(yLoc-y)**2),2)) %>% arrange(dist)
  return(list(hex_dist$Xind[1],hex_dist$Yind[1]))
}


#To find all hexes within a radius of a point
hexes_in_rad <- function(xLoc,yLoc,rad){
  hex_dist <- hexdat_centers %>% mutate(dist = round(sqrt((xLoc-x)**2+(yLoc-y)**2),2)) %>% filter(dist <= rad)
  return(list(hex_dist$Xind,hex_dist$Yind))
}

#To get distance to nearest hex that contains specific contents
hexes_dist_to_nearest_contents <- function(xHex,yHex,content,hive_data){
  
  xLoc <- (hexdat_centers %>% filter(Xind == xHex & Yind == yHex))$x
  yLoc <- (hexdat_centers %>% filter(Xind == xHex & Yind == yHex))$y
  
  hex_dist <- hexdat_centers %>% mutate(dist = round(sqrt((xLoc-x)**2+(yLoc-y)**2),2))
  
  hive_df <- as.data.frame(as.table(hive_data[,,1:2])) %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_df) <- c("y","x","contents","amount")
  hive_df <- hive_df %>% arrange(y) %>% mutate(id = 1:(MAX_COLS*MAX_ROWS))
  
  new_hex_dist <- full_join(hex_dist,
                            hive_df %>% select(id,contents),
                            by="id") %>% filter(contents == content) %>% arrange(dist)
  
  return(new_hex_dist$dist[1])
}

#Brood Density Calculation
get_brood_density <- function(x_ind,y_ind,hive_data){
  xLoc <- x_center_mat[x_ind,y_ind]
  yLoc <- y_center_mat[x_ind,y_ind]
  
  hex_dist <- hexdat_centers %>% mutate(dist = round(sqrt((xLoc-x)**2+(yLoc-y)**2),2))
  
  #We can just cbind since the ids are all in order the whole way down
  new_hex_dist <- cbind(hex_dist,hive_data) %>% filter(contents == BROOD) %>% filter(dist <= NECTER_CONSUMP_RAD)
  
  ###Shouldn't be distance here! Should just be number of cells
  #Might need to rescale better there
  
  #distance_to_all_brood <- sum(new_hex_dist$dist)
  
  number_of_brood <- length(new_hex_dist$dist)
  
  #return(distance_to_all_brood/(3*NECTER_CONSUMP_RAD*(NECTER_CONSUMP_RAD+1)))
  return(number_of_brood/(3*NECTER_CONSUMP_RAD*(NECTER_CONSUMP_RAD+1)))
}

#Get whole hive brood selection probability
calc_brood_dense_prob <- function(hive_data){
  brood_dense_array <- array(0, dim = c(MAX_ROWS,MAX_COLS))
  
  hive_input <- as.data.frame(as.table(hive_data[,,1:2]))
  hive_input <- hive_input %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_input) <- c("y","x","contents","amount")
  hive_input <- hive_input %>% arrange(y) %>% mutate(id = 1:(MAX_COLS*MAX_ROWS)) %>% select(contents)
  
  for(x in 1:MAX_COLS){
    for(y in 1:MAX_ROWS){
      brood_dense_array[y,x] <- 1 + get_brood_density(x,y,hive_input)*(K-1)
    }
  }
  
  return(brood_dense_array)
}


#Get whole hive brood selection probability with heat
calc_brood_dense_prob_heat <- function(hive_data){
  brood_dense_array <- array(0, dim = c(MAX_ROWS,MAX_COLS))
  
  hive_input <- as.data.frame(as.table(hive_data[,,1:2]))
  hive_input <- hive_input %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_input) <- c("y","x","contents","amount")
  hive_input <- hive_input %>% arrange(y) %>% mutate(id = 1:(MAX_COLS*MAX_ROWS)) %>% select(contents)
  
  for(x in 1:MAX_COLS){
    for(y in 1:MAX_ROWS){
      #Also we multiply by heat as well. 
      #If there is heat we multiply by the worker value
      if(hive_data[y,x,4] == 1){
        brood_dense_array[y,x] <- 1 + get_brood_density(x,y,hive_input)*(K-1)
      }
      else{
        brood_dense_array[y,x] <- 1 + get_brood_density(x,y,hive_input)*(K-1)*(1-WORKER_HEAT_AVOIDANCE)
      }
      
    }
  }
  
  return(brood_dense_array)
}
