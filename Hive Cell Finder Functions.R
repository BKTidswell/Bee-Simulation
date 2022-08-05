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
  xLoc <- (hexdat_centers %>% filter(Xind == x_ind, Yind == y_ind))$x
  yLoc <- (hexdat_centers %>% filter(Xind == x_ind, Yind == y_ind))$y
  
  hex_dist <- hexdat_centers %>% mutate(dist = round(sqrt((xLoc-x)**2+(yLoc-y)**2),2))
  
  hive_df <- as.data.frame(as.table(hive_data[,,1:2])) %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_df) <- c("y","x","contents","amount")
  hive_df <- hive_df %>% arrange(y) %>% mutate(id = 1:(MAX_COLS*MAX_ROWS))
  
  new_hex_dist <- full_join(hex_dist,
                            hive_df %>% select(id,contents),
                            by="id") %>% filter(contents == BROOD) %>% filter(dist < NECTER_CONSUMP_RAD)
  
  distance_to_all_brood <- sum(new_hex_dist$dist)
  
  return(distance_to_all_brood/(3*NECTER_CONSUMP_RAD*(NECTER_CONSUMP_RAD+1)))
}
