
#Find number of surrounding brood to a cell
n_surrounding_brood <- function(xLocs,yLocs,hive_data){
  
  n_broods <- rep(NA,length(xLocs))
  
  for(i in 1:length(xLocs)){
    #Stats negative since this also counts the hex that is there already, which is brood
    count_brood <- -1
    surrounding_hexes <- hexes_in_rad(xLocs[i],yLocs[i],1)
  
    for(j in 1:length(surrounding_hexes[[1]])){
      if(hive_data[surrounding_hexes[[2]][j],surrounding_hexes[[1]][j],1] == BROOD){
        count_brood <- count_brood + 1
      }
    }
  
    n_broods[i] <- count_brood
  }
  
  return(n_broods)
}

#Find all brood cells
get_all_content <- function(hive_data,content){
  hive_df <- as.data.frame(as.table(hive_data[,,1:2])) %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_df) <- c("y","x","contents","amount")
  hive_df <- hive_df %>% arrange(y) %>% mutate(id = 1:(MAX_COLS*MAX_ROWS))
  
  brood_df <- full_join(hexdat_centers,
                        hive_df %>% select(id,contents),
                        by="id") %>% filter(contents == content)
  
  return(brood_df)
}

#vectorized distance to nearest
vect_hexes_dist_to_nearest_contents <- function(xHexs,yHexs,content,hive_data){
  dist_honey_brood <- rep(NA,length(xHexs))
  
  for(i in 1:length(xHexs)){
    dist_honey_brood[i] <- hexes_dist_to_nearest_contents(xHexs[i],yHexs[i],content,hive_data)
  }
  
  return(dist_honey_brood)
}

#Gets the mean number of brood around other brood
brood_metric <- function(hive_data){
  brood_dense_df <- get_all_content(hive_data,BROOD) %>% mutate(n_brood = n_surrounding_brood(x,y,hive_data))
  
  return(mean(brood_dense_df$n_brood))
}

#Gets the mean smallest distance from honey to a brood
pollen_ring_metric <- function(hive_data){
  pollen_ring_df <- get_all_content(hive_data,HONEY) %>% mutate(dist_honey_brood = vect_hexes_dist_to_nearest_contents(Xind,Yind,BROOD,hive_data))
  
  return(mean(pollen_ring_df$dist_honey_brood))
}

#Get the percent of contents in the heated area
get_percent_in_heat <- function(hive_data,contents){
  HEAT_RADIUS <- 10
  
  all_in_heat <- hexes_in_rad(HEAT_CENTER_X,HEAT_CENTER_Y,HEAT_RADIUS)
  
  count <- 0
  
  for(i in 1:length(all_in_heat[[1]])){
    x <- all_in_heat[[1]][i]
    y <- all_in_heat[[2]][i]
    if(hive_data[y,x,1] == contents){
      count <- count+1
    }
  }
  
  return(count/length(all_in_heat[[1]]))
}

