# Hive Setup Functions

#To set up the hive

#"Unless specified, each model run was initiated with a completely full comb 
# with an ideal pattern of a center region of brood, surrounded by a ring of 
# pollen, and honey in all remaining cells. The assignment of type to each cell 
# is deterministic and constant across all simulations. The brood region is a
# circular disk centered in the middle of the comb with radius 18 cell lengths. 
# Around this brood region is a ring of pollen 4 cell lengths wide. The rest of 
# the comb is filled with honey."

make_set_hive <- function(){
  BROOD_RADIUS <- 18
  POLLEN_WIDTH <- 4
  
  #Second two rows are for brood honey and pollen
  set_hive <- array(c(rep(HONEY,MAX_ROWS*MAX_COLS),
                      rep(0,MAX_ROWS*MAX_COLS*2)),
                    dim = c(MAX_ROWS,MAX_COLS, 3))
  
  all_to_pollen <- hexes_in_rad(CENTER_X,CENTER_Y,BROOD_RADIUS+POLLEN_WIDTH)
  
  for(i in 1:length(all_to_pollen[[1]])){
    x <- all_to_pollen[[1]][i]
    y <- all_to_pollen[[2]][i]
    set_hive[y,x,1] <- POLLEN
  }
  
  all_to_brood <- hexes_in_rad(CENTER_X,CENTER_Y,BROOD_RADIUS)
  
  for(i in 1:length(all_to_brood[[1]])){
    x <- all_to_brood[[1]][i]
    y <- all_to_brood[[2]][i]
    set_hive[y,x,1] <- BROOD
  }
  
  for(x in 1:MAX_COLS){
    for(y in 1:MAX_ROWS){
      layer2_val <- ifelse(set_hive[y,x,1] == BROOD, sample(1:MAX_BROOD, 1),
                           ifelse(set_hive[y,x,1] == HONEY, sample(1:MAX_HONEY,1),
                                  ifelse(set_hive[y,x,1] == POLLEN, sample(1:MAX_POLLEN,1),
                                         0)))
      layer3_val <- ifelse(set_hive[y,x,1] == BROOD, sample(1:24, 1), 0)
      
      set_hive[y,x,2] <- layer2_val
      set_hive[y,x,3] <- layer3_val
    }
  }
  
  return(set_hive)
}


#This makes the nearly empty hive setup
make_empty_hive <- function(){
  BROOD_RADIUS <- 1
  
  #Second two rows are for brood honey and pollen
  set_hive <- array(c(rep(EMPTY,MAX_ROWS*MAX_COLS),
                      rep(0,MAX_ROWS*MAX_COLS*2)),
                    dim = c(MAX_ROWS,MAX_COLS, 3))
  
  all_to_brood <- hexes_in_rad(CENTER_X,CENTER_Y,BROOD_RADIUS)
  
  for(i in 1:length(all_to_brood[[1]])){
    x <- all_to_brood[[1]][i]
    y <- all_to_brood[[2]][i]
    set_hive[y,x,1] <- BROOD
  }
  
  for(x in 1:MAX_COLS){
    for(y in 1:MAX_ROWS){
      layer2_val <- ifelse(set_hive[y,x,1] == BROOD, sample(1:MAX_BROOD, 1),
                           ifelse(set_hive[y,x,1] == HONEY, sample(1:MAX_HONEY,1),
                                  ifelse(set_hive[y,x,1] == POLLEN, sample(1:MAX_POLLEN,1),
                                         0)))
      layer3_val <- ifelse(set_hive[y,x,1] == BROOD, sample(1:24, 1), 0)
      
      set_hive[y,x,2] <- layer2_val
      set_hive[y,x,3] <- layer3_val
    }
  }
  
  return(set_hive)
}

#Get whole hive brood selection probability
calc_brood_dense_prob <- function(hive_data){
  brood_dense_array <- array(0, dim = c(MAX_ROWS,MAX_COLS))
  
  hive_data <- hive
  
  for(x in 1:MAX_COLS){
    for(y in 1:MAX_ROWS){
      brood_dense_array[y,x] <- 1 + get_brood_density(x,y,hive_data)*(K-1)
    }
  }
  
  return(brood_dense_array)
}