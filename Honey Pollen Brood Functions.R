# Pollen and Honey Functions

#For adding products by the worker bees

N_ATTEMPTS <- 6

collect_products <- function(hive_data,product,product_max){
  for(i in 1:N_ATTEMPTS){
    rand_x <- sample(1:MAX_COLS,1)
    rand_y <- sample(1:MAX_ROWS,1)
    
    if(hive_data[rand_y,rand_x,1] == product && hive_data[rand_y,rand_x,2] < product_max){
      hive_data[rand_y,rand_x,2] <- hive_data[rand_y,rand_x,2] + 1
      break
    } else if(hive_data[rand_y,rand_x,1] == EMPTY){
      hive_data[rand_y,rand_x,1] <- product
      hive_data[rand_y,rand_x,2] <- 1
      break
    }
    
  }
  return(hive_data)
}

#Collecting products but with heat

collect_products_heat <- function(hive_data,product,product_max){
  for(i in 1:N_ATTEMPTS){
    rand_x <- sample(1:MAX_COLS,1)
    rand_y <- sample(1:MAX_ROWS,1)
    
    #If the place has heat and you "roll" under work avoidence then try again
    if((hive_data[rand_y,rand_x,4] == 1) && (runif(1) <= WORKER_HEAT_AVOIDANCE)){
      break
    } else{
      #Otherwise put things there as normal
      if(hive_data[rand_y,rand_x,1] == product && hive_data[rand_y,rand_x,2] < product_max){
        hive_data[rand_y,rand_x,2] <- hive_data[rand_y,rand_x,2] + 1
        break
      } else if(hive_data[rand_y,rand_x,1] == EMPTY){
        hive_data[rand_y,rand_x,1] <- product
        hive_data[rand_y,rand_x,2] <- 1
        break
      }
    }
    
  }
  return(hive_data)
}

#For Eating Pollen and Honey

#Here k is the Ratio of honey/pollen taken from cells fully surrounded by brood cells to honey/pollen taken from cells with no brood neighbors (dimensionless)

eat_products <- function(hive_data,product){
  amount_to_eat <- 0
  
  for(i in 1:N_ATTEMPTS){
    rand_x <- sample(1:MAX_COLS,1)
    rand_y <- sample(1:MAX_ROWS,1)
    
      if(hive_data[rand_y,rand_x,1] == product){
        amount_to_eat <- min(hive_data[rand_y,rand_x,2], 1 + get_brood_density(rand_x,rand_y,hive_data)*(K-1))
        
        hive_data[rand_y,rand_x,2] <- hive_data[rand_y,rand_x,2] - amount_to_eat
        if(hive_data[rand_y,rand_x,2] == 0){
          hive_data[rand_y,rand_x,1] <- EMPTY
        }
        break
      }
  }
  
  return(list(hive_data,amount_to_eat))
}


#For Eating Pollen and Honey (Model 2)

eat_products_m2 <- function(hive_data,prob_array,product){
  amount_to_eat <- 0
  
  for(i in 1:N_ATTEMPTS){
    
    id <- sample(array(1:(MAX_COLS*MAX_ROWS), dim = c(MAX_ROWS,MAX_COLS)),1,prob = prob_array)
    Xind <- ceiling(id/MAX_ROWS)
    Yind <- ((id-1)%%MAX_ROWS)+1
    
    if(hive_data[Yind,Xind,1] == product){
      amount_to_eat <- 1
      
      hive_data[Yind,Xind,2] <- hive_data[Yind,Xind,2] - amount_to_eat
      if(hive_data[Yind,Xind,2] == 0){
        hive_data[Yind,Xind,1] <- EMPTY
      }
      break
    }
  }
  return(list(hive_data,amount_to_eat))
}


#For Eating Pollen and Honey with Head (Using Model 2)

eat_products_heat <- function(hive_data,prob_array,product){
  amount_to_eat <- 0
  
  for(i in 1:N_ATTEMPTS){
    
    id <- sample(array(1:(MAX_COLS*MAX_ROWS), dim = c(MAX_ROWS,MAX_COLS)),1,prob = prob_array)
    Xind <- ceiling(id/MAX_ROWS)
    Yind <- ((id-1)%%MAX_ROWS)+1
    
    #If the place has heat and you "roll" under work avoidence then try again
    if((hive_data[Yind,Xind,4] == 1) && (runif(1) <= WORKER_HEAT_AVOIDANCE)){
      break
    } else{
      if(hive_data[Yind,Xind,1] == product){
        amount_to_eat <- 1
        hive_data[Yind,Xind,2] <- hive_data[Yind,Xind,2] - amount_to_eat
        if(hive_data[Yind,Xind,2] == 0){
          hive_data[Yind,Xind,1] <- EMPTY
        }
        break
      }
    }
  }
  
  return(list(hive_data,amount_to_eat))
}


#Random Feeding 1 at a Time
eat_products_random <- function(hive_data,product){
  amount_to_eat <- 1
  
  for(i in 1:N_ATTEMPTS){
    rand_x <- sample(1:MAX_COLS,1)
    rand_y <- sample(1:MAX_ROWS,1)
    
    if(hive_data[rand_y,rand_x,1] == product){
      hive_data[rand_y,rand_x,2] <- hive_data[rand_y,rand_x,2] - amount_to_eat
      if(hive_data[rand_y,rand_x,2] == 0){
        hive_data[rand_y,rand_x,1] <- EMPTY
      }
      break
    }
  }
  return(list(hive_data,amount_to_eat))
}


# Brood Functions

#For Brood Aging

# We don't hatch them here, we hatch them hourly
age_brood <- function(hive_data){
  #Get the x and y of all brood
  brood_xs <- data.frame(which(hive_data[,,1] == BROOD,arr.ind = TRUE))$col
  brood_ys <- data.frame(which(hive_data[,,1] == BROOD,arr.ind = TRUE))$row
  
  #Okay if there aren't any brood don't try to age them
  if(length(brood_xs) > 0){
    
    for(i in 1:length(brood_xs)){
      hive_data[brood_ys[i],brood_xs[i],2] <- hive_data[brood_ys[i],brood_xs[i],2] + 1
    }
    
  }
  
  return(hive_data)
}

#Here we also check heat first to see if they die
age_brood_heat <- function(hive_data){
  #Get the x and y of all brood
  brood_xs <- data.frame(which(hive_data[,,1] == BROOD,arr.ind = TRUE))$col
  brood_ys <- data.frame(which(hive_data[,,1] == BROOD,arr.ind = TRUE))$row
  
  #Okay if there aren't any brood don't try to age them
  if(length(brood_xs) > 0){
  
    for(i in 1:length(brood_xs)){
      hive_data[brood_ys[i],brood_xs[i],2] <- hive_data[brood_ys[i],brood_xs[i],2] + 1
      
      #Here we remove them from heat if they "roll" under the heat death chance
      if((hive_data[brood_ys[i],brood_xs[i],1] == 1) && (runif(1) <= BROOD_HEAT_DEATH)){
        hive_data[brood_ys[i],brood_xs[i],1] <- EMPTY
        hive_data[brood_ys[i],brood_xs[i],2] <- 0
        hive_data[brood_ys[i],brood_xs[i],3] <- 0
      }
    }
    
  }
  
  return(hive_data)
}

#Determines brood hatching

hatch_brood <- function(hive_data,hour){
  hatch_x <- data.frame(which(hive_data[,,1] == BROOD & hive_data[,,2] >= MAX_BROOD & hive_data[,,3] == hour,arr.ind = TRUE))$col
  hatch_y <- data.frame(which(hive_data[,,1] == BROOD & hive_data[,,2] >= MAX_BROOD & hive_data[,,3] == hour,arr.ind = TRUE))$row
  
  #Hatch all brood that hatch this hour
  for(i in 1:length(hatch_x)){
    hive_data[hatch_y[i],hatch_x[i],1] <- EMPTY
    hive_data[hatch_y[i],hatch_x[i],2] <- 0
    hive_data[hatch_y[i],hatch_x[i],3] <- 0
  }
  
  return(hive_data)
}
