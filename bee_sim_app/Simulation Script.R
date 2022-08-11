# Simulation
# Hive levels
# 1. Contents (Honey, Pollen, Brood, Empty)
# 2. Amount (For Brood it's the age)
# 3. Brood hourly hatching cohort

runOneHour <- function(hive,queen,h){
  #This is to give the actions a random order
  # 1: Collecting Honey
  # 2: Collecting Pollen
  # 3: Eating Honey
  # 4: Eating Pollen
  # 5: Lay eggs
  # 6: Brood Hatch
  order <- 1:6 #sample(1:6,6)
  
  for(o in order){
    
    if(o == 1){
      #only collect during the day
      if(h <= 12){
        #Now add the right amount of products
        for(i in 1:HONEY_BY_HOUR){
          hive <- collect_products(hive,HONEY,MAX_HONEY)
        }
      }
    } else if(o == 2){
      #only collect during the day
      if(h <= 12){
        #Now add the right amount of products
        for(i in 1:POLLEN_BY_HOUR){
          hive <- collect_products(hive,POLLEN,MAX_POLLEN)
        }
      }
    } else if(o == 3){
    
      #Now we have bees eat
      honey_eaten <- 0
      honey_eat_attempts <- 0
      
      #We want to make sure that they don't eat more than they should
      # to keep it balanced
      while(honey_eaten < HONEY_EATEN_PER_HOUR && honey_eat_attempts < HONEY_BY_HOUR){
        eating_output <- eat_products_random(hive,HONEY)
        hive <- eating_output[[1]]
        honey_eaten <- honey_eaten + eating_output[[2]]
        honey_eat_attempts <- honey_eat_attempts + 1
      }
      
    } else if(o == 4){
      
      pollen_eaten <- 0
      pollen_eat_attempts <- 0
      
      while(pollen_eaten < POLLEN_EATEN_PER_HOUR && pollen_eat_attempts < POLLEN_BY_HOUR){
        eating_output <- eat_products_random(hive,POLLEN)
        hive <- eating_output[[1]]
        pollen_eaten <- pollen_eaten + eating_output[[2]]
        pollen_eat_attempts <- pollen_eat_attempts + 1
      }
      
    } else if(o == 5){
      #And then we have the queen lay brood
      for(i in 1:QUEEN_CELLS_PER_HOUR){
        queen$move_to_center()
        hive <- queen$lay_brood(hive)
      }
      
    } else if(o == 6){
      #Now we hatch the bees
      hive <- hatch_brood(hive,h)
    }
  }
  #age brood if h is 24
  if(h == 24){
    hive <- age_brood(hive)
  }
  
  return(hive)
}
  