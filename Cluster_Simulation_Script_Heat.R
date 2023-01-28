library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(reshape2)
library(R6)
library(foreach)
library(doParallel)
library(parallel)

# test if there is at least one argument: if not, return an error
args = commandArgs(trailingOnly=TRUE)
print(args)

#trial types are "worker", "queen", "brood"

if (length(args)<2) {
  stop("At least two argument must be supplied", call.=FALSE)
} else if (length(args)==2) {
  # default output file
  trial_add <- as.numeric(args[1])
  trial_type <- args[2]
  
  if(!(trial_type %in% c("worker","queen","brood","none"))){
    stop("Trial type must be worker, queen, brood, or none", call.=FALSE)
  }
}

#Define stuff
BROOD <- 1
POLLEN <- 2
HONEY <- 3 
EMPTY <- 0

MAX_ROWS <- 75
MAX_COLS <- 45

print(trial_add)

#Load all functions
source("Graphing Functions.R")
source("Hive Cell Finder Functions.R")
source("Hive Setup Functions.R")
source("Honey Pollen Brood Functions.R")
source("Queen Def.R")
source("Final Measures.R")

# Simulation
# Hive levels
# 1. Contents (Honey, Pollen, Brood, Empty)
# 2. Amount (For Brood it's the age)
# 3. Brood hourly hatching cohort

N_TRIALS <- 40
N_DAYS <- 30

#To keep track of the data overall
parameter_df <- tibble(trial_n = 0,
                       type = trial_type,
                       days = 0,
                       n = 0,
                       rb = 0,
                       rn = 0,
                       w = 0,
                       pph = 0,
                       ph = 0,
                       pp = 0,
                       k = 0,
                       pBrood = 0,
                       pHoney = 0,
                       pPollen = 0,
                       pEmpty = 0,
                       pBroodHeat = 0,
                       pHoneyHeat = 0,
                       pPollenHeat = 0,
                       pEmptyHeat = 0,
                       broodMetric = 0,
                       pollenRing = 0)

numCores <- detectCores()

registerDoParallel(numCores)

print(numCores)

parameter_df <- foreach(trial = 1:N_TRIALS, .combine='rbind') %dopar%{
  source("Random Bee Parameters.R")
  
  #hive <- make_empty_hive()
  hive <- make_set_hive()
  
  queen <- Queen$new(median(hexdat_centers$x),median(hexdat_centers$y))
  
  #Start up the count
  Count_Contents <- tibble(Brood = length(which(hive[,,1] == BROOD)),
                           Honey = length(which(hive[,,1] == HONEY)),
                           Pollen = length(which(hive[,,1] == POLLEN)),
                           Empty = length(which(hive[,,1] == EMPTY)))
  
  for(d in 1:N_DAYS){
    print(paste0("Day ",d))
    
    #24 hours in a day
    for(h in 1:24){
      
      #This is to give the actions a random order
      # 1: Collecting Honey
      # 2: Collecting Pollen
      # 3: Eating Honey
      # 4: Eating Pollen
      # 5: Lay eggs
      # 6: Brood Hatch
      order <- sample(1:6,6)
      
      for(o in order){
        
        if(o == 1){
          #only collect during the day
          if(h <= 12){
            #Now add the right amount of products
            for(i in 1:HONEY_BY_HOUR){
              
              if(trial_type == "worker"){
                hive <- collect_products_heat(hive,POLLEN,MAX_POLLEN)  
              }else{
                hive <- collect_products(hive,HONEY,MAX_HONEY) 
              }
              
            }
          }
        } else if(o == 2){
          #only collect during the day
          if(h <= 12){
            #Now add the right amount of products
            for(i in 1:POLLEN_BY_HOUR){
              
              if(trial_type == "worker"){
                hive <- collect_products_heat(hive,POLLEN,MAX_POLLEN)  
              }else{
                hive <- collect_products(hive,HONEY,MAX_HONEY) 
              }
              
            }
          }
        } else if(o == 3){
          
          brood_density_prob_array <- calc_brood_dense_prob(hive)
          
          #Now we have bees eat
          honey_eaten <- 0
          honey_eat_attempts <- 0
          
          #We want to make sure that they don't eat more than they should
          # to keep it balanced
          while(honey_eaten < HONEY_EATEN_PER_HOUR && honey_eat_attempts < HONEY_BY_HOUR){
            eating_output <- eat_products_heat(hive,brood_density_prob_array,HONEY)
            hive <- eating_output[[1]]
            honey_eaten <- honey_eaten + eating_output[[2]]
            honey_eat_attempts <- honey_eat_attempts + 1
          }
          
        } else if(o == 4){
          
          brood_density_prob_array <- calc_brood_dense_prob(hive)
          
          pollen_eaten <- 0
          pollen_eat_attempts <- 0
          
          while(pollen_eaten < POLLEN_EATEN_PER_HOUR && pollen_eat_attempts < POLLEN_BY_HOUR){
            eating_output <- eat_products_heat(hive,brood_density_prob_array,POLLEN)
            hive <- eating_output[[1]]
            pollen_eaten <- pollen_eaten + eating_output[[2]]
            pollen_eat_attempts <- pollen_eat_attempts + 1
          }
          
        } else if(o == 5){
          #And then we have the queen lay brood
          for(i in 1:QUEEN_CELLS_PER_HOUR){
            
            if(trial_type == "queen"){
              queen$move_from_heat()
            }else{
              queen$move_to_center()
            }
            
            hive <- queen$lay_brood(hive)
          }
          
        } else if(o == 6){
          #Now we hatch the bees
          hive <- hatch_brood(hive,h)
        }
        
      }
    }
    
    if(trial_type == "brood"){
      hive <- age_brood_heat(hive)         
    }else{
      hive <- age_brood(hive)       
    }
    
  }
  
  data_out <- tibble(trial_n = trial,
                     type = trial_type,
                     days = N_DAYS,
                     n = QUEEN_CELLS_PER_HOUR,
                     rb = BROOD_RADIUS,
                     rn = NECTER_CONSUMP_RAD,
                     w = TOTAL_DAILY_HONEY,
                     pph = round(POLLEN_RATIO,4),
                     ph = round(HONEY_CONSUMPTION_RATIO,4),
                     pp = round(POLLEN_CONSUMPTION_RATIO,4),
                     k = round(K,4),
                     pBrood = length(which(hive[,,1] == BROOD))/(MAX_ROWS*MAX_COLS),
                     pHoney = length(which(hive[,,1] == HONEY))/(MAX_ROWS*MAX_COLS),
                     pPollen = length(which(hive[,,1] == POLLEN))/(MAX_ROWS*MAX_COLS),
                     pEmpty = length(which(hive[,,1] == EMPTY))/(MAX_ROWS*MAX_COLS),
                     pBroodHeat = get_percent_in_heat(hive,BROOD),
                     pHoneyHeat = get_percent_in_heat(hive,HONEY),
                     pPollenHeat = get_percent_in_heat(hive,POLLEN),
                     pEmptyHeat = get_percent_in_heat(hive,EMPTY),
                     broodMetric = brood_metric(hive),
                     pollenRing = pollen_ring_metric(hive))
  
  write.csv(data_out,paste0("hive_data/Beehive Data Out Trial ",str_pad(trial + trial_add, 5, pad = "0"),"_",trial_type,".csv"))
  
  ggsave(paste0("hive_plots/hiveplot_",str_pad((d-1)*24+h, 5, pad = "0"),
                "_D",str_pad(d, 3, pad = "0"),
                "_T",str_pad(trial + trial_add, 5, pad = "0"),".pdf"),
         hive_matrix_to_graph(hive,queen),width=7, height=7)
  
  data_out
}

write.csv(parameter_df,paste0("Beehive Data Out ",Sys.time(),".csv"))



