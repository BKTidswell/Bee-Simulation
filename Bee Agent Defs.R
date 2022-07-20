#Defines what a bee is 

library(R6)
source("Bee Parameters.R")

# Bee Definition
Bee <- R6Class("Bee",
               public = list(
                 cur_X = NULL,
                 cur_Y = NULL,
                 prev_X = -1,
                 prev_Y = -1,
                 honey_to_deposit = HONEY_BY_HOUR_BY_BEE,
                 pollen_to_deposit = POLLEN_BY_HOUR_BY_BEE,
                 honey_to_eat = EATEN_HONEY_BY_HOUR_BY_BEE,
                 pollen_to_eat = EATEN_POLLEN_BY_HOUR_BY_BEE,
                 initialize = function(cur_X = NA, cur_Y = NA) {
                   self$cur_X <- cur_X
                   self$cur_Y <- cur_Y
                 },
                 move = function(){
                   next_hex <- hex_move(self$cur_X,self$cur_Y,self$prev_X,self$prev_Y)
                   
                   self$prev_X <- self$cur_X
                   self$prev_Y <- self$cur_Y
                   
                   self$cur_X <- next_hex[1]
                   self$cur_Y <- next_hex[2]
                   
                   #print(c(self$cur_X,self$cur_Y))
                 },
                 interact_with_comb = function(current_hive){
                   curr_fill <- current_hive[self$cur_X,self$cur_Y,1]
                   curr_val <- current_hive[self$cur_X,self$cur_Y,2]
                   
                   #If you end up on an empy hive
                   if(curr_fill == EMPTY){
                     #randomly select what to add, biased by the pollen ratio
                     what_to_add <- sample(c(HONEY,POLLEN),1,prob = c(1,POLLEN_RATIO))
                     
                     #Deposit only if you have something to deposit
                     if(what_to_add == HONEY && self$honey_to_deposit > 0){
                       #Can't go over max!
                       if(self$honey_to_deposit < MAX_HONEY){
                         current_hive[self$cur_X,self$cur_Y,1] <- HONEY
                         current_hive[self$cur_X,self$cur_Y,2] <- self$honey_to_deposit
                         #Set your own honey to 0
                         self$honey_to_deposit <- 0
                       } else{
                         current_hive[self$cur_X,self$cur_Y,1] <- HONEY
                         current_hive[self$cur_X,self$cur_Y,2] <- MAX_HONEY
                         #Subtract max from honey
                         self$honey_to_deposit <- self$honey_to_deposit - MAX_HONEY
                       }
                     } else if (what_to_add == POLLEN && self$pollen_to_deposit > 0){
                       #Can't go over max!
                       if(self$pollen_to_deposit < MAX_POLLEN){
                         current_hive[self$cur_X,self$cur_Y,1] <- POLLEN
                         current_hive[self$cur_X,self$cur_Y,2] <- self$pollen_to_deposit
                         #Set your own honey to 0
                         self$pollen_to_deposit <- 0
                       } else{
                         current_hive[self$cur_X,self$cur_Y,1] <- POLLEN
                         current_hive[self$cur_X,self$cur_Y,2] <- MAX_POLLEN
                         #Subtract max from honey
                         self$pollen_to_deposit <- self$pollen_to_deposit - MAX_POLLEN
                       }
                     }
                     
                     # If you land on honey...
                   } else if(curr_fill == HONEY){
                     #If you have honey to deposit do to, if it isn't full
                     if (self$honey_to_deposit > 0 && curr_val < MAX_HONEY){
                       #If you can add it all do it, otherwise only add part
                       if (curr_val + self$honey_to_deposit <= MAX_HONEY){
                         current_hive[self$cur_X,self$cur_Y,2] <- curr_val + self$honey_to_deposit
                         self$honey_to_deposit <- 0
                       } else{
                         current_hive[self$cur_X,self$cur_Y,2] <- MAX_HONEY
                         self$honey_to_deposit <- self$honey_to_deposit - (MAX_HONEY - curr_val)
                       }
                       # You are done putting it away then you can take
                     } else if(self$honey_to_deposit == 0 && self$honey_to_eat > 0){
                       # If you can eat enough do it otherwise only eat part
                       if (curr_val >= self$honey_to_eat){
                         current_hive[self$cur_X,self$cur_Y,2] <- curr_val - self$honey_to_eat
                         self$honey_to_eat <- 0
                         #If there is 0 left change to empty
                         if (curr_val - self$honey_to_eat == 0){
                           current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                         }
                       } else{
                         #If there isn't enough set it to empty and eat all you can
                         current_hive[self$cur_X,self$cur_Y,2] <- 0
                         current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                         self$honey_to_eat <- self$honey_to_eat - curr_val
                       }
                     }
                   } else if(curr_fill == POLLEN){
                     #If you have pollen to deposit do to, if it isn't full
                     if (self$pollen_to_deposit > 0 && curr_val < MAX_POLLEN){
                       #If you can add it all do it, otherwise only add part
                       if (curr_val + self$pollen_to_deposit <= MAX_POLLEN){
                         current_hive[self$cur_X,self$cur_Y,2] <- curr_val + self$pollen_to_deposit
                         self$pollen_to_deposit <- 0
                       } else{
                         current_hive[self$cur_X,self$cur_Y,2] <- MAX_POLLEN
                         self$pollen_to_deposit <- self$pollen_to_deposit - (MAX_POLLEN - curr_val)
                       }
                       # You are done putting it away then you can take
                     } 
                     # Pollen is only for brood!
                     #
                     # else if(self$pollen_to_deposit == 0 && self$pollen_to_eat > 0){
                     #   # If you can eat enough do it otherwise only eat part
                     #   if (curr_val >= self$pollen_to_eat){
                     #     current_hive[self$cur_X,self$cur_Y,2] <- curr_val - self$pollen_to_eat
                     #     self$pollen_to_eat <- 0
                     #     #If there is 0 left change to empty
                     #     if (curr_val - self$pollen_to_eat == 0){
                     #       current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                     #     }
                     #   } else{
                     #     #If there isn't enough set it to empty and eat all you can
                     #     current_hive[self$cur_X,self$cur_Y,2] <- 0
                     #     current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                     #     self$pollen_to_eat <- self$pollen_to_eat - curr_val
                     #   }
                     # }
                   }
                   return(current_hive)
                 },
                 buzz = function(){
                   cat(paste0("Hello, I am a bee! I have:\n",
                              self$honey_to_deposit, " Honey \n",
                              self$pollen_to_deposit, " Pollen \n\n",
                              "I want to eat:\n",
                              self$honey_to_eat, " Honey \n",
                              self$pollen_to_eat, " Pollen \n\n",
                              "I am at: X = ", self$cur_X, " and Y = ", self$cur_Y, ".\n"))
                 }
               )
)

# Nurse Definition
Nurse <- R6Class("Nurse",
               public = list(
                 cur_X = NULL,
                 cur_Y = NULL,
                 prev_X = -1,
                 prev_Y = -1,
                 honey_for_brood = BROOD_EATEN_HONEY_BY_HOUR_BY_BEE,
                 pollen_for_brood = BROOD_EATEN_POLLEN_BY_HOUR_BY_BEE,
                 initialize = function(cur_X = NA, cur_Y = NA) {
                   self$cur_X <- cur_X
                   self$cur_Y <- cur_Y
                 },
                 move = function(){
                   next_hex <- hex_move(self$cur_X,self$cur_Y,self$prev_X,self$prev_Y)
                   
                   self$prev_X <- self$cur_X
                   self$prev_Y <- self$cur_Y
                   
                   self$cur_X <- next_hex[1]
                   self$cur_Y <- next_hex[2]
                   
                   #print(c(self$cur_X,self$cur_Y))
                 },
                 interact_with_comb = function(current_hive){
                   curr_fill <- current_hive[self$cur_X,self$cur_Y,1]
                   curr_val <- current_hive[self$cur_X,self$cur_Y,2]
                   
                   #If you end up on honey deposit
                   if(curr_fill == HONEY){
                     #Take some honey for the brood
                     if (self$honey_for_brood > 0){
                       # If you can take enough do it otherwise only take part
                       if (curr_val >= self$honey_for_brood){
                         current_hive[self$cur_X,self$cur_Y,2] <- curr_val - self$honey_for_brood
                         self$honey_for_brood <- 0
                         #If there is 0 left change to empty
                         if (curr_val - self$honey_for_brood == 0){
                           current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                         }
                       } else{
                         #If there isn't enough set it to empty and take all you can
                         current_hive[self$cur_X,self$cur_Y,2] <- 0
                         current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                         self$honey_for_brood <- self$honey_for_brood - curr_val
                       }
                     }
                   } else if(curr_fill == POLLEN){
                     #If you have pollen to take still go do it
                     if (self$pollen_for_brood > 0){
                       # If you can take enough do it otherwise only take part
                       if (curr_val >= self$pollen_for_brood){
                         current_hive[self$cur_X,self$cur_Y,2] <- curr_val - self$pollen_for_brood
                         self$pollen_for_brood <- 0
                         #If there is 0 left change to empty
                         if (curr_val - self$pollen_for_brood == 0){
                           current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                         }
                       } else{
                         #If there isn't enough set it to empty and take all you can
                         current_hive[self$cur_X,self$cur_Y,2] <- 0
                         current_hive[self$cur_X,self$cur_Y,1] <- EMPTY
                         self$pollen_for_brood <- self$pollen_for_brood - curr_val
                       }
                     }
                   } else if(curr_fill == BROOD){
                     #Later we might keep track, but for now we'll just say feed it and get more 
                     self$honey_for_brood <- BROOD_EATEN_HONEY_BY_HOUR_BY_BEE
                     self$pollen_for_brood <- BROOD_EATEN_POLLEN_BY_HOUR_BY_BEE
                   }
                   return(current_hive)
                 },
                 buzz = function(){
                   cat(paste0("Hello, I am a nurse! I have:\n",
                              "I want to gather for brood:\n",
                              self$honey_for_brood, " Honey \n",
                              self$pollen_for_brood, " Pollen \n\n",
                              "I am at: X = ", self$cur_X, " and Y = ", self$cur_Y, ".\n"))
                 }
               )
)

# Nurse Definition
Queen <- R6Class("Queen",
                 public = list(
                   cur_X = NULL,
                   cur_Y = NULL,
                   prev_X = -1,
                   prev_Y = -1,
                   cells_per_10 = ceiling(QUEEN_CELLS_PER_HOUR/10),
                   initialize = function(cur_X = NA, cur_Y = NA) {
                     self$cur_X <- cur_X
                     self$cur_Y <- cur_Y
                   },
                   move = function(){
                     next_hex <- hex_move(self$cur_X,self$cur_Y,self$prev_X,self$prev_Y)
                     
                     self$prev_X <- self$cur_X
                     self$prev_Y <- self$cur_Y
                     
                     self$cur_X <- next_hex[1]
                     self$cur_Y <- next_hex[2]
                     
                     #print(c(self$cur_X,self$cur_Y))
                   },
                   interact_with_comb = function(current_hive){
                     curr_fill <- current_hive[self$cur_X,self$cur_Y,1]
                     curr_val <- current_hive[self$cur_X,self$cur_Y,2]
                     
                     #If you end up on an empty hive
                     if(curr_fill == EMPTY){
                       current_hive[self$cur_X,self$cur_Y,1] <- BROOD
                       current_hive[self$cur_X,self$cur_Y,2] <- 0
                     }
                     return(current_hive)
                   },
                   buzz = function(){
                     cat(paste0("Hello, I am a nurse! I have:\n",
                                "I want to gather for brood:\n",
                                self$honey_for_brood, " Honey \n",
                                self$pollen_for_brood, " Pollen \n\n",
                                "I am at: X = ", self$cur_X, " and Y = ", self$cur_Y, ".\n"))
                   }
                 )
)


