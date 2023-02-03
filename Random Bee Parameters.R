MAX_HONEY <- 25
MAX_POLLEN <- 12
MAX_BROOD <- 21

#n
QUEEN_CELLS_PER_HOUR <- sample(60:120,1)

#r_b
BROOD_RADIUS <- sample(2:4,1)
#r_n
NECTER_CONSUMP_RAD <- sample(1:4,1)

#omega
TOTAL_DAILY_HONEY <- sample(1000:4000,1)

#p_ph
POLLEN_RATIO <- runif(1,0.2,1)

#p_h
HONEY_CONSUMPTION_RATIO <- runif(1,0.9,1.1)
#p_p
POLLEN_CONSUMPTION_RATIO <- runif(1,0.9,1.1)


#k
K <- runif(1,5,20)

#Brood Heat Death Chance
BROOD_HEAT_DEATH <- runif(1,0,1)

#Worker Heat Avoidance
WORKER_HEAT_AVOIDANCE <- runif(1,0,1)

#Queen Heat Avoidance
#The times 5 is to balance it to make it the same SD on the edges as in the heat 
QUEEN_HEAT_AVOIDANCE <- runif(1,0,1)

#This is to make it from 0 to 5, and make higher heat avoidance be where the SD is smaller so heat is avoided more
QUEEN_HEAT_MOVEMENT_SD <- (1 - QUEEN_HEAT_AVOIDANCE)*5

#Only collecting during the day
HONEY_BY_HOUR <- ceiling(TOTAL_DAILY_HONEY/12)
POLLEN_BY_HOUR <- ceiling((TOTAL_DAILY_HONEY*POLLEN_RATIO)/12)

HONEY_EATEN_PER_HOUR <- ceiling((TOTAL_DAILY_HONEY*HONEY_CONSUMPTION_RATIO)/24)
POLLEN_EATEN_PER_HOUR <- ceiling((TOTAL_DAILY_HONEY*POLLEN_RATIO*POLLEN_CONSUMPTION_RATIO)/24)

