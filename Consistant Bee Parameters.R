MAX_HONEY <- 25
MAX_POLLEN <- 12
MAX_BROOD <- 21

#Choosing Parameters here based on what ended up working well.

#n
QUEEN_CELLS_PER_HOUR <- 90 #sample(60:120,1)

#r_b
BROOD_RADIUS <- 2 #sample(2:4,1)
#r_n
NECTER_CONSUMP_RAD <- 1 #sample(1:4,1)

#omega
TOTAL_DAILY_HONEY <- 2000 #sample(1000:4000,1)

#p_ph
POLLEN_RATIO <- 0.25 #runif(1,0.2,1)

#p_h
HONEY_CONSUMPTION_RATIO <- 1.05 #runif(1,0.9,1.1)
#p_p
POLLEN_CONSUMPTION_RATIO <- 1.05 #runif(1,0.9,1.1)


#k
K <- 17 #runif(1,5,20)

#Now for these we'll sample a smaller set of more interesting point

#Brood Heat Death Chance
BROOD_HEAT_DEATH <- sample(c(0.1,0.25,0.5,0.75),1) #runif(1,0,1)

#Worker Heat Avoidance
WORKER_HEAT_AVOIDANCE <- sample(c(0.1,0.25,0.4,0.5,0.75),1) #runif(1,0,1)

#Queen Heat Avoidance
#The times 5 is to balance it to make it the same SD on the edges as in the heat 
QUEEN_HEAT_AVOIDANCE <- sample(c(0.1,0.25,0.4,0.5,0.75),1) #runif(1,0,1)

#This is to make it from 0 to 5, and make higher heat avoidance be where the SD is smaller so heat is avoided more
QUEEN_HEAT_MOVEMENT_SD <- (1 - QUEEN_HEAT_AVOIDANCE)*5

#Only collecting during the day
HONEY_BY_HOUR <- ceiling(TOTAL_DAILY_HONEY/12)
POLLEN_BY_HOUR <- ceiling((TOTAL_DAILY_HONEY*POLLEN_RATIO)/12)

HONEY_EATEN_PER_HOUR <- ceiling((TOTAL_DAILY_HONEY*HONEY_CONSUMPTION_RATIO)/24)
POLLEN_EATEN_PER_HOUR <- ceiling((TOTAL_DAILY_HONEY*POLLEN_RATIO*POLLEN_CONSUMPTION_RATIO)/24)

