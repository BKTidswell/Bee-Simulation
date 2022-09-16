

mat <- matrix(1:25, nrow = 5)
mat

as.table(mat)
as.data.frame(mat)
as.data.frame(as.table(mat))

row.names(mat) = seq(1:nrow(mat))
colnames(mat) = seq(1:ncol(mat))

data.frame(i=rep(row.names(mat),ncol(mat)),
           j=rep(colnames(mat),each=nrow(mat)),
           score=as.vector(mat))

hive <- make_set_hive()

hive
as.table(hive[,,1:2])
as.data.frame(hive[,,1:2])

hive_df <- as.data.frame(as.table(hive[,,1:2]))
hive_df <- hive_df %>% pivot_wider(names_from = Var3, values_from = Freq)

system.time({
  hive_df <- as.data.frame(as.table(hive[,,1:2]))
  hive_df <- hive_df %>% pivot_wider(names_from = Var3, values_from = Freq)
})


#This is faster
system.time({
row.names(hive) = seq(1:nrow(hive))
colnames(hive) = seq(1:ncol(hive))
hive_df_2 <- data.frame(Var1=rep(row.names(hive[,,1:2]),ncol(hive[,,1:2])),
           Var2=rep(colnames(hive[,,1:2]),each=nrow(hive[,,1:2])),
           Var3=c(rep("A",MAX_ROWS*MAX_COLS),rep("B",MAX_ROWS*MAX_COLS)),
           Freq=as.vector(hive[,,1:2]))
hive_df_2 <- reshape(hive_df_2, dir = "wide", idvar = c("Var1","Var2"), timevar = "Var3")
})

reshape(hive_df_2, direction = "wide")

reshape(hive_df_2, dir = "wide", idvar = c("Var1","Var2"), timevar = "Var3")

x_ind <- 22
y_ind <- 22

# user  system elapsed 
# 1.611   0.016   1.626 
system.time({
  replicate(100, get_brood_density_old(22,22,hive))
})


# user  system elapsed 
# 0.397   0.001   0.397
source("Hive Cell Finder Functions.R")
system.time({
  hive_input <- as.data.frame(as.table(hive[,,1:2]))
  hive_input <- hive_input %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_input) <- c("y","x","contents","amount")
  hive_input <- hive_input %>% arrange(y) %>% mutate(id = 1:(MAX_COLS*MAX_ROWS)) %>% select(contents)
  replicate(100, get_brood_density(22,22,hive_input))
})

#With New
#   user  system elapsed 
# 13.492   0.043  13.531

#With Old
# user  system elapsed 
# 55.785   0.280  56.099
system.time({
  calc_brood_dense_prob(hive)
})

x_center_mat <- hexdat_centers %>% select(x,Xind,Yind) %>% 
                                   pivot_wider(names_from = Yind, values_from = x) %>% 
                                   select(-Xind) %>% 
                                   as.matrix()

y_center_mat <- hexdat_centers %>% select(y,Xind,Yind) %>% 
  pivot_wider(names_from = Yind, values_from = y) %>% 
  select(-Xind) %>% 
  as.matrix()

x_center_mat[20,10]
y_center_mat[20,10]

(hexdat_centers %>% filter(Xind == 20, Yind == 10))$x
(hexdat_centers %>% filter(Xind == 20, Yind == 10))$y

