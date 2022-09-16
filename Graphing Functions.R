
# Makes the beehive graph framework
# https://stackoverflow.com/questions/24006361/plot-series-of-filled-hexagons-in-ggplot2

types <- c("Brood", "Pollen", "Honey", "Empty")
#hive_colors <- c("#F3DAB1","#FFF966","#DE620B","#230F11")
hive_colors <- c(Brood = "#FF0000", Pollen = "#00FF00", Honey = "#0000FF", Empty = "#000000")

hex_size <- 1

hex_center_xs <- rep_len(c(seq(from = 0, by = hex_size, length.out = MAX_COLS),
                           seq(from = hex_size/2, by = hex_size, length.out = MAX_COLS)), MAX_ROWS*MAX_COLS)

hex_center_ys <- sort(c(rep(seq(from = 0, by = hex_size*sqrt(3), length.out=ceiling(MAX_ROWS/2)), MAX_COLS),
                        rep(seq(from = hex_size*sqrt(3)/2, by = hex_size*sqrt(3), length.out=floor(MAX_ROWS/2)), MAX_COLS)),decreasing = TRUE)

hex_x = cbind(hex_center_xs + 0, hex_center_xs + 0.5, hex_center_xs + 0.5,
              hex_center_xs + 0, hex_center_xs - 0.5, hex_center_xs - 0.5) 
hex_y = cbind(hex_center_ys - 1/(sqrt(3)), hex_center_ys - 1/(2*sqrt(3)), hex_center_ys + 1/(2*sqrt(3)),
              hex_center_ys + 1/(sqrt(3)), hex_center_ys + 1/(2*sqrt(3)), hex_center_ys - 1/(2*sqrt(3)))

hexdat_x <- melt(cbind(id = 1:length(hex_center_xs), as.data.frame(hex_x)), id.vars = "id", value.name = "x")
hexdat_y <- melt(cbind(id = 1:length(hex_center_ys), as.data.frame(hex_y)), id.vars = "id", value.name = "y")

hexdat <- full_join(hexdat_y, hexdat_x)

hex_center_xs_df <- melt(cbind(id = 1:length(hex_center_xs), as.data.frame(hex_center_xs)), id.vars = "id", value.name = "x") %>% select(-variable)
hex_center_ys_df <- melt(cbind(id = 1:length(hex_center_ys), as.data.frame(hex_center_ys)), id.vars = "id", value.name = "y") %>% select(-variable)

hexdat_centers <- full_join(hex_center_xs_df, hex_center_ys_df, by = "id") %>% mutate(Xind = ((id-1)%%MAX_COLS)+1,
                                                                                      Yind = ceiling(id/MAX_COLS))

#These are to not have to filter to find values, easier to lookup in m2
x_center_mat <- hexdat_centers %>% select(x,Xind,Yind) %>% 
  pivot_wider(names_from = Yind, values_from = x) %>% 
  select(-Xind) %>% 
  as.matrix()

y_center_mat <- hexdat_centers %>% select(y,Xind,Yind) %>% 
  pivot_wider(names_from = Yind, values_from = y) %>% 
  select(-Xind) %>% 
  as.matrix()

MAX_X <- max(hex_center_xs)
MAX_Y <- max(hex_center_ys)

CENTER_X <- (hexdat_centers %>% filter(Yind == ceiling(MAX_ROWS/2) & Xind == ceiling(MAX_COLS/2)))$x
CENTER_Y <- (hexdat_centers %>% filter(Yind == ceiling(MAX_ROWS/2) & Xind == ceiling(MAX_COLS/2)))$y

# Graphing Functions

#This takes a given hive and turns it into a graph

hive_matrix_to_graph <- function(hive_data,queen){
  hive_tbl <- as.table(hive_data[,,1:2])
  colnames(hive_tbl) <- 1:MAX_COLS
  rownames(hive_tbl) <- 1:MAX_ROWS
  hive_df_pre <- as.data.frame(hive_tbl)
  hive_df <- as.data.frame(hive_tbl) %>% pivot_wider(names_from = Var3, values_from = Freq)
  colnames(hive_df) <- c("y","x","contents","amount")
  hive_df <- hive_df %>% arrange(y) %>%
    mutate(id = 1:(MAX_COLS*MAX_ROWS)) %>% 
    mutate(named_content = ifelse(contents == 0, "Empty",
                                  ifelse(contents == 1, "Brood",
                                         ifelse(contents == 2, "Pollen",
                                                ifelse(contents == 3, "Honey","???")))))
  new_hexdat <- full_join(hexdat,
                          hive_df %>% select(id,named_content,amount),
                          by="id") %>% mutate(named_content = fct_relevel(as.factor(named_content),types))
  
  new_hexdat <- new_hexdat %>% mutate(amount = ifelse(named_content == "Honey",amount/MAX_HONEY,
                                                      ifelse(named_content == "Pollen",amount/MAX_POLLEN,
                                                             ifelse(named_content == "Brood",amount/MAX_BROOD,1))))
  
  g <- ggplot() + 
    geom_polygon(new_hexdat, mapping = aes(x,y,group = id, fill = named_content, alpha = amount), colour = "black") +
    scale_fill_manual(values=hive_colors) +
    coord_fixed() +
    theme_classic()
  
  return(g)
}

#Graphs the trends in contents over time

graph_trends <- function(content_df){
  Count_Contents_Percent <- tibble(content_df/(MAX_ROWS*MAX_COLS)) %>% mutate(days = (1:n())/24) %>%
    pivot_longer(c(Brood,Honey,Pollen,Empty))
  
  g <- ggplot(Count_Contents_Percent, aes(x = days, y = value, color = name))+
    scale_color_manual(values = hive_colors)+
    geom_line()+
    theme_classic()
  
  return(g)
}
