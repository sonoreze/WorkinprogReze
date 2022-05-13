# library(sf)
# 
# coo <- matrix(c(
#   47.155448754072374, -1.6406400682309141,
# 47.15744374926704, -1.652151433982812
# ), ncol=2,  byrow=T)  
# coo_col <- data.frame(coo)
# dimnames(coo_col)[[2]] <- c("latitude","longitude")
# 
# coord_col <- SpatialPointsDataFrame(coords=coo_col[,1:2], data=as.data.frame(coo_col))
# coord_col_sf <- st_as_sf(coord_col, crs = 4326, agr = "constant")
# st_crs(coord_col_sf) <- 4326 #EPSG WGS84
# st_distance(coord_col_sf)
# 
# # Calcul de la distance entre les deux points
# my_coords_sf <- st_as_sf(coo_col, coords = c("latitude","longitude"), crs = 4326)
# st_distance(coord_col_sf)
# 
# # Même résultat avec la fonction ci-dessous (attention : elle
# # n'aime pas les noms de colonnes, c'est pourquoi le jeu de données est coo
# # et pas coo_col)
# line = st_sfc(st_linestring(coo), crs = 4326)
# st_length(line)


library(sf)
library(tidyverse)
library(ggplot2)
library(patchwork)
library(lwgeom)
library(mapview)
# 
# uge <- data.frame(long=c(-1.6406400682309141,-1.652151433982812), lat=c(47.155448754072374,47.15744374926704)) %>%
#   st_as_sf(coords = c("long", "lat")) %>% 
#   st_set_crs(4326)
# mapview(uge)
# 
# st_distance(uge)
# max(st_distance(uge))
# 
# 
#   ##############################
# library(sf)
# library(dplyr)
# 
# set.seed(1)
# 
# df <- data.frame(
#   gr = c(rep("a",5),rep("b",5)),
#   x  = rnorm(10),
#   y = rnorm(10)
# )
# 
# df <- st_as_sf(df,coords = c("x","y"),remove = F)
# df %>%
#   group_by(gr) %>%
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T),
#   )

#####################
tags_table_init <- as.data.frame(noisecapture_data[,c("Id","Date","x","y","leq_mean","tags","accuracy")]) %>%
  filter(x != "NA") %>%
  filter(tags != "") %>%
  filter(accuracy<20) %>%
  arrange(Id,Date) %>%
  group_by(Id) %>%
  arrange(Date) %>%
  mutate(IdTraceReset=cumsum(c(TRUE, as.integer(diff(as.POSIXct(Date)), units = "secs") >= 2L))) %>%
  ungroup() %>%
  mutate(IdGlobal=str_c(Id,IdTraceReset)) %>%
  arrange(IdGlobal) %>%
  group_by(Id) %>%
  arrange(IdTraceReset) %>%
  group_by(IdGlobal) %>%
  mutate(IdTrace=cur_group_id()) %>%
  ungroup() %>%
  select(-c(accuracy,IdGlobal,IdTraceReset))

tags_table <- tags_table_init %>%
  st_as_sf(coords = c("x","y"),remove = F) %>%
  st_set_crs(4326) %>%
  group_by(IdTrace) %>%
  mutate(
    lead = geometry[row_number() + 1],
    distance = st_distance(geometry, lead, by_element = T),
  ) %>%
  mutate(DistanceTotale = sum(as.numeric(distance),na.rm=TRUE)) %>%
  select(-c(lead)) %>%
  filter(DistanceTotale<500)

length(unique(rle(tags_table_init$IdTrace)$values))
length(unique(rle(tags_table$IdTrace)$values))

# summary(tags_table$DistanceTotale)
# tags_table[tags_table$DistanceTotale >499,]
# data_255 <- tags_table[tags_table$IdTrace == "255",]
# 
# IdMax <- tags_table[tags_table$IdTrace == "352",] %>%
#   st_as_sf(coords = c("x", "y")) %>% 
#   st_set_crs(4326)
# mapview(IdMax)
# 
# ################
# IdMax <- tags_table[tags_table$IdTrace == "352",]
# uge_352 <- IdMax[1:10,c("x","y")]
# uge_352 <- uge_352 %>%
#   st_as_sf(coords = c("x", "y")) %>% 
#   st_set_crs(4326)
# mapview(uge_352)
# st_distance(uge_352)
# 
# st_as_sf(uge_352,coords = c("x","y"),remove = F) %>%
#   mutate(
#     lead = geometry[row_number() + 1],
#     dist = st_distance(geometry, lead, by_element = T),
#   )
# 
# ################# OK
# num <- c(352,23)
# IdMax <- data_geom[data_geom$IdTrace %in% num,c("IdTrace","x","y")]
# uge <- IdMax %>%
#   st_as_sf(coords = c("x","y")) %>%
#   st_set_crs(4326) %>%
#   group_by(IdTrace) %>%
#   mutate(
#     lead = geometry[row_number() + 1],
#     distance = st_distance(geometry, lead, by_element = T),
#   ) %>%
#   mutate(DistanceTotale = sum(as.numeric(distance),na.rm=TRUE))
# 
# summary(as.numeric(uge$distance))
# sum(as.numeric(uge$distance),na.rm=TRUE)
#   