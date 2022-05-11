library(sf)

coo <- matrix(c(
  47.155448754072374, -1.6406400682309141,
47.15744374926704, -1.652151433982812
), ncol=2,  byrow=T)  
coo_col <- data.frame(coo)
dimnames(coo_col)[[2]] <- c("latitude","longitude")

coord_col <- SpatialPointsDataFrame(coords=coo_col[,1:2], data=as.data.frame(coo_col))
coord_col_sf <- st_as_sf(coord_col, crs = 4326, agr = "constant", remove = F)
st_crs(coord_col_sf) <- 4326 #EPSG WGS84

# Calcul de la distance entre les deux points
my_coords_sf <- st_as_sf(coo_col, coords = c("latitude","longitude"), crs = 4326)
st_distance(coord_col_sf)

# Même résultat avec la fonction ci-dessous (attention : elle
# n'aime pas les noms de colonnes, c'est pourquoi le jeu de données est coo
# et pas coo_col)
line = st_sfc(st_linestring(coo), crs = 4326)
st_length(line)


