# Calculate the center/average of multiple GeoLocation coordinates
# Expects an array of objects with .latitude and .longitude properties
# https://medium.com/fishbrain/finding-the-center-point-in-a-cluster-of-coordinates-e607cdf75fd5

averageGeolocation <- function(coords) 
  {
    x <- 0
    y <- 0
    z <- 0
    
    new_coords <- data.frame(coords)
    
    total <- dim(coords)[1]
    
    for (i in 1:total) {
      new_coords[i,"latitude"] <- new_coords[i,1] * pi / 180
      new_coords[i,"longitude"] <- new_coords[i,2] * pi / 180
      
      
      x <- x + cos(new_coords[i,3]) * cos(new_coords[i,4])
      y <- y + cos(new_coords[i,3]) * sin(new_coords[i,4])
      z <- z + sin(new_coords[i,3])
    }
    
    x = x / total
    y = y / total
    z = z / total
    
    centralLongitude <- atan2(y, x)
    centralSquareRoot <- sqrt(x * x + y * y)
    centralLatitude <- atan2(z, centralSquareRoot)
    
    latitude <- centralLatitude * 180 / pi
    longitude <- centralLongitude * 180 / pi
    return(c(latitude,longitude))
  }

#Exemple
#coords <- matrix(c(47.15461,47.15418,-1.63409,-1.63212), ncol=2)
#averageGeolocation(coords)


library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

my_coords <- structure(list(longitude = c(-106.425668, -96.111617, -96.5,-96.4, -96, -96.1), latitude = c(23.18127, 15.779873, 19.68,19.68, 19.68, 19.68)), row.names = c(NA, 6L), class = "data.frame")

my_coords_sf <- st_as_sf(my_coords, coords = c("longitude", "latitude"), crs = 4326)

st_distance(my_coords_sf)

#################
# https://mhallwor.github.io/_pages/basics_SpatialPoints
firstPoints <- SpatialPoints(coords = my_coords)
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)
euclidDist

# Google : 1,25 km (est-ce à vol d'oiseau ?)
coo <- matrix(c(
  47.15541429941908, -1.640535106457943,
  47.157397510417056, -1.651941481495584,
  47.15748057523031, -1.6523461253211502,
  47.157584390164935, -1.6532411135763287,
  47.157678964063976, -1.6542998443445218,
  47.1576365023342, -1.6551513704315401,
  47.15749753643392, -1.6557474387972666,
  47.15743384360905, -1.6560596650291732,
  47.157250485050604, -1.656584772782835
  ), ncol=2,  byrow=T)  
coo_col <- data.frame(coo)
dimnames(coo_col)[[2]] <- c("latitude","longitude")

# Calcul de la matrice des distances entre les points
my_coords_sf <- st_as_sf(coo_col, coords = c("latitude","longitude"), crs = 4326)
st_distance(my_coords_sf)

#Vérification par addition des distances entre
# les points pris deux à deux: 1,811 km
1287.34575 + 45.93188 + 100.185 + 118.19404 + 94.80308 + 68.05584 + 35.43246 + 61.84391

# C'est ce que retourne la fonction ci-dessous (attention : elle
# n'aime pas les noms de colonnes, c'est pourquoi le jeu de données est coo
# et pas coo_col)
line = st_sfc(st_linestring(coo), crs = 4326)
st_length(line)

####################################
id22 <- as.matrix(data_trace[data_trace$IdTrace == 22,c("x","y")])
line = st_sfc(st_linestring(id22), crs = 4326)
st_length(line)


my_coords_sf <- st_as_sf(id22, coords = c("x", "y"), crs = 4326)
dmat <- st_distance(my_coords_sf)
dmat

for (i in 1:dim(id22)[1])
  { tot <- tot + dmat[i+1,i] } 
tot

################### LIGNE DROITE : 898,18 m
coo <- matrix(c(
  47.155448754072374, -1.6406400682309141,
47.15744374926704, -1.652151433982812
), ncol=2,  byrow=T)  
coo_col <- data.frame(coo)
dimnames(coo_col)[[2]] <- c("latitude","longitude")

# Calcul de la matrice des distances entre les points
my_coords_sf <- st_as_sf(coo_col, coords = c("latitude","longitude"), crs = 4326)
st_distance(my_coords_sf)

# Même résultat avec la fonction ci-dessous (attention : elle
# n'aime pas les noms de colonnes, c'est pourquoi le jeu de données est coo
# et pas coo_col)
line = st_sfc(st_linestring(coo), crs = 4326)
st_length(line)