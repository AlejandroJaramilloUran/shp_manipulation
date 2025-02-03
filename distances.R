library(sf)

#points (coordinates)
datos <- read.table("points.csv", header=TRUE, sep=",")

#first shp
linea_costera <- st_read("lines.shp")

#second shp
lagos <- st_read("HydroLAKES_polys_v10.shp") 

#check geometry of lagos and upload the new shp
lagos_valid <- st_make_valid(lagos)
st_write(lagos_valid, "lagos_valid.shp")
lagos <- st_read("lagos_valid.shp")

#filter lagos using area
lagos_grandes <- lagos[lagos$Lake_area >= 50, ]
st_write(lagos_grandes, "lagos_grandes.shp")
lagos <- st_read("lagos_grandes.shp") 


#Function to calculate the distance between a set of points and both shp
calcular_distancia_agua <- function(puntos, shapefile_costa, shapefile_lagos) {
  ids_montana <- unique(puntos$ID_mountain)
  
  for (id in ids_montana) {
    # different set of points
    puntos_id_mountain <- puntos[puntos$ID_mountain == id,]
    
    # Convert points to sf object
    puntos_id <- st_as_sf(puntos_id_mountain, coords = c("x", "y"), crs = 4326)
    
    #buffer
    bbox <- st_bbox(puntos_id)
    poligono <- st_as_sfc(bbox)
    buffer <- st_buffer(poligono, dist = 10000)
    
    # intersection between shapefiles and buffer
    corte_costa <- st_intersection(shapefile_costa, buffer)
    corte_lagos <- st_intersection(shapefile_lagos, buffer)
    
    
    if (nrow(corte_costa) == 0) {
      distancia_minima_costa <- rep(">10 km", nrow(puntos_id))
    } else {
      # Calculate distances
      distancias_costa <- st_distance(puntos_id, corte_costa)
      
      # Minimun distance
      distancia_minima_costa <- apply(as.matrix(distancias_costa), 1, min)
    }
    
    if (nrow(corte_lagos) == 0) {
      distancia_minima_lagos <- rep(">10 km", nrow(puntos_id))
    } else {

      distancias_lagos <- st_distance(puntos_id, corte_lagos)
      
      distancia_minima_lagos <- apply(as.matrix(distancias_lagos), 1, min)
    }
    
    # Update
    puntos_id_mountain$d_costa <- distancia_minima_costa
    puntos_id_mountain$d_lagos <- distancia_minima_lagos
    
    # save
    write.csv(puntos_id_mountain, paste0("/sm_", id, ".csv"), row.names = FALSE)
  }
}
calcular_distancia_agua(datos, linea_costera, lagos)