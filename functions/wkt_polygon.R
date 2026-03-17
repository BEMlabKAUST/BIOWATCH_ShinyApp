geojson_to_wkt <- function(feature) {
  
  geojson_str <- jsonlite::toJSON(feature, auto_unbox = TRUE)
  polygon_sf  <- geojsonsf::geojson_sf(geojson_str)
  
  polygon_sf <- sf::st_make_valid(polygon_sf)
  polygon_sf <- sf::st_transform(polygon_sf, crs = 4326)
  
  sf::st_as_text(sf::st_geometry(polygon_sf))
}