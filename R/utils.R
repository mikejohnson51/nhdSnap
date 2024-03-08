process_OSM = function(osm_pts, this.pt){
  osm_pts = osm_pts %>% 
    mutate(distance = as.numeric(st_distance(., st_transform(this.pt, st_crs(.))))) %>% 
    arrange(distance, desc(stringDist)) %>% 
    head(1) %>% 
    st_transform(5070)
  
  if(is.null(osm_pts$name)){ osm_pts$name = NA }
  
  osm_pts 
}


num_extract = function(x){ as.numeric(str_extract(x, "(\\d)+"))}


find_nearest_pt = function(feature, pt){
  this = feature %>% 
    st_nearest_points(pt) %>% 
    st_line_sample(sample = c(0,1)) %>% 
    st_cast("POINT") %>%
    st_as_sf() %>% 
    rename(geometry = x) %>% 
    mutate(distance = as.numeric(st_distance(., feature)))%>% 
    arrange(distance) %>% 
    head(1) %>% 
    mutate(distance = as.numeric(st_distance(., pt))) 
  
  bind_cols(this, st_drop_geometry(feature))
}


populate =  function(feature, id1, id2, source, name, stringDist, priority, on_network, wikidata) {

  
  if(!is.null(feature)){
    if(nrow(feature) != 0 ){
    out = dplyr::select(feature, id1 = !!id1, id2 = !!id2, 
                 !!stringDist, name = !!name, wikidata = !!wikidata) %>% 
      mutate(source = source, 
             num = num_extract(name), 
             priority =  priority, 
             on_network = on_network)
    
    if(is.null(id2)) { out$id2 = NA }
    if(is.null(wikidata)) { out$wikidata = NA }
    if(is.null(stringDist)) { out$stringDist = NA }
    
  } else{
    out = NULL
  }
  } else {
    out = NULL
  }

  out
}

out_to_leaf = function(out){
  st_as_sf(out, coords = c("X", "Y"), crs = 5070) %>% 
    st_transform(4326)
}

orientation = function(core){
  dX = abs(st_coordinates(core)[1,1] - st_coordinates(core)[2,1])
  dY = abs(st_coordinates(core)[1,2] - st_coordinates(core)[2,2])
  
  if(dX > dY){
    return('EW')
  } else {
    return("NS")
  }
}

findHUC12 = function(AOI){
  
  ws.crs = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs'
  
  bb = st_transform(AOI, ws.crs) %>% sf::st_bbox()
  bb.ordered =  paste(bb[1],bb[2],bb[3],bb[4], sep = "%2C")
  
  endpoint = 12 / 2
  
  url = paste0('https://hydro.nationalmap.gov/arcgis/rest/services/wbd/MapServer/',
               endpoint,
               '/query?',
               '&geometry=',
               bb.ordered,
               '&geometryType=esriGeometryEnvelope',
               '&outFields=*',
               '&returnGeometry=true',
               '&returnZ=false',
               '&returnM=false',
               '&returnExtentOnly=false',
               '&f=geoJSON')
  
  tryCatch({ sf::read_sf(url) %>% st_transform(sf::st_crs(AOI)) },
           warning = function(w) { NULL },
           error = function(e) { NULL })
  
  
}

runit <- function (pt) {
  return(tryCatch(
    suppressWarnings(align_pt_to_nhd(pt = pt)), 
    error=function(e) NULL))
}

st_line_midpoints <- function(sf_lines = NULL) {
  
  g <- st_geometry(sf_lines)
  df = st_drop_geometry(sf_lines)
  
  g_mids <- lapply(g, function(x) {
    
    coords <- as.matrix(x)
    
    # this is just a copypaste of View(maptools:::getMidpoints):
    get_mids <- function (coords) {
      dist <- sqrt((diff(coords[, 1])^2 + (diff(coords[, 2]))^2))
      dist_mid <- sum(dist)/2
      dist_cum <- c(0, cumsum(dist))
      end_index <- which(dist_cum > dist_mid)[1]
      start_index <- end_index - 1
      start <- coords[start_index, ]
      end <- coords[end_index, ]
      dist_remaining <- dist_mid - dist_cum[start_index]
      mid <- start + (end - start) * (dist_remaining/dist[start_index])
      return(mid)
    }
    
    mids <- st_point(get_mids(coords))
  })
  
  geometry <- st_sfc(g_mids, crs = st_crs(sf_lines))
  df$geometry = geometry 
  st_as_sf(df)
}

