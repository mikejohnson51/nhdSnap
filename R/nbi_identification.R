library(dplyr)
library(sf)

nbi = readRDS("./data/nbi_sf_2019.rds")
tt = filter(nbi, state == "WA")

length(unique(nbi$NIB)) - dim(nbi)[1]

### NIB CA 0086 is example of multiple tiger geoms for same feature
###11991
## Tunnel case study = CA 55C0694

i = 123
this.pt = st_transform(tt[i,], 5070)

bb =  this.pt %>% st_buffer(100) %>% st_transform(4269) %>% 
  AOI::bbox_get()

tiger = do.call(rbind,list(
  TIGER::tiger_get(bb, 8),
  TIGER::tiger_get(bb, 4),
  TIGER::tiger_get(bb, 2)))

rd = osmdata::opq(bb) %>% 
  osmdata::add_osm_feature("highway") %>% 
  ?osmdata::osmdata_sf()

osm = rd$osm_lines
if(is.null(osm$bridge)) { osm$bridge = NA }
if(is.null(osm$tunnel)) { osm$tunnel = NA }
if(is.null(osm$id)) { tibble::rownames_to_column(osm, "id") }

this.tiger = tiger[lengths(st_intersects(st_transform(tiger, 5070),
                                       st_buffer(this.pt, 30))) > 0,] %>% 
  st_transform(5070) %>% 
  #st_transform(4326) %>% 
  select(id  = OBJECTID, name = NAME, basename = BASENAME, LAYER) %>% 
  mutate(NIB = this.pt$NIB,
         source = paste0("tiger::", LAYER), 
         distance = as.numeric(st_distance(.,this.pt)),
         bridge = NA,
         tunnel = NA,
         over = FALSE) %>% 
  select(-LAYER)

this.tiger$name[is.na(this.tiger$name)] = "NA"
this.tiger$basename[is.na(this.tiger$basename)] = "NA"

this.osm = osm[lengths(st_intersects(st_transform(osm, 5070),
                                     st_buffer(this.pt, 30))) > 0,] 


if(is.null(this.osm$tiger.name_base)){  this.osm$tiger.name_base = "NA" }

this.osm = this.osm %>% 
  st_transform(5070) %>% 
  select(id = osm_id, name = name, 
         basename = tiger.name_base, bridge, tunnel) %>% 
  mutate(
    NIB = this.pt$NIB,
    source = "osm::highway",
    distance = as.numeric(st_distance(.,this.pt)),
    over = FALSE)

this.osm$name[is.na(this.osm$name)] = "NA"
this.osm$basename[is.na(this.osm$basename)] = "NA"

if(nrow(this.osm) >= 2){
  
  n = unique(this.osm$name)
  
  for(i in 1:length(n)){
    # find OSM nodes with name n
    if(is.na(n[i])){ 
      ind = which(is.na(this.osm$name))
    } else {
      ind = which(this.osm$name == n[i])
    }
   
    # is there one of those with a bridge?
    if(length(ind) > 1){
      if(sum(!is.na(this.osm$bridge[ind])) > 0){
        # if so remove the one w/o the bridge
        ind2 = ind[which(is.na(this.osm$bridge[ind]))]
        this.osm = this.osm[-ind2,]
      } else if(sum(!is.na(this.osm$tunnel[ind])) > 0){
        # if so remove the one w/o the bridge
        ind2 = ind[which(is.na(this.osm$tunnel[ind]))]
        this.osm = this.osm[-ind2,]
      } else if(length(ind) > 1){
        ind2 = ind[-which.min(this.osm$distance[ind])]
        this.osm = this.osm[-ind2,]
      }
    }
    
    
    if(nrow(this.osm) >= 3){
      this.osm = this.osm[!is.na(this.osm$name),]
    }
    
    if(nrow(this.osm) == 2){
      this.osm$over = ifelse(is.na(this.osm$bridge), FALSE, TRUE)
    }
    
    if(nrow(this.osm) == 2){
      this.osm$over = ifelse(!is.na(this.osm$tunnel), TRUE, this.osm$over)
    }
  }
}

# if(sum(lengths(st_crosses(this.osm))) == 0 & nrow(this.osm) > 1){
#   this.osm = this.osm %>%
#     arrange(distance) %>%
#     head(1)
# }

if(nrow(this.osm) == 1){ this.osm$over = TRUE }

n = unique(this.tiger$name)

if(nrow(this.tiger) > length(n)){
  
  for(i in 1:length(n)){
    
    if(is.na(n[i])){ 
      ind = which(is.na(this.tiger$name))
    } else {
      ind = which(this.tiger$name == n[i])
    }

    if(length(ind) > 1){
      ind2 = ind[which.max(this.tiger$distance[ind])]
      this.tiger = this.tiger[-ind2,]
    }
    
  }
}

## OSM length 2, crosses road

if(nrow(this.osm) == 1){
  this.tiger = this.tiger %>%
    arrange(distance) %>%
    head(1)
  this.tiger$over = this.osm$over
}

if(nrow(this.tiger) > nrow(this.osm)){
  this.tiger  = this.tiger %>% 
    arrange(distance) %>% 
    slice(1:nrow(this.osm))
}

if(nrow(this.osm) == 2){
  this.tiger$over = FALSE
  over.osm = which(this.osm$over)
  tiger.o = sapply(1:nrow(this.tiger), 
                   function(x){ orientation(this.tiger[x,])}
  )
  
  tmp = which(this.tiger$basename == this.osm$basename[over.osm])
  
  if(length(tmp) > 0 ){
    this.tiger$over[tmp] = TRUE
  } else {
    
    or.match = which(tiger.o == orientation(this.osm[over.osm,]))
    
    name1 = stringdist::stringsim(tolower(this.osm$name[1]), 
                                  tolower(this.tiger$name), 
                                  method = "jw")
    
    name2 = stringdist::stringsim(tolower(this.osm$name[2]), 
                                  tolower(this.tiger$name), 
                                  method = "jw")
    
    out = data.frame(osm = c(1:2),
                     tiger = c(which.max(name1), which.max(name2)),
                     val  = c(max(name1), max(name2)))
    
    out$tiger[which.min(out$val)] = c(1:2)[c(!c(1:2) %in% out$tiger[which.max(out$val)])]
    
    name.match = out$tiger[out$osm == over.osm]
    
    if(or.match == name.match){
      this.tiger$over[or.match] = TRUE
    } 
  }
  
}


xx = do.call(rbind, list(this.tiger, this.osm)) %>% arrange(desc(over))

if(nrow(xx) != 4){
water = osmdata::opq(bb) %>% 
  osmdata::add_osm_feature("waterway") %>% 
  osmdata::osmdata_sf()

if(!is.null(water$osm_lines)){
if(nrow(water$osm_lines) != 0 ) { 
 
  osm_water = water$osm_lines %>% st_transform(5070)
  
  if(is.null(osm_water$id)) { osm_water = tibble::rownames_to_column(osm_water, "id") }
  if(is.null(osm_water$source)) { osm_water$source = "osm::waterway" } else {
    osm_water$source = paste0("osm::",osm_water$source)
  }
  
  osm_water = osm_water %>% select(id, waterway, source, name)
  osm_water2 = osm_water[lengths(st_crosses(osm_water, xx)) > 0,]
  
  if(nrow(osm_water2) > 1){
    osm_water2 = osm_water2[which.min(st_distance(st_intersection(xx[1,], osm_water2), this.pt)),]
  }
}
} else {
  osm_water2 = NULL 
}


nhd = query_cida(bb, "nhd" ) 

if(!is.null(nhd)){
  nhd = nhd%>% 
  st_transform(5070) %>% 
  select(id = comid, waterway = ftype, name = gnis_name) %>%
  mutate(source = "usgs::NHD")

nhd2 = nhd[lengths(st_crosses(nhd, xx)) > 0,]

if(nrow(nhd2) > 1){
  nhd2 = nhd2[which.min(st_distance(st_intersection(xx[1,], nhd2), this.pt)),]
}

} else {
  nhd2 = NULL
}


under_w = do.call(rbind, list(nhd2, osm_water2)) %>% 
  mutate(over = FALSE)
}

over = xx %>% filter(over == TRUE) 
over$stringDist = max(
  stringsim(tolower(over$name[1]), tolower(c(over$name[2], over$basename[2]))),
  stringsim(over$basename[1], c(over$name[2], over$basename[2])))
under = xx %>% filter(over == FALSE)
if(nrow(under) == 0){ under = under_w}
under$stringDist = stringsim(tolower(under$name[1]), tolower(under$name[2]))
# NIB, Water, overSource, overID1, underSource, underID1, X, Y, dist, overMatch, underMatch

out = st_intersection(over, under) 

for(i in 1:nrow(out)){
  if(st_geometry_type(out[i,]) == "MULTIPOINT"){
   new =  st_cast(out[1,], "POINT") %>% 
      mutate(dist = as.numeric(st_distance(.,this.pt))) %>% 
      arrange(dist) %>% 
      select(dist) %>% 
      head(1)
   
   out$geometry[i] = new$geometry
  }
}
 
out = out %>% 
  select(NIB, 
         overSource = source, overID = id,
         underSource = source.1, underID = id.1,
         nameMatchOver = stringDist, nameMatchUnder = stringDist.1,
         bridge, tunnel) %>% 
  mutate(distance = as.numeric(st_distance(., this.pt)),
         X = st_coordinates(.)[,1], Y = st_coordinates(.)[,2],
         ) %>%
  arrange(distance) #%>% 
  #st_drop_geometry()

library(leaflet)
leaflet() %>% 
  addProviderTiles(providers$Esri.WorldImagery) %>% 
  addPolylines(data = st_transform(this.osm, 4326), col = 'red', label = this.osm$name) %>% 
  addPolylines(data = st_transform(under[1,], 4326), col = 'blue') %>% 
  addMarkers(data = st_transform(this.pt, 4326)) %>% 
  addCircleMarkers(data = st_transform(out, 4326)) 
  
st_drop_geometry(out) %>% filter(distance < 100)

########