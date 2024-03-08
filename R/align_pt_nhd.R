#Good test = 24497
#54497

library(sf)
library(dplyr)
library(stringdist)
library(AOI)
library(stringr)
library(osmdata)

# hp = read_sf('/Users/mikejohnson/Downloads/ORNL_EHAHydroPlant_FY2020 2/Plant_operational_ExternalGISFY2020final.shp') %>% 
#   filter(!is.na(NID_ID)) %>% 
#   st_transform(5070)
# 


align_pt_to_nhd = function(pt, searchRadius = 2000){
  
  # Initalize place holders for all possible realizations:
  osm_pt <- osm_line <- osm_pg <- # OSM features
  fema <- this.gnis <- # Federal data
  this.fl <- this.wb <- ends <- wb_fl <- hp.here <-   # NHD data
  NULL

  
  # Current Dam in EPSG:5070
  this.pt = st_transform(pt, 5070) %>% 
    mutate(stringDist = NA)
  
  # Set the search area: searching within a radius
  bb  = st_transform(st_buffer(this.pt,searchRadius), 5070) 

# NHD Work ---------------------------------------------------------------

# Flowlines ---------------------------------------------------------------
  # Here we are:
    # 1. Query the NHD for the bounding box
    # 2. Transform to EPSG: 5070
    # 3. Calculate the string similarity between the NHD GNIS name and the NID river attribute
  
  fl = nhdplusTools::get_nhdplus(bb) %>% 
    dplyr::select(comid, gnis_id, gnis_name, ftype, wbareacomi, reachcode) %>% 
    st_transform(5070)

if(!is.null(fl)){
    
  fl = mutate(fl, stringDist = stringsim(tolower(this.pt$river), tolower(gnis_name), method='jw'))
  
# Here we see if the NID name contains TR, OS, or TRIB. 
  on.trib = grepl("TR", this.pt$river) | grepl("OS", this.pt$river) | grepl("TRIB", this.pt$river)
# If it does,then we know the name should not be matched to river names because the name is not representative of the river it is on.

if(on.trib) { 
  fl = fl # No subset, keep all FL
} else if (max(fl$stringDist, na.rm = TRUE) > .7){
  fl = filter(fl, stringDist == max(stringDist, na.rm = TRUE))
} else {
  fl = fl # if not on tributary (on.trib = FALSE), but no close name-match (max < .7)
}

# From the remaining flowlines, we find the one closest to 
# the NID dam, select the attributes we want, and then
# Identify the closest POINT on the flowline to the dam
  this.fl = fl[st_nearest_feature(this.pt, fl),] %>% 
    #dplyr::select(comid, gnis_name, stringDist) %>% 
    find_nearest_pt(this.pt) 
}
## Waterbodies -------------------------------------------------------------

  # NHD Waterbodies
  # Query Waterbodies
  all.wb =  nhdplusTools::get_waterbodies(bb)
  # If their are waterbodies...
  if(!is.null(all.wb)){
    #transform to 5070 and calcuate name-match
    wb = st_transform(all.wb, 5070) %>% 
      mutate(stringDist = max(
               stringsim(tolower(this.pt$dam_name), tolower(gnis_name), method='jw'),
               stringsim(tolower(this.pt$dam_former_name), tolower(gnis_name), method='jw'),
               stringsim(tolower(this.pt$other_dam_name), tolower(gnis_name), method='jw'),
               na.rm = TRUE
  ))
    #Find nearest featrue to the dam, select the needed feilds, find nearest point to dam on the feature 
    this.wb = wb[st_nearest_feature(this.pt, wb),] %>% 
      dplyr::select(comid, gnis_name, areasqkm, reachcode, stringDist) %>% 
      find_nearest_pt(this.pt) 
    
    if(!is.null(fl)){
    # Identify if any of the flowlines are assocatied to the waterbody
    wb_fl =  fl[which(fl$wbareacomi %in%  this.wb$comid),]
    # If so, find the isolate the water body
    wb_fl =  wb_fl[st_nearest_feature(this.pt, wb_fl),]
 
    # Intersect the wb and the waterbody FL and calcuate the length of the returned object
    ends = suppressWarnings(st_intersection(wb, wb_fl)) %>%
      mutate(len = as.numeric(st_length(.))) %>%
      dplyr::select(len, wb_comid = comid, fl_comid = comid.1, gnis_name, stringDist) %>% 
      filter(wb_comid == this.wb$comid)
    
    # If there are intesection geometries...
    if(nrow(ends) != 0){
      # Use the find nearest pt function which:
      # 1. takes ends and cast to nearest points to the dam (sf)
      # 2. isolates the start and end nodes
      # 3. casts to POINT
      # 4. Joins to the origional attributes
      ends = find_nearest_pt(ends, this.pt) 
    } else {
      ends = NULL
    } 
    }
  }

# OSM Work ----------------------------------------------------------------
# Here we start our OSM work
# Define OSM query for our bounding box
  osm_query = opq(st_transform(bb, 4326))
  
# OSM dams ----------------------------------------------------------------

## Get OMS waterway:dam elements
  dd  = add_osm_feature(osm_query, key = "waterway", value = "dam") %>%
    osmdata_sf()
  
  # If water::way dam nodes are not null 
  if(!is.null(dd$osm_points)){
  # And there is data!
    if(nrow(dd$osm_points) !=0){
      ## The OSM data is sometimes weird. OSM ids are always listed as 
      ## rownames and often as an explict attribute, we just use rowname.
      osm_points = dd$osm_points %>% tibble::rownames_to_column("osm_id2")
      # Compare OSM names to the NID dam name
      tmp = pmax(
        stringsim(tolower(osm_points$name), tolower(this.pt$dam_name), method='cosine'),
        stringsim(tolower(osm_points$name), tolower(this.pt$dam_former_name), method='cosine'),
        stringsim(tolower(osm_points$name), tolower(this.pt$other_dam_name), method='cosine'),
        na.rm= TRUE)
        
    
      ## If name match returns NULL use NA
      if(length(tmp) == 0){ osm_points$stringDist = NA } else { osm_points$stringDist = tmp }
      ## Process OSM does the following:
        ## 1. Calcuate distance to dam
        ## 2. Arrange by distance and the stringDist
        ## 3. Pick the top (1) point
      osm_pt =  osm_points %>% process_OSM(this.pt)
    } 
  }
  
  if(!is.null(dd$osm_lines)){
    if(nrow(dd$osm_lines) !=0){
      osm_lines = dd$osm_lines %>% 
        tibble::rownames_to_column("osm_id2")
      
      tmp = pmax(
        stringsim(tolower(osm_lines$name), tolower(this.pt$dam_name), method='jw'),
        stringsim(tolower(osm_lines$name), tolower(this.pt$dam_former_name), method='jw'),
        stringsim(tolower(osm_lines$name), tolower(this.pt$other_dam_name), method='jw'),
        stringsim(tolower(osm_lines$alt_name), tolower(this.pt$dam_name), method='jw'),
        stringsim(tolower(osm_lines$alt_name), tolower(this.pt$dam_former_name), method='jw'),
        stringsim(tolower(osm_lines$alt_name), tolower(this.pt$other_dam_name), method='jw'),
        na.rm = TRUE)
        
      if(length(tmp) == 0){ 
        osm_lines$stringDist = NA 
      } else { 
        osm_lines$stringDist = tmp 
      }
      # Instead of passing the whole line, assume the line centroid (maybe identify point closest to this.pt?)
      osm_line = st_line_midpoints(osm_lines) %>% 
        process_OSM(this.pt)
    }
  }
  
# OSM Water ---------------------------------------------------------------
  # Get OSM natural:water elements 
  dd2 = add_osm_feature(osm_query, key = "natural", value = "water") %>% 
    osmdata_sf()
  
  # Focus on polygons. If there return is not null
  if(!is.null(dd2$osm_polygons)){
    # and there is data...
    if(nrow(dd2$osm_polygons) != 0){
    # project the polygons to 5070
      osm.wb = st_transform(dd2$osm_polygons, 5070) 
    # Make sure source is specified
      if(is.null(osm.wb$source)){ osm.wb$source = "OSMbase" } 
    # and find the nearest feature (store as index)
      osm_pg = osm.wb[st_nearest_feature(this.pt, osm.wb),] %>% 
        dplyr::select(osm_id, source) %>% 
        find_nearest_pt(this.pt) 
    }
  }


# GNIS --------------------------------------------------------------------
# Isolate nearest GNIS dam feature, record the distance and name-match between dam and feature
#this.gnis = gnis.5070[bb,]
  
this.gnis  = gnis.5070[st_nearest_feature(this.pt, gnis.5070),] %>% 
  mutate(distance = as.numeric(st_distance(., this.pt)),
         stringDist = stringsim(tolower(this.pt$dam_name), tolower(FEATURE_NAME), method = "cosine"))

# FEMA NHLD ---------------------------------------------------------------
  # Query FEMA stuctures data
 fema =  NFHL::nfhl_get(bb, 24)
  if(is.null(fema)){fema = data.frame()}
  # If there are features
  if(nrow(fema) != 0){
    # Transform to EPSG:5070 and select type == 'Dam'
    # Create Centroid, and name-match
    fema = st_transform(fema, 5070) %>% 
      filter(STRUCT_TYP == "Dam") %>% 
      st_line_midpoints() %>% 
      mutate(stringDist = stringsim(tolower(this.pt$dam_name), tolower(STRUCT_NM)), distance = as.numeric(st_distance(.,this.pt))) %>% 
      arrange(distance, stringDist) %>% 
      head(1)
  } else {
    fema = NULL
   # message("No FEMA Dams nearby")
  }
  
  # This chunck prioritizes either WB or FL reps
  if(is.null(this.wb)){
    if(is.null(this.fl)){
      # No wb no FL
      fl.priority = 100
      wb.priority = 100
    } else {
      # No wb with FL
      fl.priority = 7
      wb.priority = 100
    }
  } else {
    if(is.null(this.fl)){
     # with wb no fl
      fl.priority = 100
      wb.priority = 7
    } else {
    # with wb with fl
      if(this.wb$distance < 50 | max(0,this.wb$stringDist, na.rm = T) > .7){
        #message("We suggest a WB snap!")
        fl.priority = 8
        wb.priority = 7
      } else if(this.fl$distance < this.wb$dist){
        #message("We suggest a FL snap!")
        fl.priority = 7
        wb.priority = 8
      } else {
       # message("We suggest a WB snap!")
        fl.priority = 8
        wb.priority = 7
      }
  }
  }

  osm_rank = ifelse(!is.na(c(osm_line$name, osm_pt$name)), 2, 2.25)
  
## HydroPower
  
  # hp.here = hp %>% filter(NID_ID == this.pt$ID)
  # 
  # if(nrow(hp.here) > 0){
  #   hp.here = st_transform(hp.here, 5070) %>% 
  #     mutate(stringDist = stringsim(tolower(this.pt$dam_name), tolower(PtName)), distance = as.numeric(st_distance(.,this.pt)))
  # }
  
  

## Build out a list of found features using 'populate':
  ll = list()
  
## {Priorities}
    # 1. Intersections because they are found by both a WB and FL
    # 2. FEMA becasue they are surveyed locations
    # 3. OSM becasue they have crowd updates
    # 3.5 OSM Waterbodies 
    # 4. GNIS
    # 5. FL or WB (NHD) depending on above code...

## Populate requires:            feature,    id1,         id2,         source,        name,            stringDist,   priority,    on_network
 ll[['nid']]          = populate(this.pt,   "nidid",      NULL,        "NID",         "dam_name",      NULL,         10,          0, NULL)
 ll[['fema']] = populate(fema, 'OBJECTID', NULL, "FEMA",  "STRUCT_NM",  "stringDist", 1, 0, NULL)
 ll[['osmline']] = populate(feature = osm_line,
                            id1 = 'osm_id2',    
                            id2 = NULL,   
                            source = "osmline",  
                            name = "name", 
                            stringDist = "stringDist", 
                            priority = osm_rank[1], 
                            on_network = 0, 
                            wikidata = NULL)
 ll[['osmpt']]        = populate(osm_pt,    'osm_id2',    NULL,        "osmpt",       "name",          "stringDist", osm_rank[2],           0, NULL)
 ll[['intersection']] = populate(ends,      'fl_comid',   'wb_comid',  "INT",         'gnis_name',     'stringDist', 4,           1, NULL)
 ll[['osmpg']]        = populate(osm_pg,    'osm_id',     NULL,        "osmpolygon",  "source",        NULL,         5,           0, NULL)
 ll[['gnis']]         = populate(this.gnis, 'FEATURE_ID', NULL,        "GNIS",        'FEATURE_NAME',  'stringDist', 6,           0, NULL)
 ll[['wb']]           = populate(this.wb,   'comid',      'reachcode', "NHDWB",       'gnis_name',     'stringDist', wb.priority, 1, NULL)
 ll[['fl']]           = populate(this.fl,   'comid',      'reachcode', "NHDFL",       'gnis_name',     'stringDist', fl.priority, 1, NULL)
 
ll[['hp']]           = populate(hp.here,   'EHA_PID',      NULL, "ONRL:HP",       'PtName',     'stringDist', 1000, 0, NULL)
 
# Bind the list, calcualte distance to NID dam, 
# sort bt priority, 
# only keep those within 500 m of this dam

# Often reservoirs come in sets of the same name but different numbers, this compares the numbers
# and only keeps those with NO number or a number matching the NID name
out  = do.call(rbind, ll) %>% 
    filter(num == ll$nid$num | is.na(num)) %>%
    mutate(distance_m = as.numeric(st_distance(.,this.pt))) %>% 
    filter(is.na(stringDist) | stringDist > .75) %>% 
    arrange(priority, desc(stringDist)) %>% 
    filter(!duplicated(.)) 
    
min_not_0 = sort(out$distance_m)[2] + 250

if(sum(between(out$distance_m,.1,min_not_0)) > 0){
  out = out %>% filter(distance_m < 100)
}


out = out %>% mutate(distance_from_1 = as.numeric(st_distance(.,out[1,]))) %>% 
  arrange(priority, distance_from_1, desc(stringDist)) %>% 
  mutate(priority = 1:n())


out = out %>% arrange(distance_from_1)

# "Validated" data are those that are explicitly represented in a data source, the rest are interpreted
validated = c("FEMA", "osmpt", "osmline", 'GNIS')
all = out %>% filter(source != "NID")
pttmp = out %>% filter(source == "NID")
tmp = out  %>% filter(source %in% validated)

if(nrow(tmp) > 0){
out$priority[out$source == "NID"] = tmp$priority[which.min(abs(tmp$distance_from_1- pttmp$distance_from_1))]+1
}

# Move the NID reprpesention priority up if its less then 30 meters from a validated source
# if(nrow(tmp2) > 0){
#   out$priority[out$source == "NID"] = max(tmp2$priority) + .25
# }

# if pond, lake or reservoir in dam name, move FL priority to the end
if( grepl(paste(c('pond', 'lake', 'reservoir'), collapse = "|"), tolower(this.pt$dam_name))){
  out$priority[out$source == "NHDFL"] = 9
}


out$groupDist = rowSums(st_distance(out,out))
out$centroid = ifelse(min(out$groupDist) == out$groupDist, 1,0)

# Find how many resources have the NID representation
out = mutate(out, equal_nid_rep = (lengths(st_equals(out, this.pt)) > 0))
# Get the NID coordinates
pt = st_coordinates(this.pt)

HUC12 = AOI::bbox_get(out) %>% findHUC12() %>% dplyr::select(huc12, tohuc)
out = st_intersection(out, HUC12)

v = unique(c(out$HUC12, out$TOHUC))
start = v %>%  substring(11,12) %>% as.numeric() %>% order %>%  which.min()
end = start + 1
out$HUC12 = v[start]





# Starting with the ranked list:
  # Add the ID
  # Add the NID coordinates
  # add the delta X and Y between the NID and the other representations
  # drop geometry
  # Keep what we want
  # Sort by priority
  # Count the number of represetnations within 50 and 100 meters
  # Drop Priority

  st = AOI::aoi_get(state = "conus")  %>% st_transform(st_crs(out)) %>% dplyr::select(state = name)
  out = st_intersection(st,out)
  cou = AOI::aoi_get(state = out$state[1], county = "all")  %>% st_transform(st_crs(out)) %>% dplyr::select(county = name)
  out = st_intersection(cou,out)
  
  final = out %>% 
    mutate(
      ID = this.pt$ID,
      X = st_coordinates(.)[,1],
      Y = st_coordinates(.)[,2],
      realizations = nrow(tmp),
      dX = X - pt[1],
      dY = Y - pt[2]) %>% 
    st_drop_geometry() %>% 
    dplyr::select(ID, source, id1, id2, name, X, Y, priority, distance_m, distance_from_1,
           stringDist, realizations, dX,dY, on_network, 
           equal_nid_rep, huc12, county, state) %>% 
    arrange(distance_from_1, priority) %>% 
    mutate(rank = 1:n(),
      with50_of_nid = sum(distance_m < 50) - 1,
           with100_of_nid = sum(distance_m < 100) - 1) %>% 
    dplyr::select(-priority)

  
  ideal.name = final %>% filter(!source %in% c("INT", "NHDWB", "osmpolygon", "NHDFL")) %>% pull(name) %>% table()  %>% sort(decreasing = T)
  final$ideal.name = stringr::str_to_title(names(ideal.name)[1])
  final$ideal.x = final$X[which(final$rank == 1)]
  final$ideal.y = final$Y[which(final$rank == 1)]
  final$ideal.source = final$source[which(final$rank == 1)]

  # Return data.frame
  final
} 

     
