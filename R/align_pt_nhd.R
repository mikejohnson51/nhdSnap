align_pt_to_nhd = function(pt, id = "NIDID", name = "DAM_NAME" ){
  
  # Current Dam
  this.pt = st_transform(pt, 5070)
  
  # Associated NHD Flowline
  
  fl = HydroData::findNHD(comid = discover_nhdplus_id(this.pt))[[2]] %>% 
    st_transform(5070)
  
  this.fl = fl[st_nearest_feature(this.pt, fl),]
  
  # Cast LINESTRING to POINTs
  all.fl.p = st_cast(this.fl$geometry, "POINT")
  
  # Isolate start and end node and indetify closest to dam
  E1 = as.numeric(st_distance(all.fl.p[1], this.pt))
  E2 = as.numeric(st_distance(all.fl.p[length(all.fl.p)], this.pt))
  
  this.nhd.end = if(E1 < E2) { all.fl.p[1] } else { all.fl.p[length(all.fl.p)]}
  dist.pt.end = min(E1,E2)
  
  # Calculate distance for dam to all NHD flowline "vertices"
  all.dist = as.numeric(st_distance(all.fl.p, this.pt))
  # Get Geometry of the closest...
  nexus = all.fl.p[which.min(all.dist)]
  # .... and the distance too...
  dist.dam.nexus = all.dist[which.min(all.dist)]
  dist.dam.end   = as.numeric(st_distance(this.nhd.end, this.pt))
 
  associatedWB = (this.fl$wbareacomi != 0)
  
  all.wb = NULL
  
  if(associatedWB){ all.wb = HydroData::findWaterbodies(comid = this.fl$wbareacomi)$waterbodies }
    
  if(is.null(all.wb)){
  # Build a bounding box around the end point and nexus...
  # and buffer by 250 meters
  this.bb = st_as_sf(c(nexus, this.nhd.end)) %>% 
    bbox_get() %>%
    st_buffer(250) %>% 
    st_transform(4269) 
  
  ## Query CIDA geoserver for NHD waterbody polygons
  
  all.wb =  findWaterbodies(this.bb)$waterbodies

  }
  
  ## If there is a waterbody within 250m of the nexus/NHD endpoint ...
  if(!is.null(all.wb)){
    
    all.wb = all.wb %>% st_transform(5070)
    this.wb = all.wb[st_nearest_feature(nexus, all.wb),]
    
    dist.wb.end = as.numeric(st_distance(this.wb, this.nhd.end))
    dist.wb.nexus = as.numeric(st_distance(this.wb, nexus))
    this.wb.comid = this.wb$comid
    this.wb.reachcode = this.wb$reachcode
    this.wb.name = this.wb$gnis_name
  } else {
    dist.wb.end <- dist.wb.nexus <- this.wb.comid <- 
    this.wb.reachcode <- this.wb.name <- NA
  }
  
  ## Build output...
  
  nexus.coords = st_coordinates(nexus)
  end.coords   = st_coordinates(this.nhd.end)
  
  df = data.frame("nexusX" = nexus.coords[1], 
             "nexusY" = nexus.coords[2],
             
             "FL_COMID" = this.fl$comid, 
             "FL_REACHCODE" = this.fl$reachcode, 
             "FL_WBCOMID"   = this.fl$wbareacomi,
             'FL_NAME' = this.fl$gnis_name,
             "FL_FTYPE" = this.fl$ftype,
             
      
             "WB_COMID"     = this.wb.comid, 
             "WB_REACHCODE" = this.wb.reachcode, 
             "WB_GNIS_NAME" = this.wb.name,
             "WB_FTYPE" = this.wb$ftype,
             
             "ptID"   = this.pt[[id]], 
             "ptName" = this.pt[[name]],
             
             'nexus_to_pt'  = dist.dam.nexus, 
             'nexus_to_wb'  = dist.wb.nexus,
             "endX"   = end.coords[1],
             "endY"   = end.coords[2],
             'end_to_pt'  = dist.dam.end,
             'end_to_wb'   = dist.wb.end,
             stringsAsFactors = F
  )
  
  ## Implementing this from the super cool idea of Daniel W
  
  df$wb_sim = max(stringdist::stringsim(tolower(df$WB_GNIS_NAME), tolower(this.pt$DAM_NAME)),
      stringdist::stringsim(tolower(df$WB_GNIS_NAME), tolower(this.pt$OTHER_DAM_NAME)),
      stringdist::stringsim(tolower(df$WB_GNIS_NAME), tolower(this.pt$DAM_FORMER_NAME)),
      stringdist::stringsim(tolower(df$WB_GNIS_NAME), tolower(this.pt$RIVER)), na.rm = T)
  
  df$fl_sim = max(stringdist::stringsim(tolower(df$FL_NAME), tolower(this.pt$DAM_NAME)),
      stringdist::stringsim(tolower(df$FL_NAME), tolower(this.pt$OTHER_DAM_NAME)),
      stringdist::stringsim(tolower(df$FL_NAME), tolower(this.pt$DAM_FORMER_NAME)),
      stringdist::stringsim(tolower(df$FL_NAME), tolower(this.pt$RIVER)), na.rm = T)
  
  
  tmp.wb.sim = ifelse(is.na(df$wb_sim) || is.infinite(df$wb_sim), 0, df$wb_sim)
  tmp.fl.sim = ifelse(is.na(df$fl_sim)|| is.infinite(df$fl_sim), 0, df$fl_sim)

  closer_to_wb = tmp.wb.sim > tmp.fl.sim
  
  if(closer_to_wb){
    ind = which.min(c(df$nexus_to_wb, df$end_to_wb))
    df$suggested_snap = ifelse(ind == 1, "NEXUS", "END")
    df$suggested_snap_dist = ifelse(ind == 1, df$nexus_to_wb, df$end_to_wb)
  } else {
    ind = which.min(c(df$nexus_to_pt, df$end_to_pt))
    df$suggested_snap = ifelse(ind == 1, "NEXUS", "END")
    df$suggested_snap_dist = ifelse(ind == 1, df$nexus_to_pt, df$end_to_pt)
  }
  
  df$max_sim = max(df$fl_sim, df$wb_sim)
  
  df
  
}
