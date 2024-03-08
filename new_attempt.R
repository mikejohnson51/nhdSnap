library(sf)
library(dplyr)
library(stringdist)
library(mapview)
library(osmdata)



st = AOI::aoi_get(state = "conus", county = "all")  %>% 
  st_transform(5070) 

this.county = st[1,]


mapview(st[1,]) + these.dams

these.dams = all.dams[st[1,],]

all.dams   = readRDS("./data/nid_cleaned_5070.rds")
gnis.5070  = readRDS("./data/gnis_sf.rds")
obj.names = c('name', 'alt_name', "FEATURE_NAME", 'gnis_name')

ll = list()
for(i in 1:10){
  
dams  = st_transform(all.dams[i,], 5070)
if(dams$river == "UNKNOWN"){ dams$river = NA}
on.trib = grepl("TR|OS|TRIB", dams$river)
# Set the search area: searching within a search radius
bb  = st_buffer(this.county, 2500)

fl = nhdplusTools::get_nhdplus(bb) 

if(!is.null(nrow(fl))){
  fl = fl %>% 
    dplyr::select(ID = comid, gnis_name) %>%
    st_transform(5070)
} else {
  fl = NULL
}

wb =  nhdplusTools::get_waterbodies(bb) 

if(!is.null(nrow(wb))){
  wb = wb %>% 
  dplyr::select(ID = comid, gnis_name) %>%
  st_transform(5070)
} else {
  wb = NULL
}

osm_query = opq(st_transform(bb, 4326))
osm_dam   = osmdata_sf(add_osm_feature(osm_query, key = "waterway", value = "dam")) 
osm_ww    = osmdata_sf(add_osm_feature(osm_query, key = "waterway", value = c("river", "stream")))
osm_nat   = osmdata_sf(add_osm_feature(osm_query,  key = "natural", value = "water")) 

osm_dam_lines = prepOSM(osm_dam$osm_lines)
osm_ww_lines  = prepOSM(osm_ww$osm_lines)
osm_ww_poly   = prepOSM(osm_nat$osm_polygons)

nhd_int = wb_fl_intersect(wb = wb, fl, dams, names, "nhd_med")
osm_int = wb_fl_intersect(wb = osm_ww_poly, fl = osm_ww_lines, dams, names, type = "osm")

gnis =  mutate(gnis.5070[bb,], ID = FEATURE_ID)

info = list(osm_ww_poly    = osm_ww_poly,
            osm_ww_lines   = osm_ww_lines,
            osm_dams_lines = osm_dams_lines,
            gnis = gnis, 
            nhd_med_fl = fl, 
            nhd_med_wb = wb, 
            nhd_int = nhd_int,
            osm_int =  osm_int )

rank = c(1,1,1,2,2,2,0,0)

o = lapply(seq_along(info), function(x){
  name_dist_comp(dams, 
                 obj = info[[x]],
                 obj.names = obj.names,
                 obj.id = "ID", 
                 context = names(info)[x],
                 fl_list = list(fl, osm_ww_lines),
                 rank = rank[x])
}) 

out = do.call(rbind, o)  %>% 
  mutate(simular = jw < (.1+ min(jw)), closest = snap_dist < (10+ min(snap_dist))) %>% 
  arrange(rank, -simular, -closest, -snap_dist) %>% 
  #filter(jw < .35 | is.na(jw)) %>% 
  filter(snap_dist < 300) %>% 
  mutate(ord = 1:n(),
         onn = sum(on_network, na.rm = TRUE) > 0,
         onn2 = !is.na(dams$river),
         mainstem = all(!is.na(dams$river), !on.trib),
         on_network = sum(onn, onn2) > 0,
         onn = NULL, onn2 = NULL)
pts = st_as_sf(out, coords = c("X", "Y"), crs = 5070)
mapview(pts[1,])   + dams  + fl + wb

st = AOI::aoi_get(state = "conus", county = "all")  %>% 
  st_transform(st_crs(pts)) %>% 
  st_filter(pts[1,])

xx   = nhdplusTools::get_huc12(pts[1,])
nldi = dataRetrieval::findNLDI(location = st_transform(pts[1,], 4326))

out2 = out %>% 
  select(NIDID = damname, context, nearest_line_id) %>% 
  tidyr::pivot_wider(id_cols = NIDID, 
                     values_from = nearest_line_id, 
                     names_from = context)

out2[setdiff(names(info), names(out2))] <- NA

ll[[i]] = cbind(mutate(out2, huc12 = xx$huc12,
                             nldi.pip = nldi$comid, 
                             state = st$state_name, 
                             county = st$name, 
                             min_snap = min(out$snap_dist)), 
                select(out[1,], 
                       realization = context, 
                       dam.name, obj.name, jw, 
                       on_network, mainstem, 
                       chosen_snap = snap_dist, X, Y))

  message(i)
}


sep = do.call(rbind, ll)

pts = st_as_sf(sep, coords = c("X", "Y"), crs = 5070)

mapview(pts, col.regions = "red") + all.dams[9,]
