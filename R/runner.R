install_github("mikejohnson51/NFHL")

all.dams   = readRDS("./data/nid_cleaned_5070.rds")
gnis.5070  = readRDS("./data/gnis_sf.rds")
source("./R/utils.R")
source("./R/query_cida.R")
source("./R/align_pt_nhd.R")

state = state.abb[!state.abb %in% c("HI", "AK")]

ind = st_nearest_feature(dams, st_transform(dd$osm_lines, 5070))

mapview(dams[1,]) + dd$osm_lines[3,]
xx = alignDams(dams[13:14,])

stringsim(tolower(dams$dam_name[4]), tolower(dd$osm_line$name), method = "cosine")

xx2 = xx %>% 
  st_as_sf(coords = c("X", "Y"), crs = 5070) %>% 
  st_transform(4326)

rm = xx2 %>% 
  select(ID, id1, distance_m) %>% 
  group_by(id1) %>% 
  filter(n() > 1) %>% 
  arrange(distance_m) %>% 
  slice(2:n()) %>% 
  st_drop_geometry() %>% 
  select(-distance_m)

xx3 = xx2 %>% 
  filter(!paste0(ID, id1) %in% paste0(rm$ID, rm$id1))

#   
library(leaflet)
leaflet() %>%
  addTiles() %>%
  addMarkers(data = st_transform(dams[13:14,],4326), label = ~ID) %>%
  addCircleMarkers(data = xx3, label = ~source)

ids = select(xx3, ID, source, id1) %>% 
  tidyr::pivot_wider(names_from = source, values_from = id1) 

fin = filter(xx3, rank == 1) %>% 
  dplyr::slice_min(distance_m, n = 1)
  select(ID, ideal.name,county, state, huc12) %>%
  # merge(ids) %>% 
  # select(-ID) %>% 
  st_transform(4326) %>% 
  mutate( X = st_coordinates(.)[,1],
          Y = st_coordinates(.)[,2],
          huc10 = substr(huc12,1,10))

leaflet() %>% 
  addTiles() %>% 
  addMarkers(data = fin)#, label = ~NID) %>% 
  addCircleMarkers(data = st_transform(dams, 4326), label = ~ID)

alignDams = function(dams){
 outs = list()
 for(i in 1:nrow(dams)){
    outs[[i]] =  runit(pt = dams[i,])
    cat(crayon::blue(i, "of", nrow(dams), "\n"))
 }
 bind_rows(outs)
}




align_pt_to_nhd(pt = dams[i,])


for(j in 1:length(state)){
  
outfile = paste0("./data/states_nid/", state[j],".rds")
  
if(!file.exists(outfile)){
  
  go.dams = dplyr::filter(all.dams, state == !!state[j])

for(i in 1:nrow(go.dams)){

tryCatch({
   r = align_pt_to_nhd(pt   = go.dams[i,])
   }, error = function(e){ r = NULL }
   )

  output[[i]] = r
  cat(crayon::blue(i, "of", nrow(go.dams), "\n"))
}
  
stats = bind_rows(output)
saveRDS(stats, file = outfile)
}
message(outfile, " already complete.")
}



ff = out_to_leaf(final)

