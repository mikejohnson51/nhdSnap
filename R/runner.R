library(sf)
library(dplyr)
library(HydroData)
library(AOI)
library(nhdplusTools)

## Downlaod from here: https://nid.sec.usace.army.mil/ords/f?p=105:1::::::
raw = readxl::read_excel('/Users/mikejohnson/Downloads/NID2019_U.xlsx')
source('./R/align_pt_nhd.R')



## Make Spatial, remove AL and HI, c
dams  = raw %>% 
  filter(!is.na(LATITUDE), !is.na(LONGITUDE)) %>% 
  filter(!STATE %in% c('AK', "HI")) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4269)

# Test for Alabama Dams
go.dams = dams
output = list()

for(i in length(output):nrow(go.dams)){
  r <- NULL
  attempt <- 1
  
  while(is.null(r) && attempt <= 4 ) {
    attempt <- attempt + 1
    try({
      r = align_pt_to_nhd(pt   = go.dams[i,], 
                          id   = "NIDID", 
                          name = "DAM_NAME")
    }
    )
  } 
  
  output[[i]] = r
  
  message(i, " of ", nrow(go.dams))
}

stats = bind_rows(output)

stats = stats %>% filter(suggested_snap_dist <= 60)
nrow(stats) 
