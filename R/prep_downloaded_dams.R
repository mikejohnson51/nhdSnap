##################################################
## Project: ref_dams
## Script purpose: Preparing Data
##################################################

library(sf)
library(dplyr)

## Data can be downloaded from the public USACE site here:
### https://nid.sec.usace.army.mil/ords/f?p=105:1::::::

raw = readxl::read_excel('/Users/mikejohnson/github/dams/data/NID2019.xlsx')
bounds = readRDS("./data/state_bounds.rds")
names(raw)
go.dams = raw %>% 
  janitor::clean_names() %>% 
  select(state, dam_name, dam_former_name, other_dam_name, 
         nidid, otherstructureid, longitude, latitude, river, nid_height, nid_storage) %>% 
  filter(!state %in% c("HI", "AK", "PR", "GU")) %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  merge(bounds, by = 'state') %>%
  mutate(flag = !(abs(latitude)  <= abs(ymax) &
                  abs(latitude)  >= abs(ymin) &
                  abs(longitude) >= abs(xmax) &
                  abs(longitude) <= abs(xmin))) %>%
  filter(flag == FALSE) %>%
  select(-flag) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(5070) %>% 
  mutate(ID = paste0(nidid, ifelse((!is.na(otherstructureid)), paste0("-", otherstructureid),"")))

saveRDS(go.dams, file = "./data/nid_cleaned_5070.rds")

## GNIS download March 2020
## https://www.usgs.gov/core-science-systems/ngp/board-on-geographic-names/download-gnis-data

gnis = data.table::fread('/Users/mikejohnson/Downloads/NationalFile_20200301.txt')

gnis.5070 = gnis %>% filter(FEATURE_CLASS == "Dam") %>% 
  st_as_sf(coords = c("PRIM_LONG_DEC", "PRIM_LAT_DEC"), crs = 4269) %>% 
  st_transform(5070)

saveRDS(gnis.5070, file = "./data/gnis_sf.rds")


