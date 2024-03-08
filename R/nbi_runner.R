
# ----- Ingest --------------------------------------------------------------

## https://www.fhwa.dot.gov/bridge/nbi/format.cfm
meta = read.csv('/Volumes/GIS_Johnson/UFOKN/bridges/meta_nib.csv', stringsAsFactors = F, header = F)

meta$names = paste(meta$V2, meta$V1, sep = "_") %>% make.names()

meta$V4[is.na(meta$V4)] = meta$V3[is.na(meta$V4)] 

txt2 = '/Users/mikejohnson/Downloads/2019HwyBridgesNoDelimiterAllStates.txt'

rawDF = readr::read_fwf(txt2,
                        col_positions = readr::fwf_positions(
                          start = meta$V3,
                          end = meta$V4,
                          col_names = meta$names),
                        col_types = paste(rep('c', nrow(meta)), collapse = ""))

rawDF = rawDF %>% janitor::clean_names()
names(rawDF)

## Remove GUAM and the Virgin Islands (34 total dams)
rawDF = rawDF %>% filter(!state_code_1 %in% c("66", "78"))

nbi <- rawDF[,'state_code_1'] %>% data.frame()

nbi$fips = substr(nbi$state_code_1, 1,2)
nbi$NIB  = rawDF$structure_number_8
nbi      = nbi %>% select(-state_code_1)


# ----- stateCode -------------------------------------------------------------

# NOTE:  despite the coding guide, the state codes are only two characters long, to fill the thrid character they add a 4!! WHYYYYYYYY

# Create a vector of state codes, named by fips codes
bounds = readRDS("./data/state_bounds.rds")

nbi =  merge(nbi, bounds)

# table(nbi$stateCode) %>% sum()
# nrow(rawDF) ## Lost 34 Bridges

# ----- countyCode ------------------------------------------------------------  

# TODO: add countyCode

# ----- latitude --------------------------------------------------------------

# From the coding guide:
#
#   Item 16 - Latitude  (XX degrees XX minutes XX.XX seconds)     8 digits
#   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
#   record and code the latitude of each in degrees, minutes and seconds to
#   the nearest hundredth of a second (with an assumed decimal point).

latDMS <- rawDF$latitude_16

# Convert '00000000' to NA
latDMS[stringr::str_detect(latDMS,'00000000')] <- NA

# ----- Deal with high latitudes
highLatMask <- !is.na(latDMS) & as.numeric(latDMS) > 7.5e7

# cbind(nbi[highLatMask,],
#       rawDF[highLatMask, 
#       c('State.Code_1','Latitude_16','Longitude_17')], 
#       row = which(highLatMask))

# State.Code_1 stateCode State.Code_1 Latitude_16 Longitude_17    row
# 01           AL        014          87304350    033122220       15318
# 24           MD        243          77503000    038820000       241063

# Both of these have lat/long switched ... flip them back
latDMS[which(highLatMask)] <- stringr::str_sub(rawDF$longitude_17[which(highLatMask)],2,9)
#lonDMS[which(highLatMask)] <- paste0('0',rawDF$Latitude_16[which(highLatMask)])

# ----- Deal with low latitudes
lowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e7

bad = cbind(nbi[lowLatMask,],
            rawDF[lowLatMask, c('state_code_1','latitude_16','longitude_17')], 
            row = which(lowLatMask))
#dim(bad) ## 79 bad ones 
#table(bad$stateCode)
# AK AL AZ CO DC DE HI KS MD  MT NC NJ PR UT VA 
# 1  4  1  1  2  1  1  2  34  5  1  6  2  1  17

# AK  leading 0 should move to end
# AL  1 = 00000108 (presumed no data), the rest move 0 from beginning to end
# AZ  leading 0
# CO  00000100 (presumed no data)
# DC "00" leading rather then ending
# HI  00000100 (presumed no data)
# KS  lats <0 presumed no data
# MD  10 = "0" leading rather then ending, 24 with "00" 
# MT  "0000" leading zeros
# NC  leading 0
# NJ  leading 0
# PR  negative -100000, leading 0
# UT  "0000" leading 
# VA  12 "00", 5 "0"

switch_zeros = function(str, max_n = 4){
  char = nchar(str)
  leading_zeros = min(char - nchar(as.numeric(str)), max_n)
  str = substring(str, leading_zeros+1, char)
  str = paste0(str, paste(rep(0, leading_zeros), collapse = ""))
  str
}

bad$fixlat = sapply(bad$latitude_16, FUN = switch_zeros)

latDMS[bad$row] = bad$fixlat

newLowLatMask <- !is.na(latDMS) & as.numeric(latDMS) < 1e7
#table(nbi$stateCode[newLowLatMask])

bad = cbind(nbi[newLowLatMask,],rawDF[newLowLatMask, c('state_code_1','latitude_16','longitude_17')], row = which(newLowLatMask))
# State.Code_1     stateCode   State.Code_1       Latitude_16 Longitude_17    row
# 01               AL          014                00000108    000000108       15958
# 08               CO          088                00000100    000000100       68380
# 5                HI          159                00000100    000000100       106631
# 20               KS          207                -1000000    001000000       188824
# 0                KS          207                -5000000    097334544       189929
# 2                PR          721                -1000000    -01000000       615272

## The remaining 6 bridges are either a bad flag or unsavable:

latDMS[bad$row] <- NA

# ----- Convert to decmial degrees -----

deg <- as.numeric(stringr::str_sub(latDMS, 1, 2))
min <- as.numeric(stringr::str_sub(latDMS, 3, 4))
sec <- as.numeric(stringr::str_sub(latDMS, 5, 8))/100

nbi$latitude <- deg + min/60 + sec/3600

# Remove values that are out of USA domain

badMask <- nbi$latitude < 10 | nbi$latitude > 80
nbi$latitude[badMask] <- NA
#nbi = nbi %>% na.omit()

# flag those outside their state domain
nbi = nbi %>% mutate(flag = !(latitude <= ymax & latitude >= ymin))

# table(nbi$state[nbi$flag])
# AL AZ CO CT DC DE IA KS MD MN MS NE NM NV NY OR PA PR  SC WA WY 
# 3  1  4  2  1  1  1  1  38  2  1  2  1  3  1  2  2  12  2  1  1

## MD as a big problem
##sub = nbi %>% filter(state_abbr == "MD", flag == T)

# ----- longitude -------------------------------------------------------------

#   Item 17 - Longitude  (XX degrees XX minutes XX.XX seconds)     9 digits
#   For bridges on STRAHNET and STRAHNET Connector highways and on the NHS,
#   record and code the longitude of each in degrees, minutes and seconds to
#   the nearest hundredth of a second (with an assumed decimal point).  A
#   leading zero shall be coded where needed.

lonDMS <- rawDF$longitude_17

# sum(is.na(lonDMS))
# 13

# Convert NA to '000000000'
lonDMS[is.na(lonDMS)] <- '000000000'
lonDMS[lonDMS == "-01000000"] <- '000000000'

# TODO:  Need to be much more careful about how we correct longitudes

# check for out-of-domain longitudes
highLonMask <- as.numeric(lonDMS) > 1.8e8
# lonDMS[highMask]
# [1] "863832130" "765833300" "770100000" "770301400" "934032380"
# [6] "933958820" "933950760" "933517040" "933910020" "933903230"
# [11] "933933510" "933707000" "933707670" "933916700" "933612730"
# [16] "760534586" "763840000" "760261940" "771700000" "772433000"
# [21] "761041400" "754727720" "764311730" "763755998" "705274592"
# [26] "870402900" "933224800" "933217260" "933211100" "654444390"
# [31] "655560104" "660941300" "655590190" "660730000" "654525520"
# 
# They all appear to be missing a leading "0". 
lonDMS[highLonMask] <- paste0("0", stringr::str_sub(signif(as.numeric(lonDMS)[highLonMask], 8), 1, 8))

badMask <- stringr::str_detect(lonDMS,'00000000')
lonDMS[badMask] <- NA

lowLonMask <- !is.na(lonDMS) & as.numeric(lonDMS) < 1e7

badLons <- as.numeric(lonDMS[lowLonMask])

lonDMS[lowLonMask] <- 
  ifelse(badLons > 1e6 , paste0(stringr::str_sub(lonDMS[lowLonMask], 2,9), "0"),
         ifelse(badLons > 1e5, paste0(stringr::str_sub(lonDMS[lowLonMask], 3,9), "00"),
                ifelse(badLons > 1e4, paste0(stringr::str_sub(lonDMS[lowLonMask], 4,9), "000"),
                       ifelse(badLons > 1e3, paste0(stringr::str_sub(lonDMS[lowLonMask], 5,9), "0000"),
                              paste0(stringr::str_sub(lonDMS[lowLonMask], 7,9), "000000")))))

lowLonMask <-  !is.na(lonDMS) & as.numeric(lonDMS) < 6.5e7

lonDMS[lowLonMask] = NA

#######################################################

# NOTE:  these are degrees W

deg <- as.numeric(stringr::str_sub(lonDMS, 1, 3))
min <- as.numeric(stringr::str_sub(lonDMS, 4, 5))
sec <- as.numeric(stringr::str_sub(lonDMS, 6, 9))/100

nbi$longitude <- -1 * (deg + min/60 + sec/3600)

# Remove values that are out of domain

badMask <- nbi$longitude < -180 | nbi$longitude > -65
nbi$longitude[badMask] <- NA

nbi = nbi %>% mutate(flag2 = !(abs(longitude) >= abs(xmax) & abs(longitude) <= abs(xmin)))

table(nbi$state[nbi$flag2])


# Remove remaining problematic lats and lons
bad1 <- !is.na(nbi$longitude) & !is.na(nbi$latitude) & nbi$latitude > 50 & nbi$longitude > -129
nbi$latitude[bad1] <- NA
nbi$longitude[bad1] <- NA
bad2 <- !is.na(nbi$longitude) & !is.na(nbi$latitude) & nbi$latitude >27.2 & nbi$latitude < 32.5 & nbi$longitude > -79
nbi$latitude[bad2] <- NA
nbi$longitude[bad2] <- NA


#---- yearBuilt ----------------------------------------------------------------
nbi$yearBuilt <- as.numeric(rawDF$year_built_27) #adding the year the bridge was built
nbi$age <- 2019 - nbi$yearBuilt
#------------------------------------------------------------------------------
#---- averageCarCount ---------------------------------------------------------
nbi$averageCarCount <- as.numeric(rawDF$average_daily_traffic_29)
#-----------------------------------------------------------------------------
#---- water (indicates whether the bridge goes over water)---------------------

nbi$Waterway <- rawDF$waterway_adequacy_71
nbi$water <- ifelse(nbi$Waterway == "N", 0, 1) 
nbi$waterwayAdequacy <- as.numeric(nbi$Waterway)
nbi$Waterway <- NULL
#------------------------------------------------------------------------------
# #---------laneCount------------------------------------------------------------
# # There are so many data points that are apparently totally wrong that this variable may not be usable.
# # Roadway width will give us similar information.
# nbi$laneCount <- as.numeric(rawDF$TRAFFIC_LANES_ON_028A)
# nbi$underLaneCount <- as.numeric(rawDF$TRAFFIC_LANES_UND_028B)
# #----------------------
#-------------channelCondition----------------------------------------------
# N Not applicable. Use when bridge is not over a waterway (channel).
# 9 There are no noticeable or noteworthy deficiencies which affect the condition of the channel.
# 8 Banks are protected or well vegetated. River control devices such as spur dikes and embankment
#   protection are not required or are in a stable condition.
# 7 Bank protection is in need of minor repairs. River control devices and embankment protection 
#   have a little minor damage. Banks and/or channel have minor amounts of drift.
# 6 Bank is beginning to slump. River control devices and embankment protection have widespread 
#   minor damage. There is minor stream bed movement evident. Debris is restricting the channel slightly.
# 5 Bank protection is being eroded. River control devices and/or embankment have major damage. 
#   Trees and brush restrict the channel.
# 4 Bank and embankment protection is severely undermined. River control devices have severe damage. 
#   Large deposits of debris are in the channel.
# 3 Bank protection has failed. River control devices have been destroyed. Stream bed aggradation,
#   degradation or lateral movement has changed the channel to now threaten the bridge and/or approach roadway.
# 2 The channel has changed to the extent the bridge is near a state of collapse.
# 1 Bridge closed because of channel failure. Corrective action may put back in light service.
# 0 Bridge closed because of channel failure. Replacement necessary. 

nbi$verticalClearance <- as.numeric(rawDF$minimum_vertical_underclearance_54b )


nbi_sf= nbi %>% 
  filter(!is.na(longitude), !is.na(latitude)) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

saveRDS(nbi, file = "./data/nbi_2019.rds")
saveRDS(nbi_sf, file = "./data/nbi_sf_2019.rds")
dim(nbi)

