
\< \# nhdSnap

<!-- badges: start -->

<!-- badges: end -->

  - “nexusX” = the X coordinate (CRS:5070) of the NHD flowline vertix
    closest to the input point  
  - “nexusY” = the Y coordinate (CRS:5070) of the NHD flowline vertix
    closest to the input point  
  - “FL\_COMID” = the COMID of the nearest NHDPlusV2 flowline  
  - “FL\_REACHCODE” = the REACHCODE of the nearest NHDPlusV2 flowline  
  - “FL\_WBCOMID” = the COMID of the waterbody assocaited with the
    nearest NHDPlusV2 flowline (no association = 0)  
  - “FL\_NAME” = the GNIS NAME of the nearest NHDPlusV2 flowline  
  - “FL\_FTYPE” = the FTYPE of the nearest NHDPlusV2 flowline  
  - “WB\_COMID” = the COMID of the associated NHDPlusV2 Waterbody (if no
    direct association a 250m buffer was searched)  
  - “WB\_REACHCODE” = the REACHCODE of the associated NHDPlusV2
    Waterbody (if no direct association a 250m buffer was searched)  
  - “WB\_GNIS\_NAME” = the GNIS NAME of the associated NHDPlusV2
    Waterbody (if no direct association a 250m buffer was searched)  
  - “WB\_FTYPE” = the FTYPE of the associated NHDPlusV2 Waterbody (if no
    direct association a 250m buffer was searched)  
  - “ptID” = the ID of the input point defined in function
    (e.g. NID\_ID)
  - “ptName” = the NAME of the input point defined in function
    (e.g. DAM\_NAME)
  - “nexus\_to\_pt” = the distance (m) from the nexus point to the input
    point  
  - “nexus\_to\_wb” = the distance (m) from the nexus point to the water
    body
  - “endX” = the X coordinate (CRS:5070) of the NHD flowline cap vertix
    closest to the input point  
  - “endY” = the Y coordinate (CRS:5070) of the NHD flowline cap vertix
    closest to the input point  
  - “end\_to\_pt” = the distance (m) from the end point to the input
    point  
  - “end\_to\_wb” = the distance (m) from the end point to the input
    point  
  - “wb\_sim” = the text simularity between the waterbody GNIS name and
    the dam name(s) and river  
  - “fl\_sim” = the text simularity between the NHD flowline GNIS name
    and the dam name(s) and river,  
  - “suggested\_snap” = the nexus or end point with the closest textual
    simularity
  - “suggested\_snap\_dist” = the distance of the sugested SNAP point to
    the existing point location  
  - “max\_sim” = the maximum textual simuality of the feature to either
    the water body of the flowline
