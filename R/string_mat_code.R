name_dist_comp = function(dams, obj, 
                          dam.names = c('dam_name', 'dam_former_name', 'other_dam_name', 'river'), 
                          obj.names, 
                          dam.id = "ID", 
                          obj.id, 
                          context = NA , 
                          fl_list, 
                          rank = NA){

  
  if(is.null(obj))   {return(NULL)}
  if(nrow(obj) == 0) {return(NULL)}
  
  obj   = st_transform(obj, 5070)
  cross = expand.grid(dam.names, obj.names, stringsAsFactors = F)

  name.mat = list()
  
  for(i in 1:nrow(cross)){
    var1 = ifelse(tolower(dams[[cross$Var1[i]]]) ==  " ", NA, tolower(dams[[cross$Var1[i]]]))
    var1 = gsub("\\s*\\([^\\)]+\\)","",as.character(var1))
    var1 = qdap::replace_number(var1)
    var2 = ifelse(tolower(obj[[cross$Var2[i]]])  ==  " ", NA, tolower(obj[[cross$Var2[i]]]))
    var2 = gsub("\\s*\\([^\\)]+\\)","",as.character(var2))
    var2 = qdap::replace_number(var2)
    if(length(var1) > 0 & length(var2) > 0){
      name.mat[[i]] <- stringdistmatrix(var1, var2, method = "jw")
    } else {
      name.mat[[i]] = NA
    }
  }
  
  name.mat = name.mat[lengths(name.mat) > 0]
  
  A      = do.call(rbind, name.mat)
  
  if(all(is.na(A))){
      # obj.name = select(st_drop_geometry(closest_obj), dplyr::any_of(obj.names))
      # if(ncol(obj.name) == 0){ obj.name = NA}
      # obj.name = obj.name[!is.na(obj.name)][1]
      # dam.name = select(st_drop_geometry(dams), dplyr::any_of(dam.names), -river)
      # dam.name = dam.name[!dam.name %in% c(NA, "UNKNOWN", 0) & !is.na(dam.name)][1]
     dam.name = NA
     obj.name = NA
    } else {
      A      = which(A==min(A, na.rm = TRUE), arr.ind=T)
      dam.name = select(st_drop_geometry(dams), cross$Var1[A[,1]])
      obj.name = unique(select(st_drop_geometry(obj),  cross$Var2[A[,1]])[A[,2],])
    }

  if(length(name.mat) == 0) { 
    dist.name = NULL
  } else {
    dist.name = do.call(pmin, c(name.mat, list(na.rm = TRUE)))
  }
  
  if(all(is.na(dist.name))){
    sim = 1
  } else {
    sim      = min(dist.name, na.rm = TRUE)
  }

  flag     = FALSE
  
  if(sim < .35){
    most_sim = which(dist.name <= sim + .1)
    sim_obj = obj[most_sim,]
    sim_obj$jw  = dist.name[most_sim]
    sim_obj$dist = as.numeric(st_distance(sim_obj, dams))
    
    closest_obj = arrange(sim_obj, jw, dist) 
    if(min(closest_obj$dist) > 250 & sim < .1) { flag  = TRUE }
  }
    
  if(sim >= .35 | flag){
    dist             = as.numeric(st_distance(dams, obj))
    within_10        = which(dist <= (min(dist, na.rm = TRUE) + 10))
    closest_obj      = obj[within_10,]
    closest_obj$dist = dist[within_10]
    closest_obj$jw   = if(is.null(dist.name) | all(is.na(dist.name))){
      NA
    } else {
      dist.name[within_10]
    }
  }
  
  if(st_geometry_type(closest_obj)[1] == "POLYGON"){
    closest_obj$area = as.numeric(st_area(obj[within_10,]))
  } else {
    closest_obj$area = NA
  }

  closest_obj = arrange(closest_obj, jw, -area, dist) %>% 
    slice(1)

  # for(i in 1:nrow(cross)){
  #   var1 = ifelse(tolower(dams[[cross$Var1[i]]]) ==  " ", NA, tolower(dams[[cross$Var1[i]]]))
  #   var1 = gsub("\\s*\\([^\\)]+\\)","",as.character(var1))
  #   var1 = qdap::replace_number(var1)
  #   var2 = ifelse(tolower(closest_obj[[cross$Var2[i]]])  ==  " ", NA, tolower(closest_obj[[cross$Var2[i]]]))
  #   var2 = gsub("\\s*\\([^\\)]+\\)","",as.character(var2))
  #   var2 = qdap::replace_number(var2)
  #   if(length(var1) > 0 & length(var2) > 0){
  #     name.mat[[i]] <- stringdistmatrix(var1, var2, method = "jw")
  #   } else {
  #     name.mat[[i]] = NA
  #   }
  # }
  # 
  # A      = do.call(rbind, name.mat)
  # 
  # if(all(is.na(A))){
  #   obj.name = select(st_drop_geometry(closest_obj), dplyr::any_of(obj.names))
  #   if(ncol(obj.name) == 0){ obj.name = NA}
  #   obj.name = obj.name[!is.na(obj.name)][1]
  #   dam.name = select(st_drop_geometry(dams), dplyr::any_of(dam.names), -river)
  #   dam.name = dam.name[!dam.name %in% c(NA, "UNKNOWN", 0) & !is.na(dam.name)][1]
  # } else {
  #   A      = which(A==min(A, na.rm = TRUE), arr.ind=T)
  #   dam.name = select(st_drop_geometry(dams), cross$Var1[A[1]])
  #   obj.name = select(st_drop_geometry(obj), cross$Var2[A[1]])[A[2],]
  # }

  if(st_geometry_type(closest_obj) == "POINT"){
    tmp = data.frame(
      nearest_line_id = closest_obj[[obj.id]],
      snap_dist       = as.numeric(st_distance(closest_obj, dams)),
      X               = st_coordinates(closest_obj)[,1],
      Y               = st_coordinates(closest_obj)[,2])
  } else {
    closest_obj = suppressWarnings({ st_cast(closest_obj, "LINESTRING") })
    
    tmp = suppressWarnings({
      maptools::snapPointsToLines(as_Spatial(dams), as_Spatial(closest_obj), idField = obj.id) %>% 
        st_as_sf()  %>% 
        mutate(X = st_coordinates(.)[,1],
               Y = st_coordinates(.)[,2],) %>% 
        st_drop_geometry()
  })
  }

  fl_list = Filter(Negate(is.null), fl_list)
  
  # pp = suppressWarnings({ 
  #   lapply(1:length(fl_list), function(x) { 
  #     nrow(st_intersection(fl_list[[x]], closest_obj)) })
  # })
 
  data.frame(cbind(damname = dams[[dam.id]],
                   rank = rank, 
                   context    = context,
                   dam.name = as.character(dam.name),
                   obj.name = as.character(obj.name),
                   most_simular_to = ifelse(is.null(dist.name), 
                                                           toJSON(NA),
                                                           toJSON(as.numeric(obj[which.min(dist.name),][[obj.id]]))),
                   jw = ifelse(is.null(dist.name), NA, ifelse(all(is.na(dist.name)), NA, min(dist.name, na.rm = TRUE))), 
                   tmp)) 
}

wb_fl_intersect = function(wb, fl, dams, obj.names, dam.names,  type = ""){
  
suppressWarnings({
  if(!is.null(wb) & !is.null(fl)){
    ls_wb = st_cast(wb, "LINESTRING")
    x = st_snap( lwgeom::st_snap_to_grid(ls_wb, size = 1), fl, tolerance = 1) %>% 
      st_intersection(fl) 

    if(nrow(x) > 1){
   
     inds = which(names(x) %in% obj.names)
     cols = list()
     
     for(i in 1:length(inds)){
       var1 = as.character(select(st_drop_geometry(dams), any_of(dam.names)))
       var1 = ifelse(tolower(var1) ==  " ", NA, tolower(var1))
       var1 = gsub("\\s*\\([^\\)]+\\)","",as.character(var1))
       var1 = qdap::replace_number(var1)
       var2 =  x[[inds[i]]]
       var2 = ifelse(tolower(var2)  ==  " ", NA, tolower(var2))
       var2 = gsub("\\s*\\([^\\)]+\\)","",as.character(var2))
       var2 = qdap::replace_number(var2)
       A = stringdistmatrix(var1, var2, method = "jw")
       A      = which(A==min(A, na.rm = TRUE), arr.ind=T)
       cols[[i]] = A[,2]
     }
     
     subs = which(table(unlist(cols)) == 2)
     if(length(subs) == 0){ subs = 1:nrow(x)  }
     x = x[subs,]
     ind = which.min(st_distance(x, dams))
     x = x[ind,]
     x[setdiff(obj.names, names(x))] <- NA
     x$type = type
     mutate(x,ID =  paste0(ID.1, "-",ID))
    } else {
      NULL
    }
  } else {
    NULL
  }
})
}


prepOSM = function(obj){
  if(!is.null(obj)){
    obj %>% 
      tibble::rownames_to_column("ID") %>% 
      st_transform(5070)
  } else {
    NULL
  }
}
