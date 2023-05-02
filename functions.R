# functions

## function for isochrone
get_neigh_com <- function(data_ville,time_max=15,perc_com=0.6,pop_data=NULL,nat_data=NULL){
  
  geom_com_peri <- spdf[which(spdf$com_name==data_ville$com_peri),]
  
  if(length(geom_com_peri) > 1){
    geom_com_peri <- geom_com_peri[which(geom_com_peri$bv2012_name==data_ville$com_centre),]
  }
  
  com_peri_code <- geom_com_peri@data$com_code
  
  centroid_com <- gCentroid(geom_com_peri)
  
  iso <- osrmIsochrone(loc = c(centroid_com@coords[1],centroid_com@coords[2]), breaks = seq(from = time_max,to = time_max, length.out = 1))
  class(iso)
  
  st_geometry(iso) <- st_collection_extract(x = st_geometry(iso), 
                                            type = "POLYGON")
  iso <- as(iso, "Spatial")
  
  #com_intersects <- gIntersects(spgeom1=iso,spgeom2=spdf, byid=T)
  
  com_intersection <- gIntersection(spgeom1=iso,spgeom2=spdf, byid=T, checkValidity = 2)
  
  com_intersection_data <- data.frame(com_name=NA,com_code=NA)
  
  for(j in 1:length(com_intersection@polygons)){
    id_j <- as.numeric(sub(".* ", "",com_intersection@polygons[[j]]@ID))
    area_over_j <- com_intersection@polygons[[j]]@area
    area_j <- spdf@polygons[[id_j]]@area
    ratio_over <- area_over_j/area_j
    if(ratio_over > perc_com){
      com_intersection_data[j,] <- spdf@data[id_j,c("com_name","com_code")]
    }else{
      com_intersection_data[j,] <- c(NA,NA)
    }
  }
  
  res_com_peri <- na.omit(com_intersection_data)
  
  if(!is.null(pop_data)){
    res_com_peri <- merge(res_com_peri,pop_data,
                          by.x=c("com_code","com_name"),
                          by.y=c("codgeo","com_name"), all.x=T)
  }
  
  if(!is.null(nat_data)){
    res_com_peri <- merge(res_com_peri,nat_data,
                          by=c("com_code","com_name"),
                          all.x=T)
  }
  
  return(res_com_peri)
  
}