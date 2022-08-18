#' getHarvestedArea
#'
#' Extract harvested area
#'
#' @param Stems output of getStems()
#' @return Simple feature object of area around harvested trees
#' @author Lennart Noordermeer \email{lennart.noordermeer@nmbu.no}
#' @examples
#' Stems=getStems(hprfile)
#' getHarvestedArea(Stems)
#' @export
getHarvestedArea=function(Stems){
  require(sp);require(sf)
  if(any(is.na(Stems$Latitude))){
    stop("Coordinates missing in Stems")
  }
  sf=st_as_sf(as.data.frame(Stems),coords=c("Latitude","Longitude"))
  st_crs(sf)=CRS("+proj=longlat +datum=WGS84")
  sf=st_transform(sf, CRS("+init=epsg:25832"))
  sf=st_buffer(sf,5) %>% st_union()
  plot(sf,col="red") %>% print()
  return(sf)
}
