#' Find correct woe_id for photosSearch query
#' 
#' @param place Text string describing the place for the query
#'
#' @return A dataframe of places incl wo_id to be used in the photosSearch function
#' @export
#' @name findPlaces


findPlaces <- function(place){

newURSltester<-"https://api.flickr.com/services/rest/?method=flickr.places.find&api_key=e921f86f20cf6766440da8a7394890ee"
query<- place

getPhotos <- paste0(newURSltester
                    ,"&query=",query)

getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                                       (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                                       ,useInternalNodes = TRUE ))

place_id<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"place_id")                 
woe_id<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"woeid")                 
latitude<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"latitude")                 
longitude<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"longitude")                 
place_url<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"place_url")                 
woe_name<-xpathSApply(getPhotos_data,"//place",xmlGetAttr,"woe_name")                 

places<-data.frame(cbind(woe_name,place_url,place_id,woe_id,latitude,longitude))

return(places)
}
