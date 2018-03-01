
load('auth.rdata')
api_key <- auth$key
format<-"rest"
query <- "Scotland"

baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.places.find&api_key=",api_key,sep="")
places <- NULL

getPlaces <- paste(baseURL, "&query=", query, "&format=", format, sep="")

getPlaces_data <- xmlRoot(xmlTreeParse(getURL(getPlaces),useInternalNodes = TRUE))

place_woeid <- xpathSApply(getPlaces_data, "//place", xmlGetAttr, "woeid") 
latitude <- xpathSApply(getPlaces_data, "//place", xmlGetAttr, "latitude")
longitude <- xpathSApply(getPlaces_data, "//place", xmlGetAttr, "longitude")
place_type <- xpathSApply(getPlaces_data, "//place", xmlGetAttr, "place_type")