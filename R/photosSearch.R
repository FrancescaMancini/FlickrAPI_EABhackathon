#' Query Flickr API and build a dataframe of metadata
#'
#' This function uses the Flickr API to get metadata about the images
#' 
#' @param year_range The year to search between (inclusive). For example c(2005,2007) will search for images from 2005, 2006 and 2007
#' @param text  Set keywords so that query returns pictures with the text (e.g. "bird")
#' @param woe_id A 32-bit identifier that uniquely represents spatial entities (e.g. 12578048 is Scotland). These ids can be found using the \code{findPlaces} function
#' @param has_geo Logical, should the function only return records that have spatial data.
#'
#' @return A dataframe of metadata
#' @export
#' @import XML
#' @import httr
#' @import RCurl
#' @name photosSearch
#' @seealso findPlaces
#' @examples
#' # run a workflow, using the logistic regression model
#' \dontrun{
#'
#' photosSearch(c(2005,2006), "bird", "12578048", TRUE)
#'
#' }

photosSearch <-
function(year_range,
         text,          
         woe_id,        
         has_geo = TRUE){
  
 api_key <- auth$key
 perpage <- "250"
 format<-"rest"
 extras <- "date_taken,geo,tags,license"
 baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL
 pics<-NULL
 year_range <- seq(min(year_range), max(year_range), 1)
 
 #API only returns 400 results per query so it is neccessary to loop through months to obtain all the results
 
 for (y in year_range){                     
   
   firstDate <- as.Date(paste0(y, "-01-01"))
   
   nDays <- length(seq(as.Date(firstDate), as.Date(paste0(y, "-12-31")), by="+1 day"))
   
   month <- round(seq(from = 1, to = nDays, length.out = 12))
   
   for(m in tail(month, -1)){
     
     mindate <- firstDate
     maxdate <- firstDate + m 
     
     getPhotos <- paste(baseURL,
                        "&text=", text,
                        "&min_taken_date=", as.character(mindate),
                        "&max_taken_date=", as.character(maxdate),
                        "&woe_id=", woe_id,
                        "&has_geo=", has_geo,
                        "&extras=", extras,
                        "&per_page=", perpage,
                        "&format=", format,
                        sep = "")
     
     getPhotos_data <- xmlRoot(xmlTreeParse(getURL(getPhotos,
                                                   ssl.verifypeer = FALSE,
                                                   useragent = "flickr")))
     
     #results are returned in different pages so it is necessary to loop through pages to collect all the data
     #parse the total number of pages
     pages_data <- data.frame(xmlAttrs(getPhotos_data[["photos"]]))
     pages_data[] <- lapply(pages_data, FUN = function(x) as.integer(as.character(x)))
     total_pages <- pages_data["pages",]
     total <- pages_data["pages",]
     
     if(total > 0){
       
       pics_tmp <- NULL
       
       # loop thru pages of photos and save the list in a DF
       for(i in c(1:total_pages)){
         
         getPhotos <- paste(baseURL
                            ,"&text=",text,"&min_taken_date=",mindate,
                            "&max_taken_date=",maxdate,"&woe_id=",woe_id,
                            "&has_geo=",has_geo,"&extras=",extras,
                            "&per_page=",perpage,"&format=",format,"&page="
                            ,i,sep="")
         
         getPhotos_data <- xmlRoot(xmlTreeParse(getURL
                                                (getPhotos,ssl.verifypeer=FALSE, useragent = "flickr")
                                                ,useInternalNodes = TRUE ))
         
         id <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id")                 #extract photo id
         owner <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner")           #extract user id
         datetaken <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken")   #extract date picture was taken
         tags <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags")             #extract tags
         latitude <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude")     #extract latitude
         longitude <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude")   #extract longitude
         license <- xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license")       #extract license
         
         tmp_df <- data.frame(id, owner, datetaken, tags,
                              latitude, longitude, license,
                              stringsAsFactors = FALSE)
         
         tmp_df$page <- i
         pics_tmp <- rbind(pics_tmp, tmp_df)
         
       }
       
       pics <- rbind(pics, pics_tmp)
       
     }
     
   }
   
 }
 
 return(pics)
 
}
