#' Query Flickr API and build a dataframe of metadata
#'
#' This function uses the Flickr API to get metadata about the images
#' 
#' @param year_range The year to search between (inclusive). For example c(2005,2007) will search for images from 2005, 2006 and 2007
#' @param text  Set keywords so that query returns pictures with the text (e.g. "bird")
#' @param bbox A vector of 4 values defining the Bounding Box of the area that will be searched. The 4 values represent the bottom-left corner of the box and the top-right corner, minimum_longitude, minimum_latitude, maximum_longitude, maximum_latitude. 
#' @param woe_id A 32-bit identifier that uniquely represents spatial entities (e.g. 12578048 is Scotland). These ids can be found using the \code{findPlaces} function Can not be provided if bbox allready provided
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
#' # Authenticate the first time you run a search
#' #auth.flickr()
#'
#' birds <- photosSearch(year_range = c(2005,2006),
#'                       text = "mute+swan",
#'                       woe_id =  12578048,
#'                       has_geo = TRUE)
#'
#' head(birds)
#' 
#' }

photosSearch <-
function(year_range,
         text,          
         bbox = NULL,
         woe_id =NULL,        
         has_geo = TRUE){

  if( !(is.null(bbox) | is.null(woe_id))==TRUE) {
    stop('can not provide bbox and woe_id')
  }

 text <- gsub(' ', '+', trimws(text))  
 api_key <- auth$key
 perpage <- "250"
 format <- "rest"
 extras <- "date_taken,geo,tags,license,url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o"
 baseURL <- paste("https://api.flickr.com/services/rest/?method=flickr.photos.search&api_key=",api_key,sep="")   #set base URL
 pics<-NULL
 year_range <- seq(min(year_range), max(year_range), 1)
 
 #API only returns 400 results per query so it is neccessary to loop through months to obtain all the results
 
 pb <- txtProgressBar(min = 0, max = length(year_range)*12, style = 3)

 for (y in year_range){                     
   
   firstDate <- as.Date(paste0(y, "-01-01"))
   
   nDays <- length(seq(as.Date(firstDate), as.Date(paste0(y, "-12-31")), by="+1 day"))
   
   month <- round(seq(from = 0, to = nDays, length.out = 13))
   
   for(m in 1:(length(month) - 1)){
     
     mindate <- firstDate + month[m]
     maxdate <- firstDate + month[m + 1]
     
     
     if(!is.null(bbox)){
     getPhotos <- paste(baseURL,
                        "&text=", text,
                        "&min_taken_date=", as.character(mindate),
                        "&max_taken_date=", as.character(maxdate),
                        "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                        "&extras=", extras,
                        "&per_page=", perpage,
                        "&format=", format,
                        sep = "")
     } else if(!is.null(woe_id)){
       getPhotos <- paste(baseURL,
                          "&text=", text,
                          "&min_taken_date=", as.character(mindate),
                          "&max_taken_date=", as.character(maxdate),
                          "&woe_id=", woe_id,
                          "&extras=", extras,
                          "&per_page=", perpage,
                          "&format=", format,
                          sep = "")     
     } else {
       getPhotos <- paste(baseURL,
                          "&text=", text,
                          "&min_taken_date=", as.character(mindate),
                          "&max_taken_date=", as.character(maxdate),
                          ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                          "&extras=", extras,
                          "&per_page=", perpage,
                          "&format=", format,
                          sep = "")
      }

     r <- GET(getPhotos)
     
     count_stat <- 0
     
     while(r$status_code != 200 & count_stat < 3){
       Sys.sleep(0.5)
       r <- GET(getPhotos)
       count_stat <-  count_stat + 1
     }

     if(r$status_code != 200){
       warning('Status code:', r$status, ' for year ', y, ' month ', m, ' - message: ', content(r, 'text'))
     }
    
     error <- tryCatch({
       getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text')))
       error <- 'sucess'
       }, error = function(err){
         warning('Year ', y, ' month ', m, ' skipped beacuse: ', err)
         error <- 'error'
       })    
     
     if(error != 'error'){
         
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
           
           if(!is.null(bbox)){ 
           
            getPhotos <- paste(baseURL
                              ,"&text=",text,"&min_taken_date=",mindate,
                              "&max_taken_date=",maxdate,
                              "&bbox=", paste0(bbox[1],",",bbox[2],",",bbox[3],",",bbox[4]),
                              "&extras=",extras,
                              "&per_page=",perpage,"&format=",format,"&page="
                              ,i,sep="")
           
           } else if(!is.null(woe_id)){
             
             getPhotos <- paste(baseURL
                                ,"&text=",text,"&min_taken_date=",mindate,
                                "&max_taken_date=",maxdate,
                                "&woe_id=",woe_id,
                                "&extras=",extras,
                                "&per_page=",perpage,"&format=",format,"&page="
                                ,i,sep="")
             
           } else {
             
             getPhotos <- paste(baseURL
                                ,"&text=",text,"&min_taken_date=",mindate,
                                "&max_taken_date=",maxdate,
                                ifelse(has_geo, paste0("&has_geo=", has_geo), ''),
                                "&extras=",extras,
                                "&per_page=",perpage,"&format=",format,"&page="
                                ,i,sep="")        
             
           }
           
           r <- GET(getPhotos)
           
           count_stat <- 0
           
           while(r$status_code != 200 & count_stat < 3){
             Sys.sleep(0.5)
             r <- GET(getPhotos)
             count_stat <-  count_stat + 1
           }
           
           if(r$status_code != 200){
             warning('Status code:', r$status, ' for year ', y, ' month ', m, ' page ', i, ' - message: ', content(r, 'text'))
           }
           
           error <- tryCatch({
             getPhotos_data <- xmlRoot(xmlTreeParse(content(r, 'text'), useInternalNodes = TRUE))
             error <- 'sucess'
           }, error = function(err){
             warning('Year ', y, ' month ', m, ' page ', i,' skipped beacuse: ', err)
             error <- 'error'
           })
           
           if(error != 'error'){
             getPhotos_data <<- getPhotos_data
             id <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "id"))                 #extract photo id
             owner <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "owner"))           #extract user id
             datetaken <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "datetaken"))   #extract date picture was taken
             tags <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "tags"))             #extract tags
             latitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "latitude"))     #extract latitude
             longitude <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "longitude"))   #extract longitude
             license <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "license"))       #extract license
             url_s <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_s"))           #extract url_s
             url_m <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_m"))           #extract url_m
             url_l <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_l"))           #extract url_l
             url_o <- listNulltoNA(xpathSApply(getPhotos_data, "//photo", xmlGetAttr, "url_o"))           #extract url_o

             if(!all(is.na(c(id,owner,datetaken,tags,latitude,longitude,license,url_s,url_m,url_l,url_o)))){
               
               tmp_df <- data.frame(id, owner, datetaken, tags,
                                    as.numeric(latitude),
                                    as.numeric(longitude), license,
                                    url_s = unlist(url_s), url_m = unlist(url_m),
                                    url_l = unlist(url_l), url_o = unlist(url_o),
                                    stringsAsFactors = FALSE)
               
               tmp_df$page <- i
               pics_tmp <- rbind(pics_tmp, tmp_df)
               rm(list = 'tmp_df')
               
             }
           }
         }
         
         pics <- rbind(pics, pics_tmp)
         
       }
     }
     
     setTxtProgressBar(pb, ((y - min(year_range)) * 12) + m)
     
     }
   
 }
 
 close(pb)
 return(pics)
 
}


