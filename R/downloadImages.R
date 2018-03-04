#' Download images found using \code{photosSearch}
#'
#' Using the data returned from the 
#' 
#' @param photoSearch_results A data.frame output from \code{photoSearch}.
#' @param licenses  Numeric vector, set the licenses you want to download images for. See \code{getLicenses}
#' @param saveDir Character, the path where images should be saved
#' 
#' @return NULL
#' @export
#' @name downloadImages
#' @examples
#' # run a workflow, using the logistic regression model
#' \dontrun{
#'
#' snowmen <- photosSearch(year_range = c(2005, 2016),
#'                         text = 'snowman',
#'                         woe_id = 12578048)
#' downloadImages(images, licenses = c(4, 7:10))
#' 
#' }

downloadImages <-
  function(photoSearch_results,
           licenses = 7:10,          
           saveDir = '.'){
    
    # get those that forefill the license requirements
    toGet <- photoSearch_results[photoSearch_results$license %in% licenses, ]
    
    if(nrow(toGet) == 0) stop('No images match these license conditions')
      
    # Download the images
    biggest_url <- function(x){
      
      tail(x = na.omit(x), 1)
      
    }
    
    downloadURLs <- apply(toGet[,c('url_s', 'url_m', 'url_l', 'url_o')], MARGIN = 1, FUN = biggest_url)
    
    dump <- sapply(downloadURLs, FUN = function(x){
      download.file(url = x,
                    destfile = file.path(saveDir, basename(x)),
                    mode = 'wb')
      })
    
    # stop('THIS FUNCTION DOESNT WORK YET')
    
    return(NULL)
    
  }