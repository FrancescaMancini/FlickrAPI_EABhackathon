#' Download images found using \code{photosSearch}
#'
#' Using the data returned from the 
#' 
#' @param photoSearch_results A data.frame output from \code{photoSearch}.
#' @param licenses  Numeric vector, set the licenses you want to download images for. See \code{getLicenses}
#' @param saveDir Character, the path where images should be saved
#' @param max_quality Numeric 1-4 giving the maximum quality of image you want to download 1=small, 4=large
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
           saveDir = '.',
           max_quality = 2){
    
    # get those that forefill the license requirements
    toGet <- photoSearch_results[photoSearch_results$license %in% licenses, ]
    
    if(nrow(toGet) == 0) stop('No images match these license conditions')
      
    # Download the images
    biggest_url <- function(x){
      
      tail(x = na.omit(x), 1)
      
    }
    
    quality <- c('url_s', 'url_m', 'url_l', 'url_o')[1:max_quality]
    
    downloadURLs <- apply(toGet[, quality, drop = FALSE], MARGIN = 1, FUN = biggest_url)
    
    dump <- sapply(downloadURLs, FUN = function(x){
      download.file(url = x,
                    destfile = file.path(saveDir, basename(x)),
                    mode = 'wb')
      })
    
    # stop('THIS FUNCTION DOESNT WORK YET')
    
    return(NULL)
    
  }