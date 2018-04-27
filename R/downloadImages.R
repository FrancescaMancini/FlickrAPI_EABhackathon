#' Download images found using \code{photosSearch}
#'
#' Using the data returned from the 
#' 
#' @param photoSearch_results A data.frame output from \code{photoSearch}.
#' @param licenses  Numeric vector, set the licenses you want to download images for. See \code{getLicenses}
#' @param saveDir Character, the path where images should be saved
#' @param max_quality Numeric 1-4 giving the maximum quality of image you want to download 1=small, 4=large
#' @param verbose logical, if TRUE the progeress through images is given
#' 
#' @return A vector of all the URLs that where targetted for download
#' @export
#' @name downloadImages
#' @examples
#' # run a workflow, using the logistic regression model
#' \dontrun{
#'
#' snowmen <- photosSearch(year_range = c(2015, 2016),
#'                         text = 'snowman',
#'                         woe_id = 12578048)
#' downloadImages(snowmen,
#'                licenses = c(6:10),
#'                saveDir = tempdir(),
#'                max_quality = 2)
#' 
#' }

downloadImages <-
  function(photoSearch_results,
           licenses = 7:10,          
           saveDir = '.',
           max_quality = 2,
           verbose = TRUE){
    
    if(!dir.exists(saveDir)){
      message(paste('saveDir', saveDir, 'does not exist, I will create it for you'))
      dir.create(saveDir, recursive = TRUE)
    }
    
    # get those that forefill the license requirements
    toGet <- photoSearch_results[photoSearch_results$license %in% licenses, ]
    
    if(nrow(toGet) == 0) stop('No images match these license conditions')
      
    # Download the images
    biggest_url <- function(x){
      
      bu <- tail(x = na.omit(x), 1)
      cat(bu)
      if(length(bu) == 0){
        return(NA)
      } else {
        return(bu)
      }
      
    }
    
    quality <- c('url_s', 'url_m', 'url_l', 'url_o')[1:max_quality]
    
    downloadURLs <- apply(toGet[, quality, drop = FALSE], MARGIN = 1, FUN = biggest_url)
    
    downloadURLs[is.na(downloadURLs)] <- toGet[is.na(downloadURLs), 'url_o'] 
    
    dump <- sapply(downloadURLs, all = downloadURLs, verbose = verbose,
                   FUN = function(x, all, verbose){
      if(verbose) cat(paste0('File ',grep(x,all),' of ',length(all),'\n'))
      download.file(url = x,
                    destfile = file.path(saveDir, basename(x)),
                    mode = 'wb')
      })
    
    return(downloadURLs)
    
  }