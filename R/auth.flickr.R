#' Authentification function for Flickr API 
#'
#' This function allows user identification  
#' Opens the browser and you can enter Flickr account details 
#' @export
#' @import httr
#' @name authFlickr


authFlickr <-
function()
{
  
  # creates the app passing the key and secret
  myapp <- oauth_app("Flickr R-package", key = auth$key, secret = auth$secret) 

  # get authentication credentials from the API
  ep <- oauth_endpoint(request = "https://www.flickr.com/services/oauth/request_token", 
                       authorize = "https://www.flickr.com/services/oauth/authorize?perms=read",
                       access = "https://www.flickr.com/services/oauth/access_token")
  
  # creates variable with authentication credentials
  sig <- oauth1.0_token(ep, myapp, cache = F) 
  
  # authenticate
  fl_sig <- sign_oauth1.0(myapp, sig) 
}
