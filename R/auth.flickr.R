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
  
  myapp<-oauth_app("Flickr R-package",key= auth$key,secret= auth$key)                  #creates the app passing the key and secret
  
  ep<-oauth_endpoint(request="https://www.flickr.com/services/oauth/request_token"    #get authentication credentials from the API
                     ,authorize="https://www.flickr.com/services/oauth/authorize",
                     access="https://www.flickr.com/services/oauth/access_token")
  
  sig<-oauth1.0_token(ep,myapp,cache=F)                                             #creates variable with authentication credentials
  
  fl_sig <- sign_oauth1.0(myapp,sig)                                                #authenticate
}
