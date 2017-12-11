#####################################################
# R code to query Flickr API and build a dataframe
# created by Francesca Mancini
# adapted from Mauricion Alarcon https://rpubs.com/rmalarc/74406
# last modified 04/11/2016
#####################################################

rm(list = ls())

library(RCurl)
library(XML)
library(httr)

load('auth.rdata')

api_key <- auth$key      #API key and secret must be obtained from https://www.flickr.com/services/api/misc.api_keys.html

secret <- auth$secret

myapp <- oauth_app("Flickr R-package",
                   key = api_key,
                   secret = secret)                  #creates the app passing the key and secret




ep <- oauth_endpoint(request="https://www.flickr.com/services/oauth/request_token",    #get authentication credentials from the API
                     authorize="https://www.flickr.com/services/oauth/authorize",
                     access="https://www.flickr.com/services/oauth/access_token")


sig<-oauth1.0_token(ep,myapp,cache=F)                                             #creates variable with authentication credentials

fl_sig <- sign_oauth1.0(myapp,sig)                                                #authenticate
