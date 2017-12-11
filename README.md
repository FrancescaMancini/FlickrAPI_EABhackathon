# Flickr

The flickr package allows you to access image metadata from flickr through R

Check out the website for more information [https://augustt.github.io/FlickrAPI_EABhackathon/](https://augustt.github.io/FlickrAPI_EABhackathon/)

```r
# install the package from github
library(devtools)
install_github('FrancescaMancini/FlickrAPI_EABhackathon')

# get some glorious metadata
snowmen <- photosSearch(year_range = c(2005, 2007),
                        text = 'snowman',
                        woe_id = 12578048)
                        
head(snowmen)
str(snowmen)
```

R code to download data associated with photos uploaded on Flickr. 
This code uses flickr.photos.search API tool to download the metadata associated with photos uploaded on Flickr. It includes loops through months and years to avoid API limitations in the number of results returned; loops through pages of results. It uses geographic restrictions and keywords to select which photos to download. It returns a dataframe
