install.packages("devtools")
library("devtools")
devtools::install_github("klutometis/roxygen")
library(roxygen2)

setwd("parent_directory")
create("cats")



cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

cat_function <- function(love=TRUE){
  if(love==TRUE){
    print("I love cats!")
  }
  else {
    print("I am not a cool person.")
  }
}


setwd("./cats")
document()


setwd("..")
install("cats")


install_github('cats','github_username')