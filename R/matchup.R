# matchup!
#
# This is an example function named 'matchup'
# which found the same value in two rows!'.
#
#' Title
#'
#' @param x the row that you want to compare (short list)
#' @param y the row that you will find the coincidences (long list)
#' @param dp dp=TRUE if you don't want duplicates and dp=FALSE if you want duplactes
#'
#' @return a vector with the coincidences, if the match have no coincidences the vector will be numeric(0)
#' @export
#'
#' @examples \dontrun y<-c(1,2,3,4,5,6,7) x<-c(11,22,33,5,5,5,5,4)
#' \dontrun md1<-matchup(x,y,dp=TRUE) md2<-matchup(x,y,dp=FALSE)
matchup<-function(x,y,dp){match(t(x),t(y)) ->z

  z<-z[!is.na(z)]



  y[z]->m

  dp==c(TRUE,FALSE)
  if(dp==TRUE){h <- m[!duplicated(m)]
  h}
  else{m}
}
