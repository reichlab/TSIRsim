#' Title
#'
#' @param these.dates 
#'
#' @return
#' @export
#'
#' @examples
get.yr <- function(these.dates) {
  yr <- format(these.dates, format="%Y")
  
#  #check data validity
#  stopifnot(is.numeric(yr), (yr >= 2015 || is.na(yr)), 
#            (yr <= 2018 || is.na(yr)))
  return(yr)
}

#' Title
#'
#' @param these.dates 
#' @param as.str 
#'
#' @return
#' @export
#'
#' @examples
get.wk <- function(these.dates, as.str=F) {
  this.wk <- sprintf("%02d", 
      ceiling(as.numeric(format(these.dates, '%j'))/7))
  this.wk <- ifelse(this.wk %in% c('00', '53'), '52', this.wk)
  if (as.str) return(this.wk)
  else return(as.numeric(this.wk))
}

#' Title
#'
#' @param these.dates 
#'
#' @return
#' @export
#'
#' @examples
get.yr.wk <- function(these.dates) {
  paste(get.yr(these.dates), get.wk(these.dates, as.str=T), sep="-")
}

#' Title
#'
#' @param wk 
#'
#' @return
#' @export
#'
#' @examples
wk2char <- function(wk) {
    sprintf("%02d", wk)
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
create.time.ref <- function(){
  # create persistent data table for all time references
  
  times_dt <- data.table(
    date = seq(as.Date('2015-01-21'), 
               Sys.Date() + 280, by=7)) %>%
    mutate(
      yr_wk = get.yr.wk(date),
      yr = get.yr(date),
      wk = get.wk(date),
      time = 1:length(yr_wk)) %>%
    arrange(date)
  return(times_dt)
}
