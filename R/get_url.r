#' Function to create full URL for cwms_get.r
#' Written by NBuccola on 2025-01-21 based on Jetilton; taken from https://github.com/jetilton/cwms_read/blob/master/cwms_read.r
#'
#' @param path character; CWMS paths to read data from
#' @param start_date character; The begin date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param end_date character; The end date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param timezone character; Example is 'PST8PDT' or 'UTC'
#' @param timeseries logical; Use "F" or "FALSE" if retrieving location-level data; some types of location levels not setup yet
#' @param CDApath = character; default to 'https://wm.nww.ds.usace.army.mil:8243/nwdp-data/'; untested for other districts...
#' @return a data.frame of the data
#' @author Norman Buccola
#' @keywords CWMS Corps USACE data retrieval
#' @examples
#' \dontrun{
#' get_url('DET.Elev-Forebay.Inst.0.0.Best', '2024-08-01', '2024-12-31')
#' }
#' @importFrom utils URLencode
#' @export
get_url<-function(path, start_date, end_date, timezone = 'PST8PDT',
                  timeseries=T,
                  CDApath = 'https://wm.nww.ds.usace.army.mil:8243/nwdp-data/'){
  start_date = format(strptime(start_date,"%Y-%m-%d"),"%Y-%m-%dT%H:%M:%S")
  end_date = format(strptime(end_date, "%Y-%m-%d"),"%Y-%m-%dT%H:%M:%S")
  office = '&office=NWDP'
  begin <-  paste0('&begin=',start_date)
  end <- paste0('&end=',end_date)
  pageSize <- '&page-size=50000'

  if(timeseries){
    dscrptr <- 'timeseries?'
    path <- paste0('name=',path)
    #unit = "&unit=EN"
    url = paste0(CDApath,dscrptr,path,office,begin,end,#unit,
                 paste0('&timezone=',timezone),
                 pageSize) # Defaults to 500
  }else{
    dscrptr <- 'levels/'
    dscrptr2 <- '/timeseries?'
    pageSize <- ''
    unit = "&unit=EN"
    url = paste0(CDApath,dscrptr,path,dscrptr2,office,begin,end,#
                 paste0('&timezone=',timezone),
                 pageSize,unit) # Defaults to 500

  }
  url = URLencode(url)
  return(url)
}
