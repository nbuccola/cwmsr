#' Wrapper function to get CWMS data from the USACE CDA
#' Written by NBuccola on 2025-01-21 based on Jetilton; taken from https://github.com/jetilton/cwms_read/blob/master/cwms_read.r
#'
#' @param paths character; CWMS paths to read data from
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
#' beginDate <- strptime('2024-08-01',"%Y-%m-%d",tz = 'PST8PDT')
#' endDate <- strptime('2024-12-31',"%Y-%m-%d",tz = 'PST8PDT')
#' CWMSpaths <- 'DET.Elev-Forebay.Inst.0.0.Best'
#' get_cwms(CWMSpaths, beginDate, endDate)
#' }
#' @export
#' @importFrom foreach foreach %do%
get_cwms<-function(paths, start_date, end_date, timezone = 'PST8PDT',
                   timeseries=T,
                   CDApath = 'https://wm.nww.ds.usace.army.mil:8243/nwdp-data/'){

  # shortcut to avoid R CMD Check warning
  y <- NULL
  date_diff <- end_date - start_date
  parse_length <- 90 #days
  if(date_diff > parse_length){
    webquerySeq <- c(seq(start_date,end_date,by = paste(parse_length,'days')),end_date)
    # Parse into 3-month web queries
    dt = foreach(y = (1:(length(webquerySeq)-1)),.combine = 'rbind') %do% {
      start_datey <- webquerySeq[y]
      end_datey <- as.POSIXct(gsub("01:00:00","00:00:00",webquerySeq[y+1]))
      print(paste("Getting",start_datey,'to',end_datey))
      x1 <- lapply(paths,cwms_to_dt, start_date = start_datey,
                   end_date = end_datey, timezone = timezone,
                   timeseries=timeseries,CDApath = CDApath)
      return(Reduce(function(x,y){merge(x,y,all=T)},x1))
    }
  }else{
    dt = lapply(paths,cwms_to_dt, start_date = start_date,
                end_date = end_date, timezone = timezone,
                timeseries=timeseries,CDApath = CDApath)
    return(Reduce(function(x,y){merge(x,y,all=T)},dt))
  }
}
