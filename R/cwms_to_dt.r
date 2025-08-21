<<<<<<< HEAD
#' Function to get CWMS data from one path on the USACE CDA
#' @param path character; CWMS paths to read data from
#' @param start_date character; The begin date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param end_date character; The end date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param timezone character; Example is 'PST8PDT' or 'UTC'
#' @param timeseries logical; Use "F" or "FALSE" if retrieving location-level data; some types of location levels not setup yet
#' @param CDApath = character; default to 'https://wm.nww.ds.usace.army.mil:8243/nwdp-data/'; untested for other districts...
#' @param linux = logical; if running on LINUX OS, use "T"
#' @return a data.frame of the data
#' @author Norman Buccola based on Jetilton; https://github.com/jetilton/cwms_read/blob/master/cwms_read.r
#' @keywords CWMS Corps USACE data retrieval
#' @examples
#' @export
#' @importFrom jsonlite fromJSON
cwms_to_dt<-function(path, start_date, end_date, timezone = timezone,
                     timeseries,CDApath = CDApath,linux=F){
  url = get_url(path, start_date, end_date, timezone = timezone,timeseries=timeseries,CDApath = CDApath)
  print(paste(paste('`',path,sep=''),'`',sep=''))
  print(url)
  if(linux){
    # For Linux with json
    download.file(url, destfile = "Station_dataFile.json", method = "libcurl",
                  extra = "--no-check-certificate",headers =c(a = 'accept: application/json;version=2' ))

    #httpResponse <- GET(url,add_headers(a = 'accept:', b = 'application/json;version=2'), accept_json())
    # data <- rjson::fromJSON(file = content(httpResponse, "text"))
    # data <- jsonlite::fromJSON(content(httpResponse, "text"))
    # download.file(url, destfile = "Station_dataFile.json", method = "wget", extra = "--no-check-certificate")
    #data = rjson::fromJSON(file="Station_dataFile.json")
    data = jsonlite::fromJSON("Station_dataFile.json")
  }else{
    # For Windows machine
    data = jsonlite::fromJSON(url)
  }
  if(timeseries){
    if(length(data$`values`)>0){

      vals <- data.frame(data$`values`)
      nms <- data$`value-columns`$`name`
      colnames(vals) <- nms
      dt = data.frame(Date = as.POSIXct(vals$`date-time`/1000,tz =timezone),
                      value = as.numeric(vals$value),stringsAsFactors = F)
    }else{
      dt = data.frame(Date = as.POSIXct(c(start_date,end_date),tz =timezone),value = NA)
    }
  }else{ # Location Levels
    if(length(data$values)>0){
      dat <- data$values
      nms <- data$name
      dt = data.frame(Date = as.POSIXct(dat[,1]/1000,tz =timezone),
                      value = round(dat[,2],1),stringsAsFactors = F)
     }else{
      dt = data.frame(Date = as.POSIXct(c(start_date,end_date),tz =timezone),value = NA)
    }
  }
  colnames(dt) <- c('Date', path)
  return(dt)
}
=======
#' Function to get CWMS data from one path on the USACE CDA
#' Written by NBuccola on 2025-01-21 based on Jetilton; taken from https://github.com/jetilton/cwms_read/blob/master/cwms_read.r
#'
#' @param path character; CWMS paths to read data from
#' @param start_date character; The begin date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param end_date character; The end date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param timezone character; Example is 'PST8PDT' or 'UTC'
#' @param timeseries logical; Use "F" or "FALSE" if retrieving location-level data; some types of location levels not setup yet
#' @param CDApath = character; default to 'https://wm.nww.ds.usace.army.mil:8243/nwdp-data/'; untested for other districts...
#' @param linux = logical; if running on LINUX OS, use "T"
#' @return a data.frame of the data
#' @author Norman Buccola
#' @keywords CWMS Corps USACE data retrieval
#' @examples
#' \dontrun{
#' cwms_to_dt('DET.Elev-Forebay.Inst.0.0.Best', '2024-08-01', '2024-12-31',
#'   "PST8PDT", timeseries = TRUE)
#' }
#' @export
#' @importFrom jsonlite fromJSON
#' @importFrom utils download.file
cwms_to_dt<-function(path, start_date, end_date, timezone = timezone,
                     timeseries,CDApath = CDApath,linux=F){
  url = get_url(path, start_date, end_date, timezone = timezone,timeseries)
  print(paste(paste('`',path,sep=''),'`',sep=''))
  print(url)
  if(linux){
    # For Linux with json
    download.file(url, destfile = "Station_dataFile.json", method = "libcurl",
                  extra = "--no-check-certificate",headers =c(a = 'accept: application/json;version=2' ))

    #httpResponse <- GET(url,add_headers(a = 'accept:', b = 'application/json;version=2'), accept_json())
    # data <- rjson::fromJSON(file = content(httpResponse, "text"))
    # data <- jsonlite::fromJSON(content(httpResponse, "text"))
    # download.file(url, destfile = "Station_dataFile.json", method = "wget", extra = "--no-check-certificate")
    #data = rjson::fromJSON(file="Station_dataFile.json")
    data = jsonlite::fromJSON("Station_dataFile.json")
  }else{
    # For Windows machine
    data = jsonlite::fromJSON(url)
  }
  if(timeseries){
    if(length(data$`values`)>0){

      vals <- data.frame(data$`values`)
      nms <- data$`value-columns`$`name`
      colnames(vals) <- nms
      dt = data.frame(Date = as.POSIXct(vals$`date-time`/1000,tz =timezone),
                      value = as.numeric(vals$value),stringsAsFactors = F)
    }else{
      dt = data.frame(Date = as.POSIXct(c(start_date,end_date),tz =timezone),value = NA)
    }
  }else{ # Location Levels
    if(length(data$values)>0){
      dat <- data$values
      nms <- data$name
      dt = data.frame(Date = as.POSIXct(dat[,1]/1000,tz =timezone),
                      value = round(dat[,2],1),stringsAsFactors = F)
     }else{
      dt = data.frame(Date = as.POSIXct(c(start_date,end_date),tz =timezone),value = NA)
    }
  }
  colnames(dt) <- c('Date', path)
  return(dt)
}
>>>>>>> ce95ca656053e0c34faea833e191156f1792ec8c
