# Get operations, temperature data for WillametteResSim
# Functions for getting data from CWMS for ResSim

#' Convert temperature units from C to F
#'
#' @description Convert degrees from C to F
#' @param x numeric; data in degrees C
#' @return numeric in degrees F
#' @export
#'
c2f <-function(x){x*(9/5)+32} # convert to F

#' Convert degrees from F to C
#'
#' @description Convert degrees from F to C
#' @param x numeric; data in degrees F
#' @return numeric in degrees C
#' @export
#'
f2c <-function(x){(x-32)*(5/9)} # convert to C


#' Get CWMS data from json config file path names
#'
#' @description Use get_cwms to extract data for a time frame for a series of pathnames defined via json config file. See inst
#' @param p list; The site information and pathnames taken from the json config file.
#' @param bdate character; The begin date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param edate character; The end date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @return writes raw Rdata files to RdataDir
#' @importFrom tidyr akima lubridate ggplot2 dplyr RColorBrewer scales stringr magrittr
#' @export
#'
GetCWMSdata <- function(p,bdate,edate,mergeNewPaths=F){
  library(tidyr)
  library(akima)
  library(lubridate)
  library(ggplot2)
  library(dplyr)
  library(RColorBrewer)
  library(scales)
  library(stringr)
  library(jsonlite)
  library(magrittr)
  Proj <- p[[grep('ProjAcrnm',names(p))]]
  OutletI <- p[[grep('OutletInfo',names(p))]]
  UpstrmPaths <- unlist(sapply(grep('UpstrmSite',names(p)),function(x) p[[x]]))
  DwnstrmPaths <- unlist(sapply(grep('DwnStrmSite',names(p)),function(x) p[[x]]))
  DwnstrmTDGPaths <- unlist(sapply(grep('DwnStrmTDGSite',names(p)),function(x) p[[x]]))
  TurbPaths <- unlist(sapply(grep('TurbSite',names(p)),function(x) p[[x]]))

  if(!is.Date(edate) |!is.POSIXt(edate) | mergeNewPaths){ # e.g., edate == 'Update'
    # Check for a previous file and prepare to merge later
    if(any(grepl('OldRdataDirProj',names(p))) && file.exists(p[['OldRdataDirProj']])){
      load(p[['OldRdataDirProj']])
      xwOld <- xw
      rm(xw)
    }
    if((!is.Date(edate) |!is.POSIXt(edate)) & !mergeNewPaths){
      # Only interested  in getting the most recent data that is not in OldRdataDirProj file
      bdate <- as.POSIXct(max(xwOld$DateTime))
    }
    edate <- as.POSIXct(today())
  }
  cwms_paths <- p[[grep('cwms_paths',names(p))]]

  allpaths <- list(NA)
  data_list <- list(NA)

  print(paste("Getting",Proj,"Data from",bdate,'to',edate))
  # Get temp string data
  if(any(grepl('temp_strings',names(cwms_paths)))){
    tstringPaths <- paste0(Proj,cwms_paths$temp_strings)
    data_list[['tstrings']] <- get_cwms(tstringPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['tstringPaths']] <- tstringPaths
  }
  # Get inflows
  if(any(grepl('QinPath',names(cwms_paths)))){
    inpths <- paste0(Proj,cwms_paths$QinPathTot)
    if(length(UpstrmPaths)>0){
       if(grepl('FOS|HCR|LOP|BLU',Proj)){
         # Assume the UpstreamPath[2] is the path with flow data
         inpths <- c(inpths,
                     paste0(UpstrmPaths,cwms_paths$TinPath),
                     paste0(UpstrmPaths[2],cwms_paths$QinPath2))
       }else{
         inpths <- c(inpths,paste0(UpstrmPaths,cwms_paths$TinPath))
         if(any(grepl('QinPath',names(cwms_paths),fixed = T,useBytes = T))){
           inpths <- c(inpths,paste0(UpstrmPaths,cwms_paths$QinPath))
         }
       }
       # Get Turbidity Paths
       if(any(grepl('TurbPath',names(cwms_paths),fixed = T,useBytes = T))){
         inpths <- c(inpths,paste0(TurbPaths,cwms_paths$TurbPath))
       }
    }
    data_list[['inflow']] <- get_cwms(inpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['inpths']] <- inpths
  }
  # Get Outflows
  if(any(grepl('OutPath',names(cwms_paths)))){
    outpths <- paste0(Proj,cwms_paths$OutPath)
    if(length(DwnstrmPaths)>0){ # Temperature data
      outpths <- c(outpths,paste0(DwnstrmPaths,cwms_paths$TdwnStrmPath))
    }
    if(length(DwnstrmTDGPaths)>0){ # TDG data
      #outpths <- paste0(DwnstrmTDGPaths,cwms_paths$TDGdwnStrmPath) # For adding a specific path
      outpths <- c(outpths,paste0(DwnstrmTDGPaths,cwms_paths$TDGdwnStrmPath))
    }
    data_list[['outflow']] <- get_cwms(outpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['outpths']] <- outpths
  }
  # Get Genflow
  if(any(grepl('GenPath',names(cwms_paths)))){
    genPaths <- paste0(Proj,cwms_paths$GenPath)
    data_list[['genflow']] <- get_cwms(genPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['genPaths']] <- genPaths
  }
  # Get Spillflow
  if(any(grepl('SpillPath',names(cwms_paths)))){
    spillPaths <- paste0(Proj,cwms_paths$SpillPath)
    data_list[['spillflow']] <- get_cwms(spillPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['spillPaths']] <- spillPaths
  }
  # Get forebay elevation
  if(any(grepl('FBWSELVPath',names(cwms_paths)))){
    fbwselvpths <- paste0(Proj,cwms_paths$FBWSELVPath)
    data_list[['fbwselv']] <- get_cwms(fbwselvpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['fbwselvpths']] <- fbwselvpths
  }
  # Get Rule Curves
  if(any(grepl('FBWSELVRule',names(cwms_paths)))){
    rulecrvpths <- unlist(sapply(grep('FBWSELVRule',names(cwms_paths)),function(x) cwms_paths[[x]]))
    rulecrvpths <- paste0(Proj,rulecrvpths)
    data_list[['rulecrv']] <- get_cwms(paths = rulecrvpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['rulecrvpths']] <- rulecrvpths
  }
  # Get GDACS gate data
  if(any(grepl('GDACS_Gates',names(cwms_paths)))){
    GDACSgatePaths <- paste0(cwms_paths$GDACS_Gates)
    data_list[['gateopen']] <- get_cwms(GDACSgatePaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['GDACSgatePaths']] <- GDACSgatePaths
  }
  # Get GDACS temperature data
  if(any(grepl('GDACS_Temps',names(cwms_paths)))){
    GDACStempPaths <- paste0(cwms_paths$GDACS_Temps)
    data_list[['gdacstemp']] <- get_cwms(GDACStempPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths[['GDACStempPaths']] <- GDACStempPaths
  }
  # Get downstream targets
  if(any(grepl('TTargPath',names(cwms_paths)))){
    ttargpths <- c(cwms_paths$MinTTargPath,cwms_paths$MaxTTargPath)
    data_list[['ttargs']] <- get_cwms(ttargpths,start_date=bdate,end_date=edate,CDApath=CDApath,timeseries=F)
    data_list[['ttargs']]$Date <- as.POSIXct(data_list[['ttargs']]$Date)
    allpaths[['ttargpths']] <- ttargpths
  }

  # Combine all data
  data_list <- Filter(Negate(is.logical), data_list)
  xw <- purrr::reduce(data_list,full_join,by = 'Date')

  xw <- xw |> dplyr::rename(DateTime = Date)

  #allpaths <- unlist(allpaths)
  # Save raw data here
  print(paste('Saving Files to',RdataDir))

  if(exists('xwOld')){
    # Merge new data with old
    xw <- xwOld |>
      full_join(xw)
    #allpaths <- unique(c(allpathsOld,allpaths))
  }
  save(xw,Proj,OutletI,allpaths,file = p[['RdataDirProj']])
  #write(unlist(allpaths),
  #      file = file.path(RdataDir,gsub('_OpsRaw','_PathNames',gsub('.Rdata','.txt',p[['dataFileName']]))))
}

#' Interpolate each column's data to hourly
#'
#' @description Interpolate data to hourly and remove columns with all NA values
#' @return Save data (wide format), project outlet info, pathnames and re-write data to environment (xw)
#'
InterpCols <- function(RdataDirProj){
  # Interpolate to hourly data
  print('Interpolating to Hourly Data')
  minDt <- min(xw$DateTime)
  maxDt <- max(xw$DateTime)
  hrlyDT <- seq.POSIXt(minDt,maxDt,by = 'hour')
  # Interpolate all numeric data linearly
  approxInterpOps <- function(y){approx(x=c(xw["DateTime"])[[1]],y=y,xout=hrlyDT,rule=1)$y}
  # Interpolate Targets with a constant
  approxInterpTargs <- function(y){approx(x=c(xw["DateTime"])[[1]],y=y,xout=hrlyDT,method = 'constant',f=0)$y}
  # Remove columns with all NA values
  AllNACols <- apply(apply(xw,1,is.na),1,all)
  if(any(AllNACols)){
    AllNACols <- which(AllNACols)
    AllNApaths <- colnames(xw)[AllNACols]
    xw <- xw[,-AllNACols]
  }
  if(any(grepl('Targ',colnames(xw)))){
    xw <- bind_cols(
      purrr::map_dfr(xw[,-c(grep('DateTime',colnames(xw)),grep('Targ',colnames(xw)))],approxInterpOps),
      purrr::map_dfr(xw[,grep('Targ',colnames(xw))],approxInterpTargs)
    )
  }else{
    xw <- purrr::map_dfr(xw[,-grep('DateTime',colnames(xw))],approxInterpOps)
  }
  xw <- xw |>
    mutate(DateTime = hrlyDT) |>
    relocate(DateTime)
  # Save hourly data with no NAs separately here
  save(xw,Proj,OutletI,file = gsub('_OpsRaw','_OpsHrlyClean',RdataDirProj))
  list2env(list(xw = xw),envir = .GlobalEnv)
}



#' Calculate a summary of each variable
#'
#' @param x Assuming a long format dataframe with name = site, value = value
#' @description Calculate a summary of each variable
#' @return a summary table of the min, max, quantiles, and percent complete (PrcComp) for each variable.
#' @importFrom tidyr dplyr
#' @export
#'
pathSummary <- function(x){
  #Calculate a summary of each variable. Assuming long format with name = site, value = value
  x |>
    group_by(name) |>
    reframe(MinDate = min(as.Date(Date[!is.na(Median)])),
            MaxDate = max(as.Date(Date[!is.na(Median)])),
            N = length(Median[!is.na(Median)]),
            Mean = mean(Median[!is.na(Median)]),
            Min = min(Median[!is.na(Median)]),
            Q05 = quantile(Median[!is.na(Median)],0.05),
            Q25 = quantile(Median[!is.na(Median)],0.25),
            Q50 = quantile(Median[!is.na(Median)],0.5),
            Q75 = quantile(Median[!is.na(Median)],0.75),
            Q95 = quantile(Median[!is.na(Median)],0.95),
            Max = max(Median[!is.na(Median)]),
            PrcComp = round(100*length(which(!is.na(Median)))/length(Median)))
}

#' Calculate Daily, monthly, and Annual values
#'
#' @description Calculate a summary of each variable based on the xw (wide format dataframe)
#' @return a list of the original long-format dailys (xld), long-format monthly (xlm), long-format annuals (xla)
#' @importFrom tidyr dplyr ggplot
#' @export
#'
calcDailyMonthlyAnn <- function(){
  print("Calculating Daily values...")
  xl <- xw |>
    tidyr::pivot_longer(-DateTime) |>
    mutate(name = gsub('\\.Ave|\\.1Hour|\\.MIXED-REV|MIXED-COMPUTED-REV|\\.CBT-REV|\\.CENWP-CALC|\\.GDACS-RAW|\\.Code|\\.Inst|\\.~1Day|\\.Best|\\.0.0','',name),
           Date = as.Date(DateTime,tz ='PST8PDT'),
           # Remove outliers
           value = if_else(grepl('Temp-',name) & !grepl('Targ',name) & (value >100 |value <32),NA,value),
           value = if_else(grepl('Flow-',name) & (value >10000 |value <0),NA,value),
           # Convert from F to C!!! (Temp targets are already in C!)
           value = if_else(grepl('Temp-',name) & !grepl('Targ',name),f2c(value),value),
    )
  # Make daily values
  xld <- xl |>
    dplyr::select(-DateTime) |> #,-matches('_S1-D')   # If you want to leave out Temp string data for daily data
    group_by(Date,name) |>
    reframe(Min = min(value,na.rm=T),
            Median = median(value,na.rm=T),
            Mean = mean(value,na.rm=T),
            Max = max(value,na.rm=T)) |>
    mutate(name = as.factor(name))

  print('Calculating Monthly Values')
  # Calculate the monthly values
  xlm <- xl |>
    mutate(Month = format(Date,'%m'),Year = format(Date,'%Y')) |>
    group_by(Year,Month,name) |>
    reframe(Min = min(value[!is.na(value)]),
            Mean = mean(value[!is.na(value)]),
            Median = median(value[!is.na(value)]),
            Q05 = quantile(value[!is.na(value)],0.05),
            Q25 = quantile(value[!is.na(value)],0.25),
            Q50 = quantile(value[!is.na(value)],0.5),
            Q75 = quantile(value[!is.na(value)],0.75),
            Q95 = quantile(value[!is.na(value)],0.95),
            Max = max(value[!is.na(value)]),
            PrcComp = round(100*length(which(!is.na(value)))/length(value))) |>
    mutate(Month = as.numeric(Month),Year = as.numeric(Year))

  print('Calculating Annual Values')
  # Make Annual averages
  xla <- xl |>
    mutate(Year = format(DateTime,'%Y')) |>
    dplyr::select(-DateTime) |>
    group_by(Year,name) |>
    reframe(N = length(value[!is.na(value)]),
            Min = min(value[!is.na(value)]),
            Mean = mean(value[!is.na(value)]),
            Median = median(value[!is.na(value)]),
            Q05 = quantile(value[!is.na(value)],0.05),
            Q25 = quantile(value[!is.na(value)],0.25),
            Q50 = quantile(value[!is.na(value)],0.5),
            Q75 = quantile(value[!is.na(value)],0.75),
            Q95 = quantile(value[!is.na(value)],0.95),
            Max = max(value[!is.na(value)]),
            PrcComp = round(100*length(which(!is.na(value)))/length(value))) |>
  mutate(Year = as.factor(Year),name = as.factor(name))


  xldSum <- pathSummary(xld)

  print('Pivot data back to wide format')
  # xwd <- xld |>
  #   tidyr::pivot_wider()
  xw <- xl |>
    tidyr::pivot_wider() |>
    dplyr::select(-Date)
  list2env(list(xw = xw,xld = xld,xldSum = xldSum,xlm=xlm,xla = xla),envir = .GlobalEnv)
}


#' Plot the percentage of complete data by month for each CWMS data path
#'
#' @param xMon Monthly data
#' @description Plot the percentage of complete data for each CWMS data path
#' @return a ggplot object
#' @importFrom ggplot
#' @export
#'
PlotPercentComplete<- function(xMon){
  figPercComp <-
    ggplot(xMon,
           aes(y=as.factor(Month),x=as.factor(Year),fill = PrcComp,colour=PrcComp,group = name)) +
    geom_tile() +
    facet_grid(name~.,scales = 'free', switch="y") +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.x=element_text(size = 10,angle=45,hjust=1),
          axis.text.y=element_text(angle=45,hjust=1),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          strip.placement = "outside",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill=NA),
          legend.title = element_blank(),
          legend.position = 'top'
    ) +
    ylab('Path and Month') +
    xlab('Year') +
    ggtitle(paste0(Proj,' Percent Complete Data'))
  figPercComp
  ggplot2::ggsave(plot = figPercComp,filename = file.path(RdataDir,gsub('.Rdata','_PercCompData.png',dataFileName)),
                  device='png',width=6,height=14)

}

