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
GetCWMSdata <- function(p,bdate,edate){
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
  cwms_paths <- p[[grep('cwms_paths',names(p))]]
  allpaths <- NA

  if(!is.Date(edate) |!is.POSIXt(edate)){ # e.g., edate == 'Update'
    # Check for a previous file and prepare to merge later
    if(any(grepl('OldRdataDirProj',names(p))) && file.exists(p[['OldRdataDirProj']])){
      load(p[['OldRdataDirProj']])
      xwOld <- xw
      rm(xw)
    }
    bdate <- as.POSIXct(max(xwOld$DateTime))
    edate <- as.POSIXct(today())
  }
  print(paste("Getting",Proj,"Data from",bdate,'to',edate))
  # Get temp string data
  if(any(grepl('temp_strings',names(cwms_paths)))){
    tstringPaths <- paste0(Proj,cwms_paths$temp_strings)
    tstrings <- get_cwms(tstringPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,tstringPaths)
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
    inflow <- get_cwms(inpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,inpths)
  }
  # Get Outflows
  if(any(grepl('OutPath',names(cwms_paths)))){
    outpths <- paste0(Proj,cwms_paths$OutPath)
    if(length(DwnstrmPaths)>0){ # Temperature data
      outpths <- c(outpths,paste0(DwnstrmPaths,cwms_paths$TdwnStrmPath))
    }
    if(length(DwnstrmTDGPaths)>0){ # TDG data
      outpths <- c(outpths,paste0(DwnstrmTDGPaths,cwms_paths$TDGdwnStrmPath))
    }
    outflow <- get_cwms(outpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,outpths)
  }
  # Get Genflow
  if(any(grepl('GenPath',names(cwms_paths)))){
    genPaths <- paste0(Proj,cwms_paths$GenPath)
    genflow <- get_cwms(genPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,genPaths)
  }
  # Get Spillflow
  if(any(grepl('SpillPath',names(cwms_paths)))){
    spillPaths <- paste0(Proj,cwms_paths$SpillPath)
    spillflow <- get_cwms(spillPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,spillPaths)
  }
  # Get forebay elevation
  if(any(grepl('FBWSELVPath',names(cwms_paths)))){
    fbwselvpths <- paste0(Proj,cwms_paths$FBWSELVPath)
    fbwselv <- get_cwms(fbwselvpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,fbwselvpths)
  }
  # Get GDACS gate data
  if(any(grepl('GDACS_Gates',names(cwms_paths)))){
    GDACSgatePaths <- paste0(cwms_paths$GDACS_Gates)
    gateopen <- get_cwms(GDACSgatePaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,GDACSgatePaths)
  }
  # Get GDACS temperature data
  if(any(grepl('GDACS_Temps',names(cwms_paths)))){
    GDACStempPaths <- paste0(cwms_paths$GDACS_Temps)
    gdacstemp <- get_cwms(GDACStempPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,GDACStempPaths)
  }
  # Get downstream targets
  if(any(grepl('TTargPath',names(cwms_paths)))){
    ttargpths <- c(cwms_paths$MinTTargPath,cwms_paths$MaxTTargPath)
    ttargs <- get_cwms(ttargpths,start_date=bdate,end_date=edate,CDApath=CDApath,timeseries=F)
    allpaths <- c(allpaths,ttargpths)
  }else{
    ttargpths <- NULL
  }
  # Get downstream TDG, Temperature

  xw <- outflow |>
      full_join(fbwselv)
  if(exists('spillflow')){
    xw <- xw |>
      full_join(spillflow)
  }
  if(exists('tstrings')){
    xw <- xw |>
      full_join(tstrings)
  }
  if(exists('genflow')){
    xw <- xw |>
      full_join(genflow)
  }
  if(exists('inflow')){
    xw <- xw |>
      full_join(inflow)
  }
  if(exists('gateopen')){
    xw <- xw |>
      full_join(gateopen)
  }
  if(exists('gdacstemp')){
    xw <- xw |>
      full_join(gdacstemp)
  }
  if(exists('ttargs')){
    xw <- xw |>
      full_join(ttargs)
  }
  xw <- xw |> dplyr::rename(DateTime = Date)
  # Save raw data here
  print(paste('Saving Files to',RdataDir))

  if(exists('xwOld')){
    # Merge new data with old
    xw <- xwOld |>
      full_join(xw)
  }
  save(xw,Proj,OutletI,cwms_paths,inpths,UpstrmPaths,DwnstrmPaths,fbwselvpths,ttargpths,
       file = p[['RdataDirProj']])
  write(allpaths,
        file = file.path(RdataDir,gsub('_OpsRaw','_PathNames',gsub('.Rdata','.txt',p[['dataFileName']]))))
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
  approxInterpOps <- function(y){approx(x=c(xw["DateTime"])[[1]],y=y,xout=hrlyDT,rule=1)$y}
  # Remove columns with all NA values
  AllNACols <- apply(apply(xw,1,is.na),1,all)
  if(any(AllNACols)){
    AllNACols <- which(AllNACols)
    AllNApaths <- colnames(xw)[AllNACols]
    xw <- xw[,-AllNACols]
  }
  xw <- purrr::map_dfr(xw[,-1],approxInterpOps) |>
    mutate(DateTime = hrlyDT) |>
    relocate(DateTime)
  # Save hourly data with no NAs separately here
  save(xw,Proj,OutletI,cwms_paths,inpths,UpstrmPaths,DwnstrmPaths,fbwselvpths,ttargpths,
       file = gsub('_OpsRaw','_OpsHrlyClean',RdataDirProj))
  list2env(list(xw = xw),envir = .GlobalEnv)

  #
  # list2env(list(xw = xw,Proj =Proj,OutletI=OutletI,UpstrmPaths=UpstrmPaths,
  #               ttargpths=ttargpths,allpaths=allpaths,
  #               DwnstrmPaths=DwnstrmPaths,DwnstrmTDGPaths=DwnstrmTDGPaths,cwms_paths=cwms_paths),
  #          envir = .GlobalEnv)
  #
}


#' Calculate weighted average of inflow temperature
#'
#' @description Calculate the estimated inflow temperature to the reservoir
#' @return modifies the xw (wide format) hourly data for a dam
#' @importFrom tidyr dplyr
#' @export
#'
CalcTin <- function(){
  if(is.null(UpstrmPaths) |
     !any(grepl('Temp-Water',colnames(xw)))){
    print("No upstream temperature measurements")
    return()
  }
  if(grepl('FOS|HCR',Proj)){
    # Calculate GPRO flow based on FOS inflow and SSCO;
    # For HCR, calculate HCOO based on the difference between total inflow and MFOO
    # assume the 2nd flow input has real flow data to subtract from the Flow-In data variable
    EstQinCols <- match(c(inpths[grep('Flow-In',inpths)],
                          inpths[grep(paste0(UpstrmPaths[2],'.Flow'),inpths)]),colnames(xw))
    xw$Qin1FlowEst = c(xw[,EstQinCols[1]] - xw[,EstQinCols[2]])[[1]]
    colnames(xw)[match('Qin1FlowEst',colnames(xw))] <- paste0(UpstrmPaths,cwms_paths$QinPath2)[1]
  }
  # Check for missing flow or Temp data
  Tvars <- grepl('Temp-Water',colnames(xw)) & unlist(sapply(UpstrmPaths,grepl,colnames(xw)))
  Qvars <- grepl('Flow.Inst',colnames(xw)) & unlist(sapply(UpstrmPaths,grepl,colnames(xw)))
  if(length(Tvars)>1 |length(Qvars)>1){
    UpstrmPaths <- UpstrmPaths[apply(Qvars,2,function(x)length(which(x))>0) |
                               apply(Tvars,2,function(x)length(which(x))>0)]
  }
  if(length(UpstrmPaths)>1){
    for(i in 1:length(UpstrmPaths)){
      cit <- grepl('Temp-Water',colnames(xw)) & grepl(UpstrmPaths[i],colnames(xw))
      ciq <- grepl('Flow',colnames(xw)) & grepl(UpstrmPaths[i],colnames(xw))
      nmi <- paste0(UpstrmPaths[i],'.QTcalc')
      qti <- xw[,cit] * xw[,ciq]
      xw <- xw |> mutate(!!nmi := qti)
      rm(qti,cit,ciq,nmi)
    }
    # Sum separate inflows and divide QT by this sum
    xw$Qsum <- apply(xw[,unlist(sapply(paste0(UpstrmPaths,'.Flow'),grep,colnames(xw)))],1,sum)
    xw$QTsum <- apply(xw[,unlist(sapply(paste0(UpstrmPaths,'.QTcalc'),grep,colnames(xw)))],1,sum)
    xw$TinAvg <- xw$QTsum/xw$Qsum
  }else{
    # If only one inflow has measured temp, copy it to the Weighted Avg Calc
    cit <- grepl('Temp-Water',colnames(xw)) & grepl(UpstrmPaths,colnames(xw))
    colnames(xw)[cit] <- 'TinAvg'
  }
  colnames(xw) <- gsub('TinAvg',paste0(Proj,'.Temp-In.WeightedAvgCalc'),colnames(xw))
  xw <- xw[,!grepl('Qsum|QTsum|QTcalc',colnames(xw))]
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
    reframe(MinDate = min(as.Date(Date[!is.na(value)])),
            MaxDate = max(as.Date(Date[!is.na(value)])),
            N = length(value[!is.na(value)]),
            Mean = mean(value[!is.na(value)]),
            Min = min(value[!is.na(value)]),
            Q05 = quantile(value[!is.na(value)],0.05),
            Q25 = quantile(value[!is.na(value)],0.25),
            Q50 = quantile(value[!is.na(value)],0.5),
            Q75 = quantile(value[!is.na(value)],0.75),
            Q95 = quantile(value[!is.na(value)],0.95),
            Max = max(value[!is.na(value)]),
            PrcComp = round(100*length(which(!is.na(value)))/length(value)))
}

#' Calculate Daily and Annual values
#'
#' @description Calculate a summary of each variable based on the xw (wide format dataframe)
#' @return a list of the original wide-format dailys (xwd), daily summary (xwdSum), annuals (xAnn)
#' @importFrom tidyr dplyr ggplot
#' @export
#'
calcDailys <- function(){
  print("Calculating Daily values...")
  xl <- xw |>
    tidyr::pivot_longer(-DateTime) |>
    mutate(name = gsub('\\.Ave|\\.1Hour|MIXED-COMPUTED-REV|\\.CBT-REV|\\.CENWP-CALC|\\.~1Day|\\.Best|\\.Inst|\\.0.0','',name),
           Date = as.Date(DateTime,tz ='PST8PDT'),
           # Remove outliers
           value = if_else(grepl('Temp-',name) & (value >100 |value <32),NA,value),
           value = if_else(grepl('Flow-',name) & (value >10000 |value <0),NA,value))
  #unique(xl$name)
  # Calculate the percent complete data for each month
  xMon <- xl |>
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

  # Make daily averages
  xld <- xl |>
    dplyr::select(-DateTime) |> #,-matches('_S1-D')   # If you want to leave out Temp string data for daily data
    group_by(Date,name) |>
    reframe(Min = min(value,na.rm=T),
            Median = median(value,na.rm=T),
            Mean = mean(value,na.rm=T),
            Max = max(value,na.rm=T)) |>
    # Left off here need to prepare this data for pivot_wider()
    mutate(name = as.factor(name))

  # Make Annual averages
  xAnn <- xl |>
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


  xwdSum <- pathSummary(xld)

  print('Pivot data back to wide format')
  xwd <- xld |>
    tidyr::pivot_wider()
  # xw <- xl |>
  #   tidyr::pivot_wider() |>
  #   dplyr::select(-Date)
  list2env(list(xwd = xwd,xwdSum = xwdSum,xAnn = xAnn),envir = .GlobalEnv)
}

#' Tabulate the exceedance values by month for each CWMS data path
#'
#' @param DataType Type of data (e.g., Temp-Water,TDG,Flow)
#' @param ExcVals Exceedance values to tabulate
#' @description Tabulate the number of days above or below a threshold
#' @return Modifies the xMon tibble
#' @importFrom dplyr
#' @export
#'
calcExceed <- function(DataType,ExcVals){
  # LEFT OFF HERE
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

#' Plot the daily averages for each parameter
#'
#' @param xld daily data
#' @description Plot daily averages for each parameter
#' @return writes a series of plots based on xl, xld to RdataDir; Writes an annual summary of data (xAnn) to the current environment
#' @importFrom ggplot2
#' @export
#'
MakeMultiYrPlots <- function(){
  # Temperature: Plot daily averages for each parameter
  figTsByTParams <-
    ggplot(xld |> dplyr::filter(grepl('Temp-',name)),
           aes(x=Date,y = value,colour=name,group = name)) +
    geom_line(alpha = 0.6) +
    ylab('Temperature, Degrees F') +
    xlab('Year') +
    scale_x_date(breaks = seq.Date(from = min(xld$Date),to = max(xld$Date),by = 'year'),
                 date_labels = '%Y'
    ) +
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
    ggtitle(paste0(Proj,' Temperature Time-Series by Parameter'))
  figTsByTParams
  ggplot2::ggsave(plot = figTsByTParams,
                  filename = file.path(RdataDir,gsub('.Rdata','_TsByTParams.png',dataFileName)),
                  device='png',width=9,height=4)

  # Flow: Plot daily averages for each parameter
  figTsByQParams <-
    ggplot(xld |> dplyr::filter(grepl('Flow-',name)),
           aes(x=Date,y = value,colour=name,group = name)) +
    geom_line(alpha = 0.6) +
    ylab('Flow, cfs') +
    xlab('Year') +
    scale_x_date(breaks = seq.Date(from = min(xld$Date),to = max(xld$Date),by = 'year'),
                 date_labels = '%Y'
    ) +
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
    ggtitle(paste0(Proj,' Flow Time-Series by Parameter'))
  figTsByQParams
  ggplot2::ggsave(plot = figTsByQParams,
                  filename = file.path(RdataDir,gsub('.Rdata','_TsByQParams.png',dataFileName)),
                  device='png',width=9,height=4)

  # Plot daily averages for WSELV
  figTsByWselvParam <-
    ggplot(xld |> dplyr::filter(grepl('Elev-',name)),
           aes(x=Date,y = value,colour=name,group = name)) +
    geom_line(alpha = 0.6) +
    ylab('Lake Elevation, ft NGVD') +
    xlab('Year') +
    scale_x_date(breaks = seq.Date(from = min(xld$Date),to = max(xld$Date),by = 'year'),
                 date_labels = '%Y'
    ) +
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
    ggtitle(paste0(Proj,' Lake Elevation Time-Series'))
  figTsByWselvParam
  ggplot2::ggsave(plot = figTsByWselvParam,
                  filename = file.path(RdataDir,gsub('.Rdata','_TsByWselvParam.png',dataFileName)),
                  device='png',width=9,height=4)

  print('Calculating parameter summaries...')

  # Flow: Show the median annual
  figAnnQComp <-
    ggplot(xAnn |> dplyr::filter(grepl('Flow',name)),
           aes(y=Mean,x=Year,colour=name,group = name)) + #label = round(Mean),fill = Mean,
    geom_line(alpha=0.8) +
    ylab('Mean Annual Flow, cfs') +
    xlab('Year') +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.x=element_text(size = 10,angle=45,hjust=1),
          #axis.text.y=element_text(angle=45,hjust=1),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          strip.placement = "outside",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill=NA),
          legend.title = element_blank(),
          legend.position = 'top'
    ) +
    ggtitle(paste0(Proj,' Annual Mean By Flow Parameter'))
  figAnnQComp
  ggplot2::ggsave(plot = figAnnQComp,filename = file.path(RdataDir,gsub('.Rdata','_AnnQComp.png',dataFileName)),
                  device='png',width=9,height=4)

  figAnnTComp <-
    ggplot(xAnn |> dplyr::filter(grepl('Temp',name)),
           aes(y=Q50,x=Year,colour=name,group = name)) + #label = round(Q50),fill = Q50,
    geom_line(alpha=0.8) +
    ylab('Median Annual Water Temperature, deg F') +
    xlab('Year') +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.x=element_text(size = 10,angle=45,hjust=1),
          #axis.text.y=element_text(angle=45,hjust=1),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          strip.placement = "outside",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill=NA),
          legend.title = element_blank(),
          legend.position = 'top'
    ) +
    ggtitle(paste0(Proj,' Annual Mean By Temp Parameter'))
  figAnnTComp
  ggplot2::ggsave(plot = figAnnTComp,filename = file.path(RdataDir,gsub('.Rdata','_AnnTComp.png',dataFileName)),
                  device='png',width=9,height=4)

  figAnnWSEComp <-
    ggplot(xAnn |> dplyr::filter(grepl('Elev',name)),
           aes(y=Q95,x=Year,colour=name,group = name)) + #label = round(Q95),fill = Q95,
    geom_line(alpha=0.8) +
    ylab('Annual 95th% Lake Elevation, ft NGVD') +
    xlab('Year') +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.x=element_text(size = 10,angle=45,hjust=1),
          #axis.text.y=element_text(angle=45,hjust=1),
          axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          strip.placement = "outside",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill=NA),
          legend.title = element_blank(),
          legend.position = 'top'
    ) +
    ggtitle(paste0(Proj,' Annual 95th% Water Surface Elevation, ft'))
  figAnnWSEComp
  ggplot2::ggsave(plot = figAnnWSEComp,filename = file.path(RdataDir,gsub('.Rdata','_AnnWSEComp.png',dataFileName)),
                  device='png',width=8,height=3)


}


ReadOpsTempFlsByProj <- function(dataFileName){
  load(dataFileName)
  # Left off here!!!

  map_dfr()


}

# Function to iteratively merge tibbles from Rdata files
merge_tibbles_from_rdata <- function(newDataFileName) {
  file_paths <- list.files(RdataDir,pattern = '.Rdata')
  # Initialize an empty tibble to store the merged data
  Xw <- Xwd <- XwdSum <- xAnn <- tibble()

  # Loop through each file path
  for (file_path in file_paths) {
    #file_path = file_paths[2]
    print(paste('merging',file_path))
    # Load the Rdata file
    load(file.path(RdataDir,file_path))

    # Adjust the variable name if necessary
    current_xw <- xw
    current_xwd <- xwd
    current_xwdSum <- xwdSum
    current_xAnn <- xAnn

    # Merge with the existing merged tibble
    if (nrow(Xw) == 0) {
      # If it's the first tibble, assign it directly
      Xw <- current_xw
      Xwd <- current_xwd
      XwdSum <- current_xwdSum
      XAnn <- current_xAnn

    } else {
      # Otherwise, merge using the specified column
      Xw <- Xw |> full_join(current_xw,by = 'DateTime')
      Xwd <- Xwd |> full_join(current_xwd,by= 'Date')
      XwdSum <- XwdSum |> full_join(current_xwdSum)
      XAnn <- XAnn |> full_join(current_xAnn)
    }
  }
  save(Xw,Xwd,XwdSum,XAnn,file = file.path(RdataDir,newDataFileName))
}

