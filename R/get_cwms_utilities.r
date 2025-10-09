# Functions for getting data from CWMS for ResSim

#' @description Convert degrees from C to F
#' @param x numeric; data in degrees C
#' @return numeric in degrees F
#' @export
c2f <-function(x){x*(9/5)+32} # convert to F

#' @description Convert degrees from F to C
#' @param x numeric; data in degrees F
#' @return numeric in degrees C
#' @export
f2c <-function(x){(x-32)*(5/9)} # convert to C

#' @description Use get_cwms to extract data for a time frame for a series of pathnames defined via json config file. See inst
#' @param p list; The site information and pathnames taken from the json config file.
#' @param bdate character; The begin date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @param edate character; The end date of extraction, in 'YYYY-MM-DD HH:MM' format
#' @return writes Rdata files to RdataDir and sends data to the current environment
#' @importFrom tidyr akima lubridate ggplot2 dplyr RColorBrewer scales stringr magrittr
#' @export
GetCWMSdata <- function(p,bdate,edate){
  Proj <- p[[grep('ProjAcrnm',names(p))]]
  OutletI <- p[[grep('OutletInfo',names(p))]]
  UpstrmPaths <- unlist(sapply(grep('UpstrmSite',names(p)),function(x) p[[x]]))
  DwnstrmPaths <- unlist(sapply(grep('DwnStrmSite',names(p)),function(x) p[[x]]))
  DwnstrmTDGPaths <- unlist(sapply(grep('DwnStrmTDGSite',names(p)),function(x) p[[x]]))
  cwms_paths <- p[[grep('cwms_paths',names(p))]]
  allpaths <- NA
  # Get temp string data via CDA (CWMS Data Acquisition)
  if(any(grepl('temp_strings',names(cwms_paths)))){
    tstringPaths <- na.omit(paste0(Proj,cwms_paths$temp_strings))
    tstrings <- get_cwms(tstringPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,tstringPaths)
    # if(grepl('DET',Proj)){
    #   # Read in thermistor data from HOBO strings (not in CWMS)
    #   tstringHobo <- readr::read_csv(file.path(LocalWd,'ResSim/data/tempStrings/Model_Calibration_Data/DET_ThermStrings_2016_forR.csv'))
    #   colnames(tstringHobo) <- gsub('D5','D0,5',gsub('date','Date',colnames(tstringHobo)))
    #   tstringHobo$Date <- as.POSIXct(tstringHobo$Date,format='%m/%d/%Y %H:%M',tz = 'PST8PDT')
    #   tstringHobo <- tstringHobo |> drop_na() |> select(-JDAY) |>
    #     dplyr::filter((Date < as.POSIXct('2016-09-20') & Date > as.POSIXct('2016-06-05'))) |>
    #     # Convert to Deg F
    #     mutate_if(is.numeric,c2f)
    #   tstrings <- tstrings |>
    #     full_join(tstringHobo |>
    #                 dplyr::filter(Date < as.POSIXct('2016-09-20') & Date > as.POSIXct('2016-06-05'))
    #     )
    # }
    # Remove Dates of bad thermistor data at FOS
    # print(xw |>
    #           select('DateTime',contains("FOS_S")) |>
    #           pivot_longer(cols = -DateTime) |>
    #           # Remove Dates of bad thermistor data at FOS
    #           filter(!grepl('2014-07-25 05:00:00|2016-03-07 06:00:00|2014-01-24|2014-01-25|2010-11-29|2010-11-28',DateTime)) #|>
    #           )

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
        if(any(grepl('QinPath',cwms_paths,fixed = T,useBytes = T))){
          inpths <- c(inpths,paste0(UpstrmPaths,cwms_paths$QinPath))
        }
      }
    }
    inpths <- inpths[!is.null(inpths) | !is.na(inpths)]
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
    outpths <- outpths[!is.null(outpths) | !is.na(outpths)]
    outflow <- get_cwms(outpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,outpths)
  }
  # Get Genflow
  if(any(grepl('GenPath',names(cwms_paths)))){
    genPaths <- na.omit(paste0(Proj,cwms_paths$GenPath))
    genPaths <- genPaths[!is.na(genPaths) | !is.null(genPaths)]
    genflow <- get_cwms(genPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,genPaths)
  }
  # Get Spillflow
  if(any(grepl('SpillPath',names(cwms_paths)))){
    spillPaths <- na.omit(paste0(Proj,cwms_paths$SpillPath))
    spillPaths <- spillPaths[!is.na(spillPaths) | !is.null(spillPaths)]
    spillflow <- get_cwms(spillPaths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,spillPaths)
  }
  # Get forebay elevation
  if(any(grepl('FBWSELVPath',names(cwms_paths)))){
    fbwselvpths <- paste0(Proj,c(cwms_paths$FBWSELVPath,cwms_paths$WCPath))
    fbwselvpths <- fbwselvpths[!is.na(fbwselvpths) | !is.null(fbwselvpths)]
    fbwselv <- get_cwms(fbwselvpths,start_date=bdate,end_date=edate,CDApath=CDApath)
    allpaths <- c(allpaths,fbwselvpths)
  }
  # Get downstream targets
  if(any(grepl('TargPath|WCPath',names(cwms_paths)))){
    targpths <- na.omit(c(cwms_paths$MinTTargPath,cwms_paths$MaxTTargPath,
                          cwms_paths$QTargPath))
    targpths <- targpths[!is.na(targpths) | !is.null(targpths)]
    targs <- get_cwms(targpths,start_date=bdate,end_date=edate,CDApath=CDApath,timeseries=F)
    allpaths <- c(allpaths,targpths)
  }
  allpaths <- unique(allpaths[!is.na(allpaths) | !is.null(allpaths)])
  # Merge data into a wide-format tibble
  xw <- outflow |>
    full_join(spillflow) |>
    full_join(fbwselv)
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
  if(exists('targs')){
    xw <- xw |>
      full_join(targs)
  }
  xw <- xw |> dplyr::rename(DateTime = Date)

  # Interpolate to hourly data
  print('Interpolating to Hourly Data')
  hrlyDT <- seq.POSIXt(bdate,edate,by = 'hour')
  approxInterpOps <- function(y){approx(x=c(xw["DateTime"])[[1]],y=y,xout=hrlyDT,rule=1)$y}
  # Remove columns with all NA values
  xw <- xw[,!apply(apply(xw,1,is.na),1,all)]
  xw <- purrr::map_dfr(xw[,-1],approxInterpOps) |>
    mutate(DateTime = hrlyDT) |>
    relocate(DateTime)

  print(paste('Saving Files to',RdataDir))
  #dataFileName <- paste0(Proj,'_OpsTempTDG',yrs,'.Rdata')
  save(xw,Proj,cwms_paths,inpths,UpstrmPaths,DwnstrmPaths,fbwselvpths,targpths,
       file = RdataDirProj)
  write(allpaths,
        file = file.path(RdataDir,gsub('_OpsTempTDG','_PathNames',
                                       gsub('.Rdata','.txt',dataFileName))))
  list2env(list(xw = xw,Proj =Proj,OutletI=OutletI,UpstrmPaths=UpstrmPaths,
                targpths=targpths,allpaths=allpaths,
                DwnstrmPaths=DwnstrmPaths,DwnstrmTDGPaths=DwnstrmTDGPaths,cwms_paths=cwms_paths),
           envir = .GlobalEnv)
}

# Calculate weighted average of inflow temperature
#' @description Calculate the estimated inflow temperature to the reservoir
#' @return modifies the xw (wide format) hourly data for a dam
#' @importFrom tidyr akima lubridate ggplot2 dplyr RColorBrewer scales stringr magrittr
#' @export
CalcTin <- function(){
  if(grepl('FOS|HCR',Proj)){
    # Calculate GPRO flow based on FOS inflow and SSCO;
    # For HCR, calculate HCOO based on the difference between total inflow and MFOO
    # assume the 2nd flow input has real flow data to subtract from the Flow-In data variable
    EstQinCols <- match(c(inpths[grep('Flow-In',inpths)],
                          inpths[grep(paste0(UpstrmPaths[2],'.Flow'),inpths)]),colnames(xw))
    xw$Qin1FlowEst = c(xw[,EstQinCols[1]] - xw[,EstQinCols[2]])[[1]]
    colnames(xw)[match('Qin1FlowEst',colnames(xw))] <- paste0(UpstrmPaths,cwms_paths$QinPath2)[1]
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

#  Assuming long format with name = site, value = value
#' @description Calculate a summary of data availability for each variable.
#' @param x tibble; The site data in long format
#' @return a summary table of the min, max, quantiles, and percent complete (PrcComp) for each variable.
#' @importFrom tidyr dplyr
#' @export
pathSummary <- function(x){
  x |>
    group_by(name) |>
    reframe(MinDate = min(as.Date(Date[!is.na(value)])),
            MaxDate = max(as.Date(Date[!is.na(value)])),
            Mean = mean(value[!is.na(value)]),
            Q05 = quantile(value[!is.na(value)],0.05),
            Q25 = quantile(value[!is.na(value)],0.25),
            Q50 = quantile(value[!is.na(value)],0.5),
            Q75 = quantile(value[!is.na(value)],0.75),
            Q95 = quantile(value[!is.na(value)],0.95),
            PrcComp = round(100*length(which(!is.na(value)))/length(value)))
}

#' @description Calculate daily values from the xw (wide-format) tibble
#' @return xwd (daily data in wide format), xMon (monthly percent complete)
#' @importFrom tidyr akima lubridate ggplot2 dplyr RColorBrewer scales stringr magrittr
#' @export
calcDailys <- function(){
  print("Calculating Daily values...")
  xl <- xw |>
    tidyr::pivot_longer(-DateTime) |>
    mutate(name = gsub('\\.Ave|\\.1Hour|MIXED-COMPUTED-REV|\\.CBT-REV|\\.CENWP-CALC|\\.~1Day|\\.Best|\\.Inst|\\.0.0','',name),
           Date = as.Date(DateTime,tz ='PST8PDT'),
           # Remove outliers
           value = if_else(grepl('Temp-',name) & (value >100 |value <32),NA,value),
           value = if_else(grepl('Flow-',name) & (value >10000 |value <0),NA,value))
  unique(xl$name)
  # Calculate the percent complete data for each month
  xMon <- xl |>
    mutate(Month = format(Date,'%m'),Year = format(Date,'%Y')) |>
    group_by(Year,Month,name) |>
    reframe(Mean = mean(value[!is.na(value)]),
            Q05 = quantile(value[!is.na(value)],0.05),
            Q25 = quantile(value[!is.na(value)],0.25),
            Q50 = quantile(value[!is.na(value)],0.5),
            Q75 = quantile(value[!is.na(value)],0.75),
            Q95 = quantile(value[!is.na(value)],0.95),
            PrcComp = round(100*length(which(!is.na(value)))/length(value))) |>
    mutate(Month = as.numeric(Month),Year = as.numeric(Year))

  figPercComp <-
    ggplot(xMon,
           aes(y=Month,x=Year,fill = PrcComp,colour=PrcComp,group = name)) +
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
    ggtitle(paste0(Proj,' Percent Complete Data'))
  figPercComp
  ggplot2::ggsave(plot = figPercComp,filename = file.path(RdataDir,gsub('.Rdata','_PercCompData.png',dataFileName)),
                  device='png',width=6,height=14)

  # Make daily averages
  xld <- xl |>
    dplyr::select(-DateTime) |> #,-matches('_S1-D')   # If you want to leave out Temp string data for daily data
    group_by(Date,name) |>
    reframe(value = mean(value,na.rm=T))

  xwdSum <- pathSummary(xld)

  print('Pivot data back to wide format')
  xwd <- xld |>
    tidyr::pivot_wider()
  xw <- xl |>
    tidyr::pivot_wider() |>
    dplyr::select(-Date)
  list2env(list(xw = xw,xwd = xwd,xwdSum = xwdSum),envir = .GlobalEnv)
}

#' @description Plot daily averages for each parameter
#' @return writes a series of plots based on xl, xld to RdataDir; Writes an annual summary of data (xAnn) to the current environment
#' @importFrom ggplot2
#' @export
MakeMultiYrPlots <- function(){
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
  # Calculate the percent complete data for each month
  xAnn <- xl |>
    mutate(Year = format(Date,'%Y')) |>
    #dplyr::filter(Year!='2025') |>
    group_by(Year,name) |>
    reframe(Mean = mean(value[!is.na(value)]),
            Q05 = quantile(value[!is.na(value)],0.05),
            Q25 = quantile(value[!is.na(value)],0.25),
            Q50 = quantile(value[!is.na(value)],0.5),
            Q75 = quantile(value[!is.na(value)],0.75),
            Q95 = quantile(value[!is.na(value)],0.95),
            PrcComp = round(100*length(which(!is.na(value)))/length(value)))
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
  list2env(list(xAnn = xAnn),envir = .GlobalEnv)

}

#' @description Iteratively merge tibbles from Rdata files
#' @return Saves multi-year Xw (hourly time-series wide format),Xwd (daily timeseries daily format),XwdSum (daily summary wide-format),XAnn (annual summary)
#' @importFrom dplyr
#' @export
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

# ReadOpsTempFlsByProj <- function(dataFileName){
#   load(dataFileName)
#   # Left off here!!!
#
#   map_dfr()
#
#
# }

