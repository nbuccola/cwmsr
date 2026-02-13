#' Calculate weighted average of inflow temperature
#'
#' @description Calculate the estimated inflow temperature to the reservoir
#' @return modifies the xw (wide format) hourly data for a dam
#' @importFrom tidyr dplyr
#' @export
#'
CalcTin <- function(cwms_paths){
  UpstrmPaths <- unlist(sapply(grep('UpstrmSite',names(cwms_paths)),function(x) cwms_paths))
  DwnstrmPaths <- unlist(sapply(grep('DwnStrmSite',names(cwms_paths)),function(x) cwms_paths))
  DwnstrmTDGPaths <- unlist(sapply(grep('DwnStrmTDGSite',names(cwms_paths)),function(x) cwms_paths))

  if(is.null(UpstrmPaths) | !any(grepl('Temp-Water',colnames(xw)))){
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

#' Calculate the 7dADM for all temperature paths
#'
#' @param xld Long-format tibble with Temp-Water data
#' @description Tabulate the number of days above or below a threshold
#' @return Modifies the xld tibble
#' @importFrom dplyr slider
#' @export
#'
calc7dadm <- function(xld){
  xld7d <- xld |>
    # Omit temp string and target data paths
    dplyr::filter(str_detect(name,'Temp-Water'),!str_detect(name,'_S1'),!str_detect(name,'Target'))
    # Calculate the 7-day average of the daily max (adm7d)
  xld <- xld |>
      full_join(
        xld7d |>
          arrange(name, Date) |>
          group_by(name) |>
          reframe(
            adm7d = slider::slide_index_mean(x = Max, i = Date, before = days(3),after = days(3)),
            Date = Date)
      )

  list2env(list(xld = xld),envir = .GlobalEnv)
}



#' Reformat and replicate Fish criteria so it spans a larger timeframe
#'
#' @param FishCrit A table of fish criteria with begin/end dates and thresholds
#' @description Reformat and replicate Fish criteria so it spans a larger timeframe
#' @return Long table with Date, name of target, threshold, value
#' @importFrom dplyr
#' @export
#'
ReformatFishCrit <- function(FishCrit){
  # Replicate and update dates for FishCriteria
  # Keep the original year's data as the base
  FishCrit00 <- FishCrit |>
    rename(name = criteria) |>
    select(-color) |>
    mutate(acuteL = as.character(acute),
           chronicL = as.character(chronic)) |>
    pivot_longer(cols=c(acuteL,chronicL),names_to = 'name2') |>
    mutate(name = paste0(name,'_',gsub('L','',name2)),
           start = as.Date(start),finish = as.Date(finish)) |>
    select(-c(name2,acute,chronic)) |>
    mutate(day_diff = as.integer(finish - start) + 1) %>%
    uncount(day_diff) %>%
    group_by(name, start, finish) %>%
    mutate(Date = start + row_number() - 1) %>%
    ungroup() |>
    select(-c(start,finish)) |>
    mutate(value = as.numeric(value))

  yrsu<-as.numeric(unique(c(format(bdate,'%Y'),format(edate,'%Y'))))

  replicated_FishCrit <- bind_rows(
    FishCrit00,
    # Replicate for target years
    purrr::map_dfr(yrsu[1]:yrsu[2], function(year_to_add) {
      FishCrit00 %>%
        mutate(
          # Add the difference in years to the date column
          Date = Date + years(year_to_add - year(first(FishCrit00$Date))),
        )
    })
  ) %>%
    # Arrange by date to ensure correct order
    arrange(Date) |>
    mutate(Year = format(Date,'%Y'),
           Month = format(Date,'%m'))

  return(replicated_FishCrit)
}


#' Tabulate the exceedance values by month for each CWMS data path
#'
#' @param DataType Type of data (e.g., Temp-Water,TDG,Flow)
#' @param TVals Exceedance values to tabulate
#' @description Tabulate the number of days above or below a threshold
#' @return Modifies the xMon tibble
#' @importFrom dplyr lubridate
#' @export
#'
calcThreshMon <- function(DataType,TVals){
  rm(xlmExc)
  dTypNms <- grepl(paste0(DataType,collapse = '|'),unique(xld$name)) &
    !grepl('_S1|Targ',unique(xld$name))
  if(!any(dTypNms)){
    print(paste0('No Data Paths with ',DataType,'. Skipping any threshold analysis.'))
    return(NULL)
  }
  dTypNms <- paste(unique(xld$name)[dTypNms], collapse = "|")
  idcols <- c('name','Year','Month','TValsNum')

  # Calculate monthly exceedences
  # Setup a subset tibbles to run calcs on.
  xlmExcSub <- xld |>
    dplyr::filter(str_detect(name,dTypNms)) |>
    mutate(Month = format(Date,'%m'),Year = format(Date,'%Y')) |>
    group_by(Year,Month,name)

  # Temperature: Calculate time above threshold
  # Next step is to use the wqCrit variable instead of a straight number
  if(any(grepl('Temp',DataType))){
    if(!any(grepl(Proj,names(tsites)))){
      print(paste0('No Data Paths with ',DataType,'. Skipping any threshold analysis.'))
      return(NULL)
    }
    print(DataType)

    TNms <- paste0('Exc',gsub('Path','',TVals))
    TValsNum <- as.numeric(TVals)[!is.na(as.numeric(TVals))]

    xlmExc <- xlmExcSub |>
          expand_grid(TValsNum) |>
          arrange(name, Date) |>
          mutate(Month = as.factor(Month),Year = as.factor(Year)) |>
          group_by(name,Month,Year,TValsNum) |>
            reframe(
            Dys7dADMExc = length(which(adm7d >TValsNum)),#Use the 7dADM data
            PrcDys7dADMExc = length(which(adm7d >TValsNum)) /  length(adm7d),#Use the 7dADM data
            DysMaxExc = length(which(Max >TValsNum)),
            PrcDysMaxExc = length(which(Max >TValsNum)) /  length(Max),
            DysMeanExc = length(which(Mean >TValsNum)),
            PrcDysMeanExc = length(which(Mean >TValsNum)) /  length(Mean)
          ) |>
          mutate(TValsNum = as.factor(TValsNum))
  }

  # Calculate time above max, in range, below min targs
  if(any(grepl('TargPath',TVals)) & any(grepl('Targ',unique(xlmExcSub$name)))){
    TargNms <- unique(xld$name)[grepl('Targ',unique(xld$name))]
    # Applies to sites that have targets associated with them
    if(any(grepl('Flow',DataType))){
      #Use the Daily Min data
      # Left off here!!!
    }
    if(any(grepl('Temp',DataType))){
      if(!any(grepl(Proj,names(tsites)))){
        print(paste0('No Data Paths with ',DataType,'. Skipping any threshold analysis.'))
        return(NULL)
      }
      TNms <- paste0('Exc',gsub('Path','',TVals))
      TValsNum <- as.numeric(TVals)[!is.na(as.numeric(TVals))]
      # Rename Target names
      names(TargNms) <- ifelse(grepl('Max',TargNms),'MaxTarg','MinTarg')
      for(i in 1:length(TargNms)){
        xlmExcSub$name <- gsub(TargNms[i],names(TargNms)[i],xlmExcSub$name)
      }
      TempCompSite <- strsplit(as.character(TargNms),'\\.')[[1]][[1]]
      TempCompSite <- unique(xlmExcSub$name)[grepl(TempCompSite,unique(xlmExcSub$name)) &
                                               !grepl('Targ',unique(xlmExcSub$name)) ]

      # NEed to replicate 2025 temp target data, since years prior to that don't seem to be in CWMS
      ttarg25 <- xlmExcSub |>
        dplyr::filter(grepl('Targ',name)) |>
        ungroup() |>
        filter(Year==2025) |>
        dplyr::select(-c(Year,Month,Min,Median,Max,adm7d)) |>
        rename(value = Mean)

      # Replicate and update the dates using dplyr and purrr
      replicated_ttarg25 <- bind_rows(
        # Keep the original year's data as the base
        ttarg25,
        # Replicate for target years
        purrr::map_dfr(as.numeric(unique(xlmExcSub$Year)), function(year_to_add) {
          ttarg25 %>%
            mutate(
              # Add the difference in years to the date column
              Date = Date + years(year_to_add - year(first(ttarg25$Date))),
            )
        })
      ) %>%
        # Arrange by date to ensure correct order
      arrange(Date) |>
      mutate(Year = format(Date,'%Y'),Month = format(Date,'%m'),
             threshold = gsub('MinTarg','lower',gsub('MaxTarg','upper',name)),
             name = 'OpsTarget')


      xlmExcTarg <-
        FishCrit_ts |>
        full_join(replicated_ttarg25) |>
        full_join(
          xlmExcSub |>
            dplyr::filter(grepl(!!TempCompSite,name) & !grepl('Targ',name)) |>
            dplyr::select(-c(Min,Median,Mean,Max)) |>
            rename(value = adm7d) |> # NOTE: Days above 7dADM
            pivot_wider()
          ) |>
        mutate(Month = as.factor(Month),
               Year = as.factor(Year),
               threshold = as.factor(threshold),
               name = as.factor(name)) |>
        distinct() |>
        pivot_wider(names_from = threshold,values_fn = mean) |>
        arrange(Date) |>
        group_by(Month,Year,name) |>
        reframe(TargMin = lower,TargMax = upper,
                Dys7dADMAbv = length(which(!!sym(TempCompSite) > upper)),
                Dys7dADMBlw = length(which(!!sym(TempCompSite) < lower)),
                Dys7dADMBtwn = length(na.omit(!!sym(TempCompSite))) - (Dys7dADMAbv + Dys7dADMBlw)
        ) |>
        rename(Target = name) |>
        mutate(name = !!TempCompSite) |>
        filter(!is.na(Target)) |>
        distinct()
        # print(n=300)
        # summary()
        # Merge with xlmExc
        xlmExc <- xlmExc |>
          full_join(xlmExcTarg) |>
          mutate(Target =
                   factor(Target,levels = c("OpsTarget",
                                            "Migration_acute","Migration_chronic",
                                            "Holding_acute","Holding_chronic",
                                            "Spawning_acute","Spawning_chronic",
                                            "Incubation_acute","Incubation_chronic"),
                               ordered=T))
        idcols <- c(idcols,c('Target','TargMin','TargMax'))
     } # End Exceedence of targets work
  }

  # TDG: Calculate time daily max is above threshold
  if(any(grepl('TDG',DataType))){
    if(!any(grepl(Proj,names(TDGsites)))){
      print(paste0('No Data Paths with ',DataType,'. Skipping any threshold analysis.'))
      return(NULL)
    }
    print(DataType)
    TNms <- paste0('Exc',gsub('Path','',TVals))
    TValsNum <- as.numeric(TVals)[!is.na(as.numeric(TVals))]
    #Use the daily max data
    xlmExc <- xlmExcSub |>
      expand_grid(TValsNum) |>
      filter(grepl(DataType,name)) |>
      arrange(name, Date) |>
      mutate(Month = as.factor(Month),Year = as.factor(Year)) |>
      group_by(name,Month,Year,TValsNum) |>
      reframe(DysMaxExc = if_else(any(!is.finite(Max)),NA,length(which(Max >TValsNum))),
              PrcDysMaxExc = DysMaxExc /  length(Max))|>
      mutate(TValsNum = as.factor(TValsNum))
  }

  # WSELV: Calculate time above threshold
  if(any(grepl('Elev-Forebay',DataType)) &
     any(grepl('WtempControl',TVals))  &
     any(names(qgtConfig[['CenterElvs']]) %in% Proj) &
     any(names(qgtConfig[['ExcElev']]) %in% Proj)){
    print(DataType)
    OutVals <- qgtConfig[['CenterElvs']][[grep(Proj,names(qgtConfig[['CenterElvs']]))]]
    TNms <- paste0('Above ',gsub('1','',qgtConfig[['ExcElev']][grep(Proj,names(qgtConfig[['ExcElev']]))]))
    TValsNum <- OutVals[,qgtConfig[['ExcElev']][,grep(Proj,names(qgtConfig[['ExcElev']]))]]
    xlmExc <- xlmExcSub |>
      expand_grid(TValsNum) |>
      arrange(name, Date) |>
      mutate(Month = as.factor(Month),Year = as.factor(Year)) |>
      select(Date,Month,Year,TValsNum,name,Mean) |>
      group_by(name,Month,Year,TValsNum)
    if(any(grepl('Spill',unique(xlmExc$name)))){
      # Account for days with spillway use when above spillway
      xlmExc <- xlmExc |>
        pivot_wider(values_from = Mean) |>
        rename_with(~gsub(paste0(Proj,'.'), "", .x, fixed = TRUE)) |>
        group_by(Month,Year,TValsNum) |>
        reframe(
          DysWTempCon = length(which(`Elev-Forebay` >TValsNum & `Flow-Spill` >0)),
          PrcDysWTempCon = DysWTempCon /  length(`Elev-Forebay`)
        ) |>
        mutate(TValsNum = as.factor(TValsNum))
    }else{
      # Don't account for days with spillway use when above spillway
      xlmExc <- xlmExc      |>
        reframe(
          DysWTempCon = length(which(Mean >TValsNum)),
          PrcDysWTempCon = length(which(Mean >TValsNum)) /  length(Mean)
        ) |>
        mutate(TValsNum = as.factor(TValsNum))
    }
    xlmExc <- xlmExc      |>
      mutate(name = paste0(!!Proj,'.Elev-Forebay'))
  } # End Days above Temp Control Structure Calc

  # WSELV: Calculate time at low pool, begin, end dates
  if(any(grepl('Elev-Forebay',DataType)) &
     any(grepl('LowPool',TVals))  &
     any(grepl('Injunction',colnames(xlmExcSub)))){
    print(DataType)
    # Left off here
    OutVals <- min(xlmExcSub)
    TNms <- paste0('Above ',gsub('1','',qgtConfig[['ExcElev']][grep(Proj,names(qgtConfig[['ExcElev']]))]))
    TValsNum <- OutVals[,qgtConfig[['ExcElev']][,grep(Proj,names(qgtConfig[['ExcElev']]))]]
    xlmExc <- xlmExcSub |>
      expand_grid(TValsNum) |>
      arrange(name, Date) |>
      mutate(Month = as.factor(Month),Year = as.factor(Year)) |>
      group_by(name,Month,Year,TValsNum) |>
      reframe(
        DysWSELVExc = length(which(Mean >TValsNum)),
        PrcDysWSELVExc = length(which(Mean >TValsNum)) /  length(Mean)
      ) |>
      mutate(TValsNum = as.factor(TValsNum))
  }


  if(exists('xlmExc')){
    # Reformat the monthly exceedences to match xla exceedences
    xlmExc <- xlmExc |>
      pivot_longer(cols = -all_of(idcols),names_to = "ExcType") |>
      mutate(ExcType2 = if_else(grepl('Prc',ExcType),'PrcExc','Exc'),
             ExcType = gsub('Prc','',ExcType)) |>
      pivot_wider(names_from = 'ExcType2')|>
      mutate(ExcType = as.factor(ExcType))
  }else{
    return(NULL)
  }
  return(xlmExc)
}


#' Tabulate dates in which a threshold is exceeded
#'
#' @param DataType character; column of data to evaluate
#' @param TVals numeric; Threshold values in which to tabulate exceedence
#' @description Tabulate dates in which a threshold is exceeded
#' @return data.frame with name, begin date, end date, and max value
#' @importFrom dplyr
#' @export
#'
CalcDatesOfExc <- function(DataType,TVals){
  rm(xlExcDts)
  dTypNms <- grepl(paste0(DataType,collapse = '|'),unique(xld$name)) &
    !grepl('_S1|Targ',unique(xld$name))
  if(!any(dTypNms)){
    print(paste0('No Data Paths with ',DataType,'. Skipping any threshold analysis.'))
    return(NULL)
  }
  dTypNms <- paste(unique(xld$name)[dTypNms], collapse = "|")

  # Calculate monthly exceedences
  # Setup a subset tibbles to run calcs on.
  xlmExcSub <- xld |>
    dplyr::filter(str_detect(name,dTypNms)) |>
    mutate(Month = format(Date,'%m'),Year = format(Date,'%Y')) |>
    group_by(Year,Month,name)

  print(DataType)

  TNms <- paste0('Exc',gsub('Path','',TVals))
  TValsNum <- as.numeric(TVals)[!is.na(as.numeric(TVals))]
  if(length(TValsNum)!=0){
    xlmExcSub<- xlmExcSub |>
      expand_grid(TValsNum) |>
      arrange(name, Date) |>
      mutate(Month = as.factor(Month),Year = as.factor(Year),
             threshold = 'upper')
  }

  if(any(grepl('FishCrit',TVals))){
    # Not implemented yet...
    xlmExcSub<- xlmExcSub |>
      full_join(FishCrit_ts |>
                  rename(TValsNum = value))


    xlmExcSub |>
      dplyr::select(-c(Min,Median,Mean,Max)) |>
      rename(value = !!sym(DataType)) |>
      pivot_wider() |>
      mutate(Month = as.factor(Month),
             Year = as.factor(Year),
             threshold = as.factor(threshold),
             name = as.factor(name)) |>
      distinct() |>
      pivot_wider(names_from = threshold,values_fn = mean) |>
      arrange(Date) |>
      group_by(Month,Year,name) |>
      reframe(TargMin = lower,TargMax = upper,
              Dys7dADMAbv = length(which(!!sym(TempCompSite) > upper)),
              Dys7dADMBlw = length(which(!!sym(TempCompSite) < lower)),
              Dys7dADMBtwn = length(na.omit(!!sym(TempCompSite))) - (Dys7dADMAbv + Dys7dADMBlw)
      ) |>
      rename(Target = name) |>
      mutate(name = !!TempCompSite) |>
      filter(!is.na(Target)) |>
      distinct()

  }


  DatesOfExc <- function(x,DataCol){
    # Tabulate the date range and max value for each exceedence period
    # Add unique ID to each exceedence period
    x |>
      group_by(name,TValsNum) |>
      reframe(
        Date = Date,
        value = !!sym(DataCol),
        ExcLgcl = if_else(value > TValsNum,T,F),
        ExcID = as.factor(consecutive_id(ExcLgcl)),
        TValsNum = as.factor(TValsNum)
      ) |>
      group_by(name,TValsNum,ExcID) |>
      reframe(
        ExcIDstrt = first(Date[ExcLgcl]),
        ExcIDend = last(Date[ExcLgcl]),
        ExcMeanVal = mean(if_else(!is.finite(value)|!ExcLgcl,NA,value)),
        ExcMaxVal = max(if_else(!is.finite(value)|!ExcLgcl,NA,value))
      ) |>
      filter(!is.na(ExcIDend)) |>
      select(-ExcID)
  }

  xlExcDts <-DatesOfExc(xlmExcSub,'adm7d')
  return(xlExcDts)

}

#' Tabulate the exceedance values by year for each CWMS data path
#'
#' @param xlmExc Monthly exceedence tibble (e.g., Temp-Water,TDG,Flow)
#' @description Tabulate the number of days above or below a threshold
#' @return Creates an annuall exceedence tibble
#' @importFrom dplyr
#' @export
#'
calcThreshAnn <- function(xlmExc,months=1:12){
  # Tabulate monthly values into annual values
  xlaExc <- xlmExc |>
    dplyr::select(-starts_with('Prc')) |>
    filter(grepl(paste0(months,collapse='|'),as.character(as.numeric(Month))))
  if(any(grepl('Targ',colnames(xlmExc)))){
    xlaExc <- xlaExc |>
      group_by(name,Year,ExcType,TValsNum,Target) |>
      reframe(
        TargMin = TargMin,TargMax = TargMax,
        Exc = sum(if_else(!is.finite(Exc),NA,Exc)),
        PrcExc = Exc/365) |>
      mutate(ExcType = as.factor(ExcType))
  }else{
    xlaExc <- xlaExc |>
      group_by(name,Year,ExcType,TValsNum) |>
      reframe(Exc = sum(if_else(!is.finite(Exc),NA,Exc)),
              PrcExc = Exc/365) |>
      mutate(ExcType = as.factor(ExcType))
  }
  return(xlaExc)
}

#' Filter out only years with JDAYs above a threshold
#'
#' @param df Daily data tibble with temperature data
#' @param date_col Which column is the date column
#' @param JDAYthresh What JDAY that is the threshold for keeping years of the data.frame
#' @description Filter out only years with adequate JDAYs above a threshold for estimated emergence calcs
#' @return Creates a trimmed tibble
#' @importFrom dplyr lubridate
#' @export
#'
filter_complete_years <- function(df, JDAYthresh) {

  df %>%
    # mutate(!!date_col_sym := as.Date(!!date_col_sym)) %>%
    # mutate(year = year(!!date_col_sym)) %>%
    # Ensure one entry per day for accurate counting
    #distinct(!!date_col_sym, .keep_all = TRUE) %>%
    group_by(Year) %>%
    filter(ifelse(last(JDAY)<JDAYthresh |
                  length(which(is.na(Mean)))>364,F,T)) %>%
    #filter(n() == ifelse(leap_year(first(!!date_col_sym)), 366, 365)) %>%
    ungroup() #%>%
    #select(-year) # Remove the temporary year column
}


#' Tabulate the estimated emergence time for temperature paths
#'
#' @param xld Daily data tibble with temperature data
#' @param spawnDay Day of year(s) in which to begin emergence calculation
#' @param interpMissing logical; interpolate missing days from previous year/winter if missing?
#' @param hatchValue numeric; accumulated thermal units (degree F - days) in which hatching occurs
#' @description Tabulate the number of days above or below a threshold
#' @return Creates an annuall exceedence tibble
#' @importFrom dplyr lubridate
#' @export
#'
EstimateEmergence <- function(xld,
                          spawnDay=263, #spawnDay is the day in which to start the emergence calculation
                          interpMissing=T,
                          hatchValue=1750
){

  # Limit calculation to temp sites of interest
  xldT <- xld |>
    dplyr::filter(grepl(tsites[,names(tsites) %in% Proj],name), !grepl('Target',name)) |>
    dplyr::select(Date,name,Mean) |>
    mutate(JDAY = yday(Date),Year = format(Date,'%Y'))

  # convert to F if the mean is less than 32
  if(mean(xldT$Mean,na.rm=T)<32){
    xldT <- xldT |>
      mutate(Mean = c2f(Mean))
  }


  # Remove years with incomplete data
  xldT <- filter_complete_years(xldT,JDAYthresh=min(spawnDay))

  xldT |>
    group_by(Year) |>
    reframe(ndaysMiss = length(which(is.na(Mean)))) |>
    print(n = 30)

  EggEmrgSum <-
    foreach(yr = unique(xldT$Year),.combine='rbind') %do% {
      ty <- xldT %>% filter(Year == yr) %>%
        filter(JDAY>0) |>
        select(JDAY,Mean)
      # if(max(ty$JDAY,na.rm = T)<min(spawnDay)){
      #   atuSum <- data.frame(atu=NA,atu.d=NA,nmiss=NA,spawnDay=spawnDay,Year=yr)
      # }else{
        atuSum <- foreach(sd = spawnDay,.combine='rbind') %do% {
          if(min(ty[!is.na(ty$Mean),'JDAY'],na.rm = T)>sd){
            atusd <- data.frame(atu=NA,atu.d=NA,nmiss=NA,spawnDay=sd,Year=yr)
          }else{
            atusd <- calcEmergenceTiming(tout = ty,atu.day = sd,hatchValue=hatchValue) %>%
              mutate(spawnDay = sd,Year = yr)
            # December is interpolated between November and January
          }
          return(atusd)
      # }
      }
      return(atuSum)
    }

  return(EggEmrgSum |> filter(!is.na(atu.d)))
}


#' Calculate the estimated egg emergence date
#'
#' @param tout #tout is a dataframe with the first column as a numeric Julian day, 2nd column temperature data
#' @param spawnDay Day of year(s) in which to begin emergence calculation
#' @param interpMissing logical; interpolate missing days from previous year/winter if missing?
#' @param hatchValue numeric; accumulated thermal units (degree F - days) in which hatching occurs
#' @description Tabulate the estimated egg emergence
#' @return data.frame with atu mm/dd, atu day of year, nmiss (missing days)
#' @importFrom dplyr lubridate
#' @export
#'
calcEmergenceTiming<-function(tout,
                              atu.day=263, #atu.day is the day in which to start the emergence calculation
                              interpMissing=T,
                              hatchValue=1750
){
  tout<-as.data.frame(tout)
  if(interpMissing){
    jd<-1:366
    jdi<-!jd%in%unique(floor(tout$JDAY))
    nmiss <- length(which(jdi))
    if(any(jdi)){
      # fill missing days
      tout<-data.frame(JDAY=jd,
                       Temp=approx(x=tout[,1],y=tout[,2],xout=jd,rule=2)$y)
    }
  }
  if(floor(tout$JDAY[1])<=2){
    atu.temps<-rbind(tout[tout$JDAY>atu.day,],
                     tout[tout$JDAY<atu.day,])
  }else{
    atu.temps<-tout[tout$JDAY>atu.day,]
  }

  if(mean(atu.temps[,2],na.rm=T)<32){
    atu.temps[,-1]<-atu.temps[,-1]*(9/5)+32 # convert to F
  }
  atu.temps$JDAY[atu.temps$JDAY<atu.day]<-
    atu.temps$JDAY[atu.temps$JDAY<atu.day]+max(atu.temps$JDAY)
  atu.temps<-atu.temps[!apply(apply(atu.temps,2,is.na),1,any),]
  #print(summary(atu.temps))
  cum.dif<-atu.temps
  cum.dif[,-1]<-NA
  if(ncol(atu.temps)>2){
    cum.dif[,-1]<-apply(atu.temps[,-1]-32,2,cumsum)
    atu.d<-apply(cum.dif[,-1],2,function(x){cum.dif[x>=hatchValue,1][1]})
  }else{
    cum.dif[,-1]<-cumsum(atu.temps[,-1]-32)
    atu.d<-cum.dif[cum.dif[,-1]>=hatchValue,1][1]
  }
  atu<-as.data.frame(t(format(as.Date(as.numeric(atu.d),origin=as.Date('2010-12-31')),'%m/%d')),stringsAsFactors=F)
  atu.d<-as.data.frame(t(round(as.numeric(atu.d))),stringsAsFactors=F)
  return(data.frame(atu=as.character(atu),atu.d=as.numeric(atu.d),nmiss = nmiss))
}
