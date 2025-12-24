
# Get Operator Logbook Data
# To-Do: Ask Amory for other Rdata files
getDETBCLScrapedOpsLogBooks <- function(
    gatePath = file.path(RdataDir,'GateOpenings','DETBCL'),
    previousScrapeFile = 'DETBCL_logbookScraped2014-2025.Rdata',
    newDataFileName){
  #  1) Read in Operator Log books (xlsx)
  #  2) From each Day sheet, extract spillway, RO gate openings and flowrates
  #  3) Save as a timeseries
  # Currently only getting DET-BCL
  # library(readr)
  # library(lubridate)
  # library(dplyr)
  # library(ggplot2)
  # library(foreach)
  # library(tidyr)
  library(readxl)
  #source(file.path(RdataDir,'errs.r'))
  fldrs <- list.dirs(gatePath,full.names = F,recursive = F)
  fldrs <- fldrs[grepl('Monthly|monthly|DET-BCL 2014|DET-BCL 2015|DET-BCL 2016',fldrs) &
                   !grepl('PSS',fldrs) ] ##|monthly|DET-BCL

  #scrape <- T #FALSE

  # Add to previous scrape file
  load(file.path(gatePath,previousScrapeFile))
  y1 <- y
  #y1$DateTime <- as.POSIXct(y1$DateTime+ 8*60*60,tz='PST8PDT')
  #attr(y$DateTime,'tzone')
  summary(y)
  rm(y)

  bdateNew <- max(y1$DateTime)
  # Take a subset of the spreadsheets to scrape and focus on new spreadsheets to append to the old file
  fldrs <- fldrs[as.numeric(gsub(' Monthly','',fldrs))>=as.numeric(format(bdateNew,'%Y'))]
  dtRng <- paste0(bdateNew,edate)

  give.n <- function(x){
    return(c(y = max(x), label = ceiling(length(x)/24)))
    # experiment with the multiplier to find the perfect position
  }





  fls <- unlist(lapply(fldrs,function(x) strsplit(x,' ')[[1]][[1]]))
  CellParams <- list(DET = list(o_range = 'AO6:AY31',q_range = 'AZ6:BK31'),
                     BCL = list(oq_range = 'BL6:BS31')
  )

  readLog <- function(...){ #file,sheet,skip,range
    x <- suppressMessages(read_excel(col_types = 'numeric',...))
    x <- x[!apply(apply(x,1,is.na),2,all),]
    colnames(x) <- gsub('Discharge','Total',
                        gsub('Gate ','SWG',
                             gsub('Lower ','DET_LRO',
                                  gsub('Upper ','DET_URO',colnames(x)))))
    return(x)
  }
  renameCols <- function(x,prfx,endng){
    colnames(x)[-1] <- paste0(prfx,colnames(x)[-1],endng)
    return(x)
  }

  #
  # yr = 1
  # fl = 1
  for(yr in (1:length(fldrs))){
    yrFldr <- fldrs[yr]
    Yr <- gsub('-','',gsub("[^0-9.-]",'',yrFldr))
    # Read in files
    fls <- list.files(file.path(gatePath,yrFldr),pattern = '.xlsm')
    fls <- fls[!grepl('~|master',fls)]
    Names <- c('DET','BCL')
    Month <- substr(unlist(lapply(fls,function(x) strsplit(x,' ')[[1]][[1]])),1,3)
    if(exists('x')) {rm(x)}
    for(fl in (1:length(fls))){
      flnm = file.path(gatePath,yrFldr,fls[fl])
      moYr <- strsplit(gsub('.xlsm','',fls[fl]),' ')
      moYr <- paste0(moYr[[1]][[2]],'-',Month)
      shts <- excel_sheets(flnm)
      shts <- shts[!is.na(as.numeric(shts))]
      for(sh in (1:length(shts))){
        print(paste(Yr,'-',Month[fl],'-',shts[sh]))
        #print(paste('Reading Sheet',moYr,sh))
        DmNm1 = paste0(names(CellParams)[1],'_')
        DmNm2 = paste0(names(CellParams)[2],'_')
        # Loop through each dam, settings setup above
        #Detroit
        dy1o <- readLog(path = flnm,sheet = shts[sh],
                        range = CellParams[[1]][['o_range']])
        colnames(dy1o)[-1] <- paste0(DmNm1,colnames(dy1o)[-1],'_Opn_ft')
        dy1q <- readLog(path = flnm,sheet = shts[sh],
                        range = CellParams[[1]][['q_range']])
        colnames(dy1q)[-1] <- paste0(DmNm1,colnames(dy1q)[-1],'_Flow_cfs')
        # Big Cliff
        dy2q <- readLog(path = flnm,sheet = shts[sh],
                        range = CellParams[[2]][['oq_range']])
        colnames(dy2q)[2:4] <- paste0(DmNm2,'SWG',c(1:3),'_Opn_ft')
        colnames(dy2q)[5:7] <- paste0(DmNm2,'SWG',c(1:3),'_Flow_cfs')
        monDat <-  dy1q |> pivot_longer(cols = -Time) |>
          bind_rows(dy1o |> pivot_longer(cols = -Time),
                    dy2q |> pivot_longer(cols = -Time)) |>
          mutate(DateTime = ymd_h(paste0(Yr,'-',Month[fl],'-',shts[sh],' ',Time),tz='PST8PDT'),
                 value = round(value,2)) |>
          dplyr::select(-Time) |>
          drop_na()
        if(sh == 1){
          m = monDat
        }else{
          m = rbind(m,monDat)
        }
      }
      if(!exists('x')){
        x = m
      }else{
        if(any(colnames(x) %in% 'Time')){
          x = x |> dplyr::select(-Time)
        }
        x = rbind(x,m)
      }
    } # End loop through monthly excel files
    if(!exists('y')){
      y = x  %>% arrange(DateTime)  %>%
        filter(!is.na(DateTime) & !is.na(value))
    }else{
      y = rbind(y,x)
    }
    write_csv(x %>% arrange(DateTime) %>% filter(!is.na(DateTime)),
              file = file.path(gatePath,paste0('DETBCL_logbookScraped',Yr,'.csv')))
  }

  if(exists('y1')){
    y <- full_join(y1,y)
  }
  y <- y |> distinct()

  save(y,file = file.path(gatePath,paste0('DETBCL_logbookScraped2014-',format(edate,'%Y'),'.Rdata')))

}

MergeGateOpsWflowWQ <- function(DETBCLGateOpsFileName){

  # Find the DET and BCL data in the data that was saved from the get_cwms function above
  load(DETBCLGateOpsFileName)

  CleanCWMSnames <- function(x) {
    # Clean up CWMS names into shorter names. Move this outside this function eventually.
    colnames(x) <- gsub('.Inst|.0|.Best|.MIXED-REV|.MIXED_REV|.1Hour|.3Hours|.15Minutes|.CBT-REV|.Ave','',colnames(x))
    colnames(x) <- gsub('.Temp-Water','_Temp',colnames(x))
    colnames(x) <- gsub('.Flow','_Flow',colnames(x))
    colnames(x) <- gsub('.Elev-Forebay','_ElevFB',colnames(x))
    colnames(x) <- gsub('.D0,5','.D0.5',colnames(x))
    colnames(x) <- gsub('_S1-','_',colnames(x))
    colnames(x) <- gsub('.%-Saturation-TDG','_TDG',colnames(x))
    return(x)
  }

  load(RdataFls[grepl('DET',RdataFls)])
  xw <-   CleanCWMSnames(xw)
  # Left off here!!!
  wqops <- xw |>
    dplyr::rename(DET_TDG = `DET-TailwaterRightBank.%-Saturation-TDG.Inst.0.0.Best`)

  load(RdataFls[grepl('BCL',RdataFls)])


  # wqops <- wqops |>
  #   full_join(xw |>
  #               dplyr::rename(
  #                 BCL1_TDG = `BCL-Fishtrap.%-Saturation-TDG.Inst.0.0.Best`,
  #                 BCL2_TDG = `BCL-GunCreekDownstream.%-Saturation-TDG.Inst.0.0.Best`,
  #                 BCLO_TDG = `BCLO.%-Saturation-TDG.Inst.0.0.Best`,
  #                 BCL_FBelv = `BCL.Elev-Forebay.Inst.0.0.Best`,
  #                 BCL_GEN_Flow_cfs = 'BCL.Flow-Gen.Ave.1Hour.1Hour.Best',
  #                 DET_GEN_Flow_cfs = 'DET.Flow-Gen.Ave.1Hour.1Hour.Best') |>
  #     mutate(
  #       DET_GEN = Q_DET_GEN,
  #       DET_QsTot = Q_DET_SWG +Q_DET_URO +Q_DET_LRO,
  #       DET_QTot = DET_GEN + DET_QsTot,
  #       DET_Qw = (DET_GEN/DET_QTot)*DET_GEN + (1-DET_GEN/DET_QTot)*DET_QsTot,
  #       BCL_QsTot = Q_BCL_SWG,
  #       DET_Qs = DET_QsTot / (n_DET_SWG +n_DET_URO +n_DET_LRO),
  #       BCL_GEN = Q_BCL_GEN,
  #       BCL_Qs = ifelse(n_BCL_SWG>0,BCL_QsTot / n_BCL_SWG,NA),
  #       BCL_QTot = BCL_GEN + BCL_QsTot,
  #       BCL_Qw = (BCL_GEN/BCL_QTot)*BCL_GEN + (1-BCL_GEN/BCL_QTot)*BCL_QsTot
  #     )
  #
  # bcTDGNew <- bclTDGNew %>%
  #   full_join(bcloTDGNew) |>
  #   full_join(detTWTDGNew) |>
  #   full_join(bclTDGNew) |>
  #   pivot_longer(cols = -Date) |>
  #   filter(!is.na(value))
  #
  # summary(bcTDGNew)
  #
  # # Load previous CWMS data
  # load(file.path(wd,'DETBCL_logbookScrapedWTDG2014-01-01-2024-12-31.Rdata'))
  # # Load current scraped log file data
  # load(file.path(wd,'DETBCL_logbookScraped2014-2025.Rdata'))
  #
  # #merge Ops and recent TDG data with BCL-Fishtrap timeframe
  # wqops <- wqops |>
  #   full_join(bcTDGNew) |>
  #   full_join(bclElevNew |> pivot_longer(cols = -Date)) |>
  #   full_join(PowerNew|> pivot_longer(cols = -Date)) |>
  #   full_join(y |>
  #               rename(Date = DateTime) |>
  #               filter(Date %in% bcTDGNew$Date))  |>
  #   mutate(name = as.factor(gsub('DET_DET_','DET_',name))) |>
  #   distinct()
  # levels(wqopsall$name)
  # summary(wqopsall)
  # attr(wqops$Date,'tzone')
  # wqops <- wqopsall
  # rm(wqopsall)
  #
  # #PowerNew |>
  # bclTDGNew |>
  #   filter(Date >= as.POSIXct('2024-06-20 12:00:00') & Date <= as.POSIXct('2024-06-20 23:00:00'))
  #
  #
  # wqops <- wqops %>%
  #   mutate(name = gsub('.Flow-Gen.Ave.1Hour.1Hour.Best','_GEN_Flow_cfs',name)) |>
  #   mutate(name = as.character(gsub('DET_DET','DET',name)),
  #          Site = factor(ifelse(grepl('DET',name),'DET','BCL'),levels = c('BCL','DET'),ordered=T),
  #          Data = factor(ifelse(grepl('TDG',name),'TDG',
  #                               ifelse(grepl('Opn',name),'Opn_ft','Flow_cfs')),
  #                        levels = c('Opn_ft','Flow_cfs','TDG'),ordered=T)#,
  #   )
  #
  # GtCmbs <- wqops |>
  #   separate_wider_delim(name,delim = '_',names = c('Name2','Gate','Data2','Units'),
  #                        too_few = "align_start") |>
  #   mutate(Gate = as.factor(Gate)) |>
  #   dplyr::select(-c(Name2,Data2,Units)) |>
  #   filter(grepl('Flow',Data),Gate!='Total') |> #!is.na(Gate),
  #   dplyr::select(-c(Data)) #|>
  # #drop_na() |>
  # #dplyr::rename(name = Site)
  # unique(GtCmbs$Gate)
  # summary(GtCmbs)
  #
  # print(GtCmbs |>
  #         filter(Date > as.POSIXct('2024-01-29') & Date < as.POSIXct('2024-12-31'),
  #                Gate == "GEN",Site =='DET'),n=500) #|>
  #
  # OpsNum <- function(Site,Gate,value,func){
  #   dgeni<- value[grepl('DET_GEN',paste0(Site,'_',Gate))]
  #   dlroi<- value[grepl('DET_LRO',paste0(Site,'_',Gate))]
  #   duroi<- value[grepl('DET_URO',paste0(Site,'_',Gate))]
  #   dswgi<- value[grepl('DET_SWG',paste0(Site,'_',Gate))]
  #   bgeni<- value[grepl('BCL_GEN',paste0(Site,'_',Gate))]
  #   bswgi<- value[grepl('BCL_SWG',paste0(Site,'_',Gate))]
  #   tibble(DET_GEN = func(dgeni),
  #          DET_LRO = func(dlroi),
  #          DET_URO = func(duroi),
  #          DET_SWG = func(dswgi),
  #          BCL_GEN = func(bgeni),
  #          BCL_SWG = func(bswgi))
  # }
  #
  #
  # GtCmbsNum <- GtCmbs |>
  #   drop_na() |>
  #   group_by(Date) |>
  #   reframe(Q = OpsNum(Site,Gate,value,sum)) |>
  #   unnest(Q,names_sep = "_") |>
  #   full_join(
  #     GtCmbs |>
  #       drop_na() |>
  #       group_by(Date) |>
  #       reframe(n = OpsNum(Site,Gate,value,length)) |>
  #       unnest(n,names_sep = "_"))
  #
  # summary(GtCmbsNum )
  #
  #
  #
  # # Get Power Flow  data
  #
  # # Get BCL forebay data
  # bclElevNew <- get_cwms('BCL.Elev-Forebay.Inst.0.0.Best',
  #                        start_date=bdateNew,end_date=edate)
  # # Get TDG data from Niagara
  # bcloTDGNew <- get_cwms('BCLO.%-Saturation-TDG.Inst.0.0.Best',
  #                        start_date=bdateNew,end_date=edate)
  # bclTDGNew <- get_cwms(c('BCL-Fishtrap.%-Saturation-TDG.Inst.0.0.Best',
  #                         'BCL-GunCreekDownstream.%-Saturation-TDG.Inst.0.0.Best'),
  #                       start_date=bdateNew,
  #                       end_date=edate)
  # # bclTDGNew |>
  # #   #mutate(Date = as.POSIXct(Date,tz ='PST8PDT')) |>
  # #   filter(Date >= as.POSIXct('2024-06-20 12:00:00') & Date <= as.POSIXct('2024-06-20 23:00:00'))
  #
  # # Get DET tailwater TDG data
  # detTWTDGNew <- get_cwms(c(
  #   'DET-TailwaterRightBank.%-Saturation-TDG.Inst.0.0.Best',
  #   'DET-TailwaterLeftBank.%-Saturation-TDG.Inst.0.0.Best'),
  #   start_date=bdateNew,end_date=edate)
  # detTWTDGNew <- detTWTDGNew |>
  #   dplyr::rename(DETLB_TDG = `DET-TailwaterLeftBank.%-Saturation-TDG.Inst.0.0.Best`,
  #                 DETRB_TDG = `DET-TailwaterRightBank.%-Saturation-TDG.Inst.0.0.Best`)
  #
  # bcTDGNew <- bclTDGNew %>%
  #   full_join(bcloTDGNew) |>
  #   full_join(detTWTDGNew) |>
  #   full_join(bclTDGNew) |>
  #   pivot_longer(cols = -Date) |>
  #   filter(!is.na(value))
  #
  # summary(bcTDGNew)
  # # Left off here: Need to decide what is the most current data and save!!!
  #
  #
  # #merge Ops and recent TDG data with BCL-Fishtrap timeframe
  # wqops <- bcTDGNew |>
  #   full_join(bclElevNew |> pivot_longer(cols = -Date)) |>
  #   full_join(PowerNew|> pivot_longer(cols = -Date)) |>
  #   full_join(y |>
  #               rename(Date = DateTime) |>
  #               filter(Date %in% bcTDGNew$Date))  |>
  #   mutate(name = as.factor(gsub('DET_DET_','DET_',name))) |>
  #   distinct()
  # levels(wqops$name)
  # summary(wqops)
  # attr(wqops$Date,'tzone')



}

