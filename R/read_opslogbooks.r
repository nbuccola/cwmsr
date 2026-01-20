<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e
#' Get DET Operator Logbook Data
#'
#' @param gatePath path to DET gate opening spreadsheets
#' @param previousScrapeFile Previous RDATA file that will be merged with
#' @param newDataFileName Name of new gate data RDATA file to save
#' @description Scrapes DET Operator Logbook spreadsheets (monthly)
#' @return Saves an Rdata file
#' @importFrom readxl dplyr
#' @export
#'
<<<<<<< HEAD
# Get DET Operator Logbook Data
=======
=======

# Get DET Operator Logbook Data
>>>>>>> fa19c5f11a312acbfcca8860bc583de252d3b5e9
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e
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

<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e
#' Merge Gate operations data
#'
#' @param DETBCLGateOpsFileName Scraped DET operating room logbook data from Norm
#' @param LOPlogbookFileName Scraped LOP operating room logbook data from Amory
#' @param GtsCWMS Gate opening data from CWMS/GDACS
#' @description Merges gate operations data
#' @return A tibble with all gate operations data
#' @importFrom ggplot2 dplyr purrr
#' @export
#'
<<<<<<< HEAD
=======
=======
>>>>>>> fa19c5f11a312acbfcca8860bc583de252d3b5e9
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e
MergeGateOpsWflowWQ <- function(DETBCLGateOpsFileName,LOPlogbookFileName,GtsCWMS){


  # Load DET Log book data
  load(DETBCLGateOpsFileName)
  DETBCL_lb <- y |>
    mutate(name = gsub('DET_DET','DET',
                       #gsub('Total_Flow','TotalFlow',
                            gsub('Opn_ft','Opn',
                                 gsub('Flow_cfs','Flow',name)))) |>
    filter(name!='Total') |>
    separate_wider_delim(name,delim='_',names = c('name','gate','param')) |>
    mutate(data = 'LogBook')

  # Load LOP log book data
  load(LOPlogbookFileName)
  df.LB.CWMS <- df.LB.CWMS |>
    dplyr::rename(gate = Type,name = Location,Opn = Sum.Openings.ft,Flow = Spill.cfs) |>
    dplyr::rename(DateTime = Datetime) |>
    group_by(DateTime) |>
    mutate(Flow = if_else(Status == 'Open',Flow,0)) |>
    ungroup() |>
    dplyr::select(-c('FB.elev.ft','TW.elev.ft',`Flow-Gen.cfs`,'Status','Date')) |>
    pivot_longer(cols = c('Flow','Opn'),names_to = 'param') |>
    mutate(data = 'LogBook')

  # Load the FOS logbook/flow data

<<<<<<< HEAD
=======
<<<<<<< HEAD

=======
>>>>>>> fa19c5f11a312acbfcca8860bc583de252d3b5e9
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e
  # Need to merge with GDACS data for FAL, HCR, CGR, GPR, FOS, BCL, DET

  # Find the DET and BCL data in the data that was saved from the get_cwms function above
  GtOpnWV <- GtsCWMS |>
    full_join(df.LB.CWMS) |>
    full_join(DETBCL_lb) |>
    mutate(gate = gsub('SWY','SWG',gsub('SB','SWG',gate)))
<<<<<<< HEAD
=======
<<<<<<< HEAD
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e

  return(GtOpnWV)
}


#' Create a csv file for Will that has gate-type specific flow at each project for use in thermal plots
#'
#' @param SaveName Name of file to save
#' @description Create a csv file for Will that has gate-type specific flow at each project for use in thermal plots
#' @return Writes a csv file for Will that has gate-type specific flow at each project for use in thermal plots
#' @importFrom ggplot2 dplyr purrr
#' @export
#'
MakeProjFlowData4Will <- function(SaveName){
  # Extract the total flow by outlet, for python plots
  #clrs<- cbPalette[2:4]

  GtDatProjList <- list(NA)
  GtMonNmDysList <- list(NA)
  proj <- unique(GtOpnWV$name)[!grepl(c('DOR|COT|FRN|GPR|FOS'),unique(GtOpnWV$name))]
  for(p in proj){
    print(p)
    GtDatProj <- GtOpnWV |>
      filter(DateTime > as.POSIXct('2025-01-01'),
             #!grepl('E',gate),
             grepl(p,name)) |>
             select(-name) |>
      mutate(gate = as.factor(gate),data= as.factor(data),param = as.factor(param)) |>
      arrange(DateTime)

    if(p == 'DET'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T),
                URO = sum(across(contains('URO')),na.rm=T),
                LRO = sum(across(contains('LRO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct() #|>
        #mutate(Gate_Type = factor(Gate_Type,levels = c('Total','Gen','Spill','SWG','URO','LRO'),ordered = T))
    }

    if(p == 'BCL'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct() #|>
    }

    if(p == 'HCR'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }
    if(p == 'LOP'){
      # LOP has no gate openings in GDACS, but logbook data has openings.
      GtDatProj <- GtDatProj |>
        # pivot_wider(names_from =  c(gate,param,data)) |>
        group_by(DateTime) |>
        filter(grepl('Flow',param) & grepl('Out|Spill|Gen',gate) & grepl('CWMS',data) |
              grepl('Flow',param) & grepl('RO|SWG',gate) & grepl('LogBook',data)) |>
        select(-param,data) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T),
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }
    if(p == 'DEX'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'FAL'){
      # GDACS opn/closed for FAL fishhorns, RO openings to show usage
      # Sarah working on verifying from scanned logbooks
      GtDatProj <- GtDatProj |>
        filter(param != 'Flow') |>
        select(-data,-param) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime')) |>
        group_by(DateTime) |>
        mutate(RO = sum(RO1,RO2),
               FishhornE765 = sum(FishhornE765ft18in,FishhornE765ft24in,FishhornE765ft30in),
               FishhornE720 = sum(FishhornE720ft18in,FishhornE720ft24in,FishhornE720ft30in),
               FishhornE800 = sum(FishhornE800ft18in,FishhornE800ft24in,FishhornE800ft30in)) |>
        select(DateTime,RO,FishhornE720,FishhornE765,FishhornE800) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Open',values_drop_na = TRUE) |>
        distinct() |>
        ungroup()
    }

    if(p == 'CGR'){
      # Need to estimate DT based on GDACS/CWMS Opening and CWMS Spill
      # DT was operated 2025-10-21 0705-1350
      GtDatProj <- GtDatProj |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate,param,data),id_cols = c('DateTime')) |>
        mutate(DTGate_Opn = sum(if_else(DTGate1_Opn_CWMS <0,0,DTGate1_Opn_CWMS),
                                if_else(DTGate2_Opn_CWMS <0,0,DTGate2_Opn_CWMS),na.rm=T),
               DTGate_Flow = if_else(as.Date(DateTime) == as.POSIXct('2025-10-21') &
                                       DTGate_Opn>0 & (round(RO1_Opn_CWMS)<=0 & round(RO2_Opn_CWMS)<=0),
                                     Spill_Flow_CWMS,0)) |>
        dplyr::select(-contains('Opn')) |>
        rename_with(~ gsub("_LogBook", "",gsub("_CWMS", "", .x, fixed = TRUE))) |>
        mutate(RO_Flow = if_else(DTGate_Flow>0,0,RO_Flow)) |>
        #filter(DTGate_Flow>0) |>
        #filter(DateTime > as.POSIXct('2025-10-21')) |>
        #print(n = 100)
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out_Flow,Gen = Gen_Flow,#Spill = Spill,
                DT = DTGate_Flow,
                RO = RO_Flow) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'GPR'){
      # GPR has GDACS RO, but don't quite trust it yet,
      # RO and spillway gate openings are based on operator logbooks
      # Left off here!!! Waiting for Jack's data
      GtDatProj <- GtDatProj |>
        # pivot_wider(names_from =  c(gate,param,data)) |>
        group_by(DateTime) |>
        filter(grepl('Flow',param) & grepl('Out|Spill|Gen',gate) & grepl('CWMS',data) |
                 grepl('Flow',param) & grepl('RO|SWG',gate) & grepl('LogBook',data)) |>
        select(-param,data) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T),
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }


    summary(GtDatProj)

    # GtDatProj |>
    #   filter(grepl('6',month(DateTime))) |>
    #   print(n = 300)
    if(p == 'FAL'){
      dataVar = "Open"
      ylb = 'Gate Opening'
    }else{
      dataVar = "Flow_cfs"
      ylb = 'Flow, in cfs'
    }

    GtMonAvg <- GtDatProj |>
      mutate(Month = factor(format(DateTime,'%b'),ordered = T,levels = month.abb)) |>
      group_by(Month,Gate_Type) |>
      reframe(Mean = round(mean(get(dataVar),na.rm=T))) |>
      pivot_wider(names_from = Month,values_from = Mean) |>
      mutate(Usage= paste0('AvgMnthly',dataVar))
    GtMonNmDys <-
      GtMonAvg |>
      full_join(
        GtDatProj |>
        mutate(Date = as.Date(DateTime)) |>
        group_by(Date,Gate_Type) |>
        reframe(NmHrs = length(which(get(dataVar)>0))) |>
        mutate(Month = factor(format(Date,'%b'),ordered = T,levels = month.abb)) |>
        group_by(Month,Gate_Type) |>
        reframe(
          NmDys = length(which(NmHrs>0)),
          AvgDlyHrs = round(mean(NmHrs,na.rm=T))) |>
        pivot_longer(cols = -c(Month,Gate_Type)) |>
        pivot_wider(names_from = Month) |>
        filter(!grepl('Total',Gate_Type)) |>
        dplyr::rename(Usage = name)) |>
      arrange(Usage) |>
      mutate(name = p)


    figTsByGtParams <-
      ggplot( GtDatProj,
             aes(x=DateTime,y = get(dataVar),colour=Gate_Type,group = Gate_Type)) +
      geom_line(alpha = 0.6) +
      ylab(ylb) +
      xlab('') +
      scale_x_datetime(breaks = seq.POSIXt(from = min(GtDatProj$DateTime),to = max(GtDatProj$DateTime),by = 'month'),
                   date_labels = '%b') +
      #scale_color_manual(values = clrs) +
      scale_color_brewer(palette = "Set2") +
      theme(strip.text.y.left = element_text(angle = 0),
            axis.text.x=element_text(size = 10,angle=45,hjust=1),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            strip.placement = "outside",
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 10),
            strip.background = element_rect(fill=NA),
            legend.title = element_blank(),
            legend.position = 'top'
      ) +
      ggtitle(paste0('2025 ',p,' Gate-Type Summary of ',dataVar))

    figTsByGtParams

    ggplot2::ggsave(plot = figTsByGtParams,
                    filename = file.path(WriteDir,'GateOpenings',paste0(p,'_',SaveName,'.png')),
                    device='png',width=9,height=4)
    write.csv(GtMonAvg,row.names = F,file.path(WriteDir,'GateOpenings',paste0(p,'_','Monthly_',SaveName,'.csv')))
    GtDatProjList[[p]] <- GtDatProj |>
      dplyr::rename(value = !!dataVar) |>
      mutate(name = p,param = dataVar)
    GtMonNmDysList[[p]] <- GtMonNmDys
    rm(GtMonNmDys,GtDatProj)
<<<<<<< HEAD

  return(GtOpnWV)
}


#' Plot the daily averages for each parameter
#'
#' @param xld daily data
#' @description Plot daily averages for each parameter
#' @return writes a series of plots based on xl, xld to RdataDir; Writes an annual summary of data (xAnn) to the current environment
#' @importFrom ggplot2
#' @export
#'
MakeProjFlowData4Will <- function(SaveName){
  # Extract the total flow by outlet, for python plots
  #clrs<- cbPalette[2:4]

  proj <- unique(GtOpnWV$name)
  for(p in proj){

    GtDatProj <- GtOpnWV |>
      filter(DateTime > as.POSIXct('2025-01-01'),
             #!grepl('E',gate),
             grepl(p,name)) |>
             select(-name) |>
      mutate(gate = as.factor(gate),data= as.factor(data),param = as.factor(param)) |>
      arrange(DateTime)

    if(p == 'DET'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T),
                URO = sum(across(contains('URO')),na.rm=T),
                LRO = sum(across(contains('LRO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'BCL'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'HCL'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }
    if(p == 'LOP'){
      # LOP has no gate openings in GDACS, but logbook data has openings.
      GtDatProj <- GtDatProj |>
        # pivot_wider(names_from =  c(gate,param,data)) |>
        group_by(DateTime) |>
        filter(grepl('Flow',param) & grepl('Out|Spill|Gen',gate) & grepl('CWMS',data) |
              grepl('Flow',param) & grepl('RO|SWG',gate) & grepl('LogBook',data)) |>
        select(-param,data) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T),
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }
    if(p == 'DEX'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'FAL'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'CGR'){
      # Need to estimate DT based on GDACS/CWMS Opening and CWMS Spill
      # DT was operated 2025-10-21 0705-1350
      GtDatProj <- GtDatProj |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate,param,data),id_cols = c('DateTime')) |>
        mutate(DTGate_Opn = sum(if_else(DTGate1_Opn_CWMS <0,0,DTGate1_Opn_CWMS),
                                if_else(DTGate2_Opn_CWMS <0,0,DTGate2_Opn_CWMS),na.rm=T),
               DTGate_Flow = if_else(as.Date(DateTime) == as.POSIXct('2025-10-21') &
                                       DTGate_Opn>0 & (round(RO1_Opn_CWMS)<=0 & round(RO2_Opn_CWMS)<=0),
                                     Spill_Flow_CWMS,0)) |>
        dplyr::select(-contains('Opn')) |>
        rename_with(~ gsub("_LogBook", "",gsub("_CWMS", "", .x, fixed = TRUE))) |>
        mutate(RO_Flow = if_else(DTGate_Flow>0,0,RO_Flow)) |>
        #filter(DTGate_Flow>0) |>
        #filter(DateTime > as.POSIXct('2025-10-21')) |>
        #print(n = 100)
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out_Flow,Gen = Gen_Flow,#Spill = Spill,
                DT = DTGate_Flow,
                RO = RO_Flow) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }


    summary(GtDatProj)

    # GtDatProj |>
    #   filter(grepl('6',month(DateTime))) |>
    #   print(n = 300)

    GtMonAvgQ <- GtDatProj |>
      mutate(Month = factor(format(DateTime,'%b'),ordered = T,levels = month.abb)) |>
      group_by(Month,Gate_Type) |>
      reframe(Mean = round(mean(Flow_cfs,na.rm=T))) |>
      pivot_wider(names_from = Month,values_from = Mean)

    GtMonNmDys <- GtDatProj |>
      mutate(Date = as.Date(DateTime)) |>
      group_by(Date,Gate_Type) |>
      reframe(NmHrs = length(which(Flow_cfs>0))) |>
      mutate(Month = factor(format(Date,'%b'),ordered = T,levels = month.abb)) |>
      group_by(Month,Gate_Type) |>
      reframe(NmDys = length(which(NmHrs>0)),
              AvgDlyHrs = round(mean(NmHrs,na.rm=T))) |>
      pivot_longer(cols = -c(Month,Gate_Type)) |>
      pivot_wider(names_from = Month) |>
      filter(!grepl('Total',Gate_Type)) |>
      dplyr::rename(Usage = name)|>
      arrange(Usage)


    figTsByGtParams <-
      ggplot( GtDatProj,
             aes(x=DateTime,y = Flow_cfs,colour=Gate_Type,group = Gate_Type)) +
      geom_line(alpha = 0.6) +
      ylab('Flow, in cfs') +
      xlab('') +
      scale_x_datetime(breaks = seq.POSIXt(from = min(GtDatProj$DateTime),to = max(GtDatProj$DateTime),by = 'month'),
                   date_labels = '%b') +
      #scale_color_manual(values = clrs) +
      scale_color_brewer(palette = "Set2") +
      theme(strip.text.y.left = element_text(angle = 0),
            axis.text.x=element_text(size = 10,angle=45,hjust=1),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            strip.placement = "outside",
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 10),
            strip.background = element_rect(fill=NA),
            legend.title = element_blank(),
            legend.position = 'top'
      ) +
      ggtitle(paste0('2025 ',p,' Gate-Specific Flow'))

    figTsByGtParams

    ggplot2::ggsave(plot = figTsByGtParams,
                    filename = file.path(WriteDir,'GateOpenings',paste0(p,'_',SaveName,'.png')),
                    device='png',width=9,height=4)
    write.csv(GtMonAvg,row.names = F,file.path(WriteDir,'GateOpenings',paste0(p,'_','Monthly_',SaveName,'.csv')))
    GtDatProjList[[p]] <- GtDatProj
    GtMonNmDysList[[p]] <- GtMonNmDys
  }

=======
=======

  return(GtOpnWV)
}


#' Plot the daily averages for each parameter
#'
#' @param xld daily data
#' @description Plot daily averages for each parameter
#' @return writes a series of plots based on xl, xld to RdataDir; Writes an annual summary of data (xAnn) to the current environment
#' @importFrom ggplot2
#' @export
#'
MakeProjFlowData4Will <- function(SaveName){
  # Extract the total flow by outlet, for python plots
  #clrs<- cbPalette[2:4]

  proj <- unique(GtOpnWV$name)
  for(p in proj){

    GtDatProj <- GtOpnWV |>
      filter(DateTime > as.POSIXct('2025-01-01'),
             #!grepl('E',gate),
             grepl(p,name)) |>
             select(-name) |>
      mutate(gate = as.factor(gate),data= as.factor(data),param = as.factor(param)) |>
      arrange(DateTime)

    if(p == 'DET'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T),
                URO = sum(across(contains('URO')),na.rm=T),
                LRO = sum(across(contains('LRO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct() #|>
        #mutate(Gate_Type = factor(Gate_Type,levels = c('Total','Gen','Spill','SWG','URO','LRO'),ordered = T))
    }

    if(p == 'BCL'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct() #|>
    }

    if(p == 'HCL'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }
    if(p == 'LOP'){
      # LOP has no gate openings in GDACS, but logbook data has openings.
      GtDatProj <- GtDatProj |>
        # pivot_wider(names_from =  c(gate,param,data)) |>
        group_by(DateTime) |>
        filter(grepl('Flow',param) & grepl('Out|Spill|Gen',gate) & grepl('CWMS',data) |
              grepl('Flow',param) & grepl('RO|SWG',gate) & grepl('LogBook',data)) |>
        select(-param,data) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T),
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }
    if(p == 'DEX'){
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                SWG = sum(across(contains('SWG')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'FAL'){
      # Left off here!!!
      GtDatProj <- GtDatProj |>
        filter(grepl('Flow',param) & !grepl('Total|Spill',gate)) |>
        select(-param) |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate),id_cols = c('DateTime','data')) |>
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out,Gen = Gen,#Spill = Spill,
                RO = sum(across(contains('RO')),na.rm=T)) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }

    if(p == 'CGR'){
      # Need to estimate DT based on GDACS/CWMS Opening and CWMS Spill
      # DT was operated 2025-10-21 0705-1350
      GtDatProj <- GtDatProj |>
        filter(!is.na(value)) |>
        pivot_wider(names_from = c(gate,param,data),id_cols = c('DateTime')) |>
        mutate(DTGate_Opn = sum(if_else(DTGate1_Opn_CWMS <0,0,DTGate1_Opn_CWMS),
                                if_else(DTGate2_Opn_CWMS <0,0,DTGate2_Opn_CWMS),na.rm=T),
               DTGate_Flow = if_else(as.Date(DateTime) == as.POSIXct('2025-10-21') &
                                       DTGate_Opn>0 & (round(RO1_Opn_CWMS)<=0 & round(RO2_Opn_CWMS)<=0),
                                     Spill_Flow_CWMS,0)) |>
        dplyr::select(-contains('Opn')) |>
        rename_with(~ gsub("_LogBook", "",gsub("_CWMS", "", .x, fixed = TRUE))) |>
        mutate(RO_Flow = if_else(DTGate_Flow>0,0,RO_Flow)) |>
        #filter(DTGate_Flow>0) |>
        #filter(DateTime > as.POSIXct('2025-10-21')) |>
        #print(n = 100)
        arrange(DateTime) |>
        group_by(DateTime) |>
        reframe(Total = Out_Flow,Gen = Gen_Flow,#Spill = Spill,
                DT = DTGate_Flow,
                RO = RO_Flow) |>
        pivot_longer(cols = -DateTime,names_to = 'Gate_Type',values_to = 'Flow_cfs',values_drop_na = TRUE) |>
        distinct()
    }


    summary(GtDatProj)

    # GtDatProj |>
    #   filter(grepl('6',month(DateTime))) |>
    #   print(n = 300)

    GtMonAvgQ <- GtDatProj |>
      mutate(Month = factor(format(DateTime,'%b'),ordered = T,levels = month.abb)) |>
      group_by(Month,Gate_Type) |>
      reframe(Mean = round(mean(Flow_cfs,na.rm=T))) |>
      pivot_wider(names_from = Month,values_from = Mean)

    GtMonNmDys <- GtDatProj |>
      mutate(Date = as.Date(DateTime)) |>
      group_by(Date,Gate_Type) |>
      reframe(NmHrs = length(which(Flow_cfs>0))) |>
      mutate(Month = factor(format(Date,'%b'),ordered = T,levels = month.abb)) |>
      group_by(Month,Gate_Type) |>
      reframe(NmDys = length(which(NmHrs>0)),
              AvgDlyHrs = round(mean(NmHrs,na.rm=T))) |>
      pivot_longer(cols = -c(Month,Gate_Type)) |>
      pivot_wider(names_from = Month) |>
      filter(!grepl('Total',Gate_Type)) |>
      dplyr::rename(Usage = name)|>
      arrange(Usage)


    figTsByGtParams <-
      ggplot( GtDatProj,
             aes(x=DateTime,y = Flow_cfs,colour=Gate_Type,group = Gate_Type)) +
      geom_line(alpha = 0.6) +
      ylab('Flow, in cfs') +
      xlab('') +
      scale_x_datetime(breaks = seq.POSIXt(from = min(GtDatProj$DateTime),to = max(GtDatProj$DateTime),by = 'month'),
                   date_labels = '%b') +
      #scale_color_manual(values = clrs) +
      scale_color_brewer(palette = "Set2") +
      theme(strip.text.y.left = element_text(angle = 0),
            axis.text.x=element_text(size = 10,angle=45,hjust=1),
            axis.text=element_text(size=12),
            axis.title=element_text(size=12,face="bold"),
            strip.placement = "outside",
            strip.text.x = element_text(size = 12),
            strip.text.y = element_text(size = 10),
            strip.background = element_rect(fill=NA),
            legend.title = element_blank(),
            legend.position = 'top'
      ) +
      ggtitle(paste0('2025 ',p,' Gate-Specific Flow'))

    figTsByGtParams

    ggplot2::ggsave(plot = figTsByGtParams,
                    filename = file.path(WriteDir,'GateOpenings',paste0(p,'_',SaveName,'.png')),
                    device='png',width=9,height=4)
    write.csv(GtMonAvg,row.names = F,file.path(WriteDir,'GateOpenings',paste0(p,'_','Monthly_',SaveName,'.csv')))
    GtDatProjList[[p]] <- GtDatProj
    GtMonNmDysList[[p]] <- GtMonNmDys

>>>>>>> fa19c5f11a312acbfcca8860bc583de252d3b5e9
  }
}

<<<<<<< HEAD
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e
  GtDatProjList <- Filter(Negate(is.na), GtDatProjList)
  GtDatProjList <- Filter(Negate(is.null), GtDatProjList)
  GtDatByOutType <- purrr::reduce(GtDatProjList,full_join)

  GtMonNmDysList <- Filter(Negate(is.na), GtMonNmDysList)
  GtMonNmDysList <- Filter(Negate(is.null), GtMonNmDysList)
  GtMonNmDysSum <- purrr::reduce(GtMonNmDysList,full_join)

  svNm <- file.path(WriteDir,'GateOpenings',paste0('Hourly',SaveName,'Combined.csv'))
  write('# Total_Flow = total dam outflow, Spill = non-power outflow, SWG = spillway gate, RO = Regulating outlet, Fall creek fishhorns are open (>0) or closed (0), Fall Creek RO is gate opening in feet',svNm)
  write.csv(GtDatByOutType |>
              pivot_wider(id_cols = c(DateTime),names_from = c(name,Gate_Type,param)),
            row.names = F,svNm)

  svNm <- file.path(WriteDir,'GateOpenings',paste0('MonthlySummary',SaveName,'Combined.csv'))
  write('# Total_Flow = total dam outflow, Spill = non-power outflow, SWG = spillway gate, RO = Regulating outlet, Fall creek fishhorns are open (>0) or closed (0), Fall Creek RO is gate opening in feet',svNm)
  write.csv(GtMonNmDysSum,row.names = F,svNm)

}

<<<<<<< HEAD
=======
=======
>>>>>>> fa19c5f11a312acbfcca8860bc583de252d3b5e9
>>>>>>> 2e20159318bfd0e5ada541ad91190353af03d60e


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






# CleanCWMSnames <- function(x) {
#   # Clean up CWMS names into shorter names. Move this outside this function eventually.
#   colnames(x) <- gsub('.Inst|.0|.Best|.MIXED-REV|.MIXED_REV|.1Hour|.3Hours|.15Minutes|.CBT-REV|.Ave','',colnames(x))
#   colnames(x) <- gsub('.Temp-Water','_Temp',colnames(x))
#   colnames(x) <- gsub('.Flow','_Flow',colnames(x))
#   colnames(x) <- gsub('.Elev-Forebay','_ElevFB',colnames(x))
#   colnames(x) <- gsub('.D0,5','.D0.5',colnames(x))
#   colnames(x) <- gsub('_S1-','_',colnames(x))
#   colnames(x) <- gsub('.%-Saturation-TDG','_TDG',colnames(x))
#   return(x)
# }
