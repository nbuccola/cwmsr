#' Plot the monthly/annual metrics for each type of data
#'
#' @param SubBasinSites data.frame with Proj and subbasin
#' @param SaveName The name of the plot
#' @description Plot the monthly/annual metrics for each type of data
#' @return writes a series of plots based on monthly/annual summaries
#' @importFrom ggplot2
#' @export
#'
MultiYrQPlots <- function(SubBasinSites,SaveName){
  # Define custom breaks and labels
  qin_breaks <- c(10, 100, 500, 1000,2500,5000,10000)
  qin_labels <- c("10", "100", "500", "1.0k","2.5k","5.0k", "10k") # Custom labels (optional)

  sbnm <- unique(SubBasinSites$SubBasin)
  sbsites <- paste0(SubBasinSites$Proj,collapse='|')
  DatSums <- list(NA)

  ## FLOWS
  # InFlow
  QinSites <- paste0(names(cnfg)[!grepl('BCL|DEX|DOR|COT|FRN',names(cnfg))],collapse='|')
  QinSub <- OpsStats |>
    mutate(Month = factor(Month,levels = c(1:12,'Annual'),ordered=T),
           name = gsub('.15Minutes|.3Hours|.6Hours','',name),
           Year = as.numeric(Year)) |>
    dplyr::filter(grepl('Flow-In',name),Year>2020,Year<2026,
                  grepl(QinSites,name),grepl(sbsites,name)) |>
    mutate(name = factor(gsub('.Flow-In','',name),ordered=T,levels=names(cnfg)))

  # Define custom Month labels (skip every other label)
  custom_labels <- month.abb #unlist(sapply(month.abb,substr,0,1)) #
  names(custom_labels) <- as.character(1:12)
  custom_labels[names(custom_labels) %in% c(2,4,6,8,10,12)] <-''#c('2','4','5','7','8','10','11')] <- ""

  # Outflow
  QoutSites <- c('DET|BCL|GPR|FOS|CGR|BLU|HCR|LOP|DEX')
  QoutSub <- OpsStats |>
    dplyr::filter(grepl('Flow-Out|Flow-Spill',name),Year>2020,Year<2026,
                  grepl(QoutSites,name),
                  grepl(sbsites,name)) |>
    mutate(Month = factor(Month,levels = c(1:12,'Annual'),ordered=T),
           name = gsub('.15Minutes|.3Hours|.6Hours','',name),
           Year = as.numeric(Year)) |>
    separate_wider_delim(name,delim='-',names = c('name','FlowType')) |>
    mutate(name = factor(gsub('.Flow','',name),ordered=T,levels=names(cnfg)),
           FlowType = as.factor(gsub('Out','Total-Out',FlowType)))

  # Annual Outflow Plot
  qin_breaks <- c(10, 100,  1000,10000)
  qin_labels <- c("10", "100",  "1.0k", "10k") # Custom labels (optional)
  # Define custom Month labels (skip every other label)
  custom_labels <- month.abb #unlist(sapply(month.abb,substr,0,1)) #
  names(custom_labels) <- as.character(1:12)
  custom_labels[names(custom_labels) %in% c(2,3,5,6,8,9,11,12)] <-''#c('2','4','5','7','8','10','11')] <- ""
  AllMonthLabs <- month.abb #unlist(sapply(month.abb,substr,0,1)) #
  names(AllMonthLabs) <- as.character(1:12)


  DatSums[['QSub']] <- QinSub |>
    mutate(FlowType = "Inflow") |>
    full_join(QoutSub)

  # Monthly Inflow/Outflow line plot
  MonQLns <-
    ggplot(DatSums[['QSub']] |>
             filter(!grepl('Ann',Month)) |>
             mutate(Year = as.factor(Year)),
           aes(y=Mean,x=Month,colour=Year,group = interaction(Year,FlowType),fill = Year)) +#,linetype = FlowType #label = round(Mean),fill = Mean,
    geom_line() + #alpha=0.7,linewidth=1.2) +
    scale_y_log10(breaks = qin_breaks, labels = qin_labels) +
    facet_grid(FlowType~name,scales = 'free') +
    scale_colour_manual(values = clrs) +
    scale_x_discrete(labels = AllMonthLabs) +
    ylab('Mean Monthly Flow [cfs]') +
    xlab('Month') +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.x=element_text(size = 9,angle=45,hjust=1),
          axis.text=element_text(size=12),
          strip.placement = "outside",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill=NA),
          legend.position = 'top'
    )
  MonQLns
  ggsave(plot=MonQLns,device = 'png',width=7,height=5,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'QLines.png',sep = '_')))

  # Annual Flow Plot
  qin_breaks <- c(10, 100, 500, 1000,2500,5000,10000)
  qin_labels <- c("10", "100", "500", "1.0k","2.5k","5.0k", "10k") # Custom labels (optional)

  AnnQPnts <-
    ggplot(DatSums[['QSub']] |>
             pivot_longer(cols = c('Q05','Q50','Q95'),names_to = 'Stat') |>
             mutate(Year = as.factor(Year),Stat = as.factor(Stat)) |>
             filter(Month == 'Annual'),
           aes(y = value,x=Year,colour = Year,shape = Stat,group = Year)) +
    geom_point(alpha=0.8) +
    facet_grid(~name+FlowType,scales='fixed')  + #,ncol = 4
    scale_y_log10(breaks = qin_breaks, labels = qin_labels) +
    scale_colour_manual(values = clrs) +
    ylab('Ann. %Non-Exc Flow [cfs]') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='top',
          axis.text.x=element_text(angle=45,hjust=1))
  AnnQPnts
  ggsave(plot=AnnQPnts,device = 'png',width=7,height=3,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'QAnnPnts.png',sep = '_')))

  # Plot daily averages for WSELV
  # WSELV data
  WsSub <- OpsStats |>
    mutate(Month = factor(Month,levels = c(1:12,'Annual'),ordered=T),
           name = gsub('.15Minutes|.3Hours|.6Hours','',name),
           Year = as.numeric(Year)) |>
    dplyr::filter(grepl('Elev',name),Year>2020,Year<2026,
                  grepl(QinSites,name),
                  grepl(sbsites,name)) |>
    mutate(name = gsub('.Elev|.0','',name)) |>
    separate_wider_delim(name,delim='-',names = c('name','param')) |>
    mutate(name = factor(name,ordered=T,levels=SubBasinSites$Proj),
           param = as.factor(param)) |>
    filter(Month == '6',!grepl('Rule',param))

  outs <- unlist(qgtConfig[['CenterElvs']])
  excEl <- unlist(qgtConfig[['ExcElev']])
  excEl.df <- data.frame(name = names(excEl), CritElevName = excEl,CritElev= NA) |>
    filter(grepl(sbsites,name))
  for(i in 1:nrow(excEl.df)){
    if(grepl(sbsites,excEl.df$name[i])){
      excEl.df$CritElev[i] <- outs[grepl(excEl.df$name[i],names(outs)) & grepl(excEl.df$CritElevName[i],names(outs))]
    }
  }
  rm(excEl,outs)
  excEl.df <- excEl.df |>
    mutate(
      CritElevName = gsub('WTC1','WTCT',CritElevName),
      Year = as.character(min(WsSub$Year)),
      name = factor(name,ordered=T,levels=SubBasinSites$Proj))

  # DatSums[['WsSub']] <- WsSub
  #
  # # June WSELV Max Plot
  # AnnWStab <-
  #   ggplot(WsSub |>
  #            mutate(Year = as.factor(Year)),
  #          aes(y = Max,x=Year,colour = Year,group = Year)) +
  #   geom_point(alpha=0.8) +
  #   facet_wrap(~name,scales='free') +
  #   scale_colour_manual(values = clrs) +
  #   ylab('June Max Water Surface Elevation [ft] by Year') +
  #   geom_hline(data = excEl.df,aes(yintercept = CritElev)) +
  #   geom_text(data = excEl.df,aes(y = CritElev,x = Year,label = CritElevName),vjust = -0.5,hjust =0.1) +
  #   theme(axis.title.x=element_blank(),
  #         strip.text.y = element_text(angle = 0),
  #         legend.position ='none',
  #         axis.text.x=element_text(angle=45,hjust=1))
  # AnnWStab
  # ggsave(plot=AnnWStab,device = 'png',width=7,height=5,
  #        filename = file.path(RdataDir,paste0(SaveName,'WSELVJuneMaxDots.png')))

  # Prep figs for days above water control structure
  WsExcSub <- OpsExc |>
    mutate(Month = factor(gsub("^0+", "", Month),levels = c(1:12,'Annual'),ordered=T),
           name = gsub('.15Minutes|.3Hours|.6Hours','',name),
           Year = as.numeric(as.character(Year))) |>
    dplyr::filter(grepl('WTempCon',ExcType),Year>2020,Year<2026) |>
  #   mutate(name = as.factor(name)) |>
  #   summary()
  # filter(grepl('LOP',name))
    filter(grepl(sbsites,name)) |>
    mutate(name = gsub('-Forebay|.Elev|.0','',name)) |>
    mutate(name = factor(name,ordered=T,levels=SubBasinSites$Proj)) |>
    rename(param = ExcType) |>
    select(Year,Month,name,param,Exc) |>
    full_join(WsSub |>
                select(Year,Month,name,Max) |>
                rename(Exc = Max) |>
                mutate(param = 'JuneMaxWS')|>
                mutate(Month = 'Annual')) |>
    mutate(Year = as.factor(Year))
  # Create a named character vector for the new labels
  siteNms <- SubBasinSites$Proj; names(siteNms) <- siteNms
  WSelv_names <- c(
    "DysWTempCon" = "Days With Temp Control",
    "JuneMaxWS" = "June Max WS Elev [ft]",
    siteNms
  )

  excEl.df <- excEl.df |>
    mutate(param = 'JuneMaxWS') |>
    bind_rows(excEl.df |>
                mutate(param = 'DysWTempCon',
                       CritElev= NA) ) |>
    mutate(name = factor(name,ordered=T,levels=SubBasinSites$Proj))

  if(length(tsites[grepl(sbsites,names(tsites))])>1){
    ht <-5; wd <-7
  }else{
    ht <-3; wd <-3
  }
  # Annual WSELV Max Table
  AnnWSExctab <-
    ggplot(WsExcSub |>
             filter(grepl('Ann',Month)) |>
             mutate(Year = as.factor(Year)),
           aes(y = Exc,x=Year,colour = Year,group = Year,label=round(Exc))) +
    #geom_point(alpha=0.8) +
    geom_text() +
    geom_hline(data = excEl.df,aes(yintercept = CritElev)) +
    geom_text(data = excEl.df,aes(y = CritElev,x = Year,label = CritElevName),vjust = -0.5,hjust =0.1) +
    facet_wrap(name~param,labeller = as_labeller(WSelv_names),scales='free') +
    scale_colour_manual(values = clrs) +
    ylab('') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1))
  AnnWSExctab
  ggsave(plot=AnnWSExctab,device = 'png',width=7,height=ht,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'DysWSAbvTempConTab.png',sep = '_')))

  ###########
  # Prep figs for Temperature
  TSub <- OpsStats |>
    dplyr::filter(grepl('Temp',name),Year>2020,Year<2026,
                  !Month %in% c('1','2','3'),
                  grepl(paste0(tsites[grepl(sbsites,names(tsites))],collapse='|'),name)) |>
    mutate(Month = factor(Month,levels = c(1:12,'Annual'),ordered=T),
           name = gsub('.0|.1Day|.COORD_Fisheries_|.USACE_Temporary_|.NMFS_USFWS_ODFW_| ','',
                       gsub('.Fisheries ','.',name)),
           name = gsub('.Temp-Water','',if_else(grepl('Targ',name),name,paste0(name,'.Observed'))),
           Year = as.numeric(Year)) |>
    separate_wider_delim(name,delim='.',names = c('name','param')) |>
    mutate(name = factor(gsub('.Observed|.Targ','',name),ordered=T,levels=gsub('.Temp-Water','',tsites)),
           param = as.factor(param))
  unique(OpsExc$ExcType)

  DatSums[['Tmon']] <-  TSub

  # Monthly Temperature line plot
  MonTLns <-
    ggplot(TSub |>
             filter(!grepl('Ann',Month),!grepl('Targ',param)) |>
             mutate(Year = as.factor(Year)),
           aes(y=Mean,x=Month,colour=Year,group = Year,fill = Year)) +
    geom_line() +
    facet_wrap(~name,scales = 'free') +
    geom_step(data = TSub |>
                filter(!grepl('Ann',Month),grepl('Targ',param)) |>
                mutate(Year = as.factor(Year)) |>
                filter(!is.na(Mean)),
              aes(group=param,colour = "Target"),linewidth=1.1) +
    scale_colour_manual(values = c(clrs,Target = 'grey')) +
    scale_x_discrete(labels =   AllMonthLabs) +
    scale_y_continuous('Mean Monthly Temperature [\u00B0C]',
      sec.axis = sec_axis(~c2f(.), name = "Mean Monthly Temperature [\u00B0F]")
    ) +
    xlab('Month') +
    theme(strip.text.y.left = element_text(angle = 0),
          axis.text.x=element_text(size = 9,angle=45,hjust=1),
          axis.text=element_text(size=12),
          strip.placement = "outside",
          strip.text.x = element_text(size = 12),
          strip.text.y = element_text(size = 10),
          strip.background = element_rect(fill=NA),
          legend.position = 'right'
    )
  MonTLns
  ggsave(plot=MonTLns,device = 'png',width=7,height=3,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'TLines.png',sep = '_')))

  TexcSub <- OpsExc |>
    mutate(
      Month = factor(gsub("^0+", "", Month),levels = c(1:12,'Annual'),ordered=T),
      name = gsub('.15Minutes|.3Hours|.6Hours','',name),
      Year = as.numeric(as.character(Year))) |>
    dplyr::filter(grepl('7dADM',ExcType),
                  !grepl('Targ',name),
                  Year>2020,Year<2026,
                  grepl(paste0(tsites[grepl(sbsites,names(tsites))],collapse='|'),name)
                  ) |>
    mutate(Month = factor(Month,levels = c(1:12,'Annual'),ordered=T),
           name = factor(gsub('.Temp-Water','',name),ordered=T,levels=gsub('.Temp-Water','',tsites)))
  unique(TexcSub$ExcType)

  DatSums[['Texc']] <-  TexcSub

  # Annual Excedence of values
  AnnTexctab <-
    ggplot( TexcSub |>
             filter(grepl('Ann',Month),!grepl('Targ',ExcType),!is.na(Exc)) |>
             mutate(Year = as.factor(Year),
                    TValsNum = factor(TValsNum,levels = unique(TValsNum))),
           aes(y = TValsNum,x=Year,label = Exc,colour = Exc,fill = Exc,group = Year)) + #shape = TValsNum,
    geom_tile(alpha=0.8) +
    geom_text(color='black',size=3) +
    facet_grid(~name) +
    ylab(paste(unique(TexcSub$name),'Ann. Days Above Temp [\u00B0C]')) +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1)) #+
    AnnTexctab
  ggsave(plot=AnnTexctab,device = 'png',width=wd,height=3,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'AnnTexcTab.png',sep = '_')))


  # Monthly Excedence of 7dADM
  MonT7dADMexctab <-
    ggplot( TexcSub |>
              filter(
                !grepl('Ann',Month),
                grepl('Max|Min|Btwn',ExcType),
                !is.na(Exc)) |>
              mutate(Year = as.factor(Year),
                     Exc = if_else(Exc==0,NA,Exc),
                     ExcType = gsub('Dys7dADMBtwnTargs','Within',gsub('Dys7dADMExcMaxTarg','Above',gsub('Dys7dADMBlwMinTarg','Below',ExcType))),
                     ExcType = factor(ExcType,levels = c('Above','Within','Below'),ordered=T)),
            aes(y = Exc,x=Month,colour = ExcType,shape = ExcType,fill = ExcType,group = Year)) +
    geom_point(alpha=0.8) +
    scale_shape_manual(name = '',values = c(24,0,25)) +
    facet_grid(Year~name,scales='free') +
    scale_x_discrete(labels = AllMonthLabs) +
    scale_color_manual(name = '',values = c("Below" = "Cornflower Blue","Within"='black', "Above" = "orange")) +
    scale_fill_manual(name = '',values = c("Below" = "Cornflower Blue", "Within"=NA,"Above" = "orange")) +
    ylab(paste('Days per Month Above,Within,Below Target')) +
    theme(strip.text.y = element_text(angle = 90),
          legend.position ='top',
          theme(legend.title = element_blank()),
          axis.text.x=element_text(angle=45,hjust=1)) #+
    #ggtitle('Days per Month 7dADM is Above or Below Temperature Target')
  MonT7dADMexctab
  ggsave(plot=MonT7dADMexctab,device = 'png',width=wd,height=5,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'MonTTargAbvBlwArrows.png',sep = '_')))

  # Annual Excedence of values
  # Why not a big exceedence at CGR in 2025
  AnnT7dADMexc <-
    ggplot(TexcSub |>
             filter(
               grepl('Ann',Month),
               grepl('Max|Min|Btwn',ExcType)) |>
             mutate(Year = as.factor(Year),
                    ExcType = gsub('Dys7dADMBtwnTargs','Within',gsub('Dys7dADMExcMaxTarg','Above',gsub('Dys7dADMBlwMinTarg','Below',ExcType))),
                    ExcType = factor(ExcType,levels = c('Above','Within','Below'),ordered=T)),
           aes(y = Exc,x=Year,colour = ExcType,shape = ExcType,fill = ExcType,group = Year)) +
    geom_point(alpha=0.8) +
    facet_grid(~name,scales='free') +
    scale_shape_manual(name = '',values = c(24,0,25)) +
    scale_color_manual(name = '',values = c("Below" = "Cornflower Blue","Within"='black', "Above" = "orange")) +
    scale_fill_manual(name = '',values = c("Below" = "Cornflower Blue", "Within"=NA,"Above" = "orange")) +
    ylab('Ann Days Above,Within,Below Target') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='top',
          theme(legend.title = element_blank()),
          axis.text.x=element_text(angle=45,hjust=1)) #+
  AnnT7dADMexc
  ggsave(plot=AnnT7dADMexc,device = 'png',width=wd,height=3,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'AnnTTargAbvBlwArrows.png',sep = '_')))

  if(any(grepl(sbnm,c('NSant','SSant')))){
    # Steelhead egg emergence
    # # Need to remove FALO and add SSFO for Steelhead
    DatSums[['StlhdEggEmrg']] <-
      OpsEggEmrg |>
      filter(grepl('Steelhead',species),!grepl('FALO',name),
             grepl(paste0(tsites[grepl(sbsites,names(tsites))],collapse='|'),name))|>
      mutate(spawnDay = as.factor(format(as.Date(spawnDay,origin = as.Date('2000-01-01')),'%m/%d')),
             name = factor(gsub('.Temp-Water','',name),ordered=T,levels=gsub('.Temp-Water','',tsites))) |>
      select(-nmiss,-species)

    AnnStlEmerge <-
      ggplot(DatSums[['StlhdEggEmrg']] |>
               dplyr::filter(Year>2020,Year<2026),
             aes(fill = atu.d,label = atu,x=Year,y = spawnDay)) +
      geom_tile(alpha=0.8) +
      geom_text(color='black',size=3) +
      ylab('Spawn Day') +
      theme(axis.title.x=element_blank(),
            strip.text.y = element_text(angle = 0),
            legend.position ='none',
            axis.text.x=element_text(angle=45,hjust=1)) #+
    AnnStlEmerge
    ggsave(plot=AnnStlEmerge,device = 'png',width=3.5,height=1.5,
           filename = file.path(RdataDir,paste(SaveName,sbnm,'StlEmergeTab.png',sep = '_')))

    DatSums[['StlhdEggEmrg']] <-
      DatSums[['StlhdEggEmrg']] |>
      mutate(Year = as.factor(Year)) |>
      pivot_wider(names_from = spawnDay,id_cols = c(name,Year),values_from = c('atu'))

  }else{
    DatSums[['StlhdEggEmrg']] <- OpsEggEmrg |>
      filter(grepl('Steelhead',species),
             grepl(paste0(tsites[grepl(sbsites,names(tsites))],collapse='|'),name))
  }

  DatSums[['ChnkEggEmrg']] <-
    OpsEggEmrg |>
    filter(grepl('Chinook',species),
           grepl(paste0(tsites[grepl(sbsites,names(tsites))],collapse='|'),name))|>
    mutate(spawnDay = as.factor(format(as.Date(spawnDay,origin = as.Date('2000-01-01')),'%m/%d')),
           name = factor(gsub('.Temp-Water','',name),ordered=T,levels=gsub('.Temp-Water','',tsites))) |>
    select(-nmiss,-species)

  # Estimated Emergence
  AnnChnEmerge <-
    ggplot(DatSums[['ChnkEggEmrg']] |>
             filter(Year>2020,Year<2026),
           aes(fill = atu.d,label = atu,x=Year,y = spawnDay)) +
    geom_tile(alpha=0.8) +
    geom_text(color='black',size=3) +
    facet_grid(name~.,scales='free') +
    ylab('Spawn Day') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1)) #+
  AnnChnEmerge
  ggsave(plot=AnnChnEmerge,device = 'png',width=wd,height=1.5,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'ChEmergeTab.png',sep = '_')))

  DatSums[['ChnkEggEmrg']] <- DatSums[['ChnkEggEmrg']] |>
    mutate(Year = as.factor(Year)) |>
    pivot_wider(names_from = spawnDay,id_cols = c(name,Year),values_from = c('atu'))

  # TDG Data
  # Where is GPRO, SSFO?
  TDGexcSub <- OpsExc |>
    mutate(
      Month = factor(gsub("^0+", "", Month),levels = c(1:12,'Annual'),ordered=T),
      name = gsub('.15Minutes|.3Hours|.6Hours|-Channel-StmBed','',name),
      Year = as.numeric(as.character(Year))) |>
    dplyr::filter(grepl('TDG',name),
                  Year>2020,Year<2026,
                  #grepl('GPR|SSFO',name)) |>
                  grepl(paste0(TDGsites[grepl(sbsites,names(TDGsites))],collapse='|'),name)) |>
    mutate(Month = factor(Month,levels = c(1:12,'Annual'),ordered=T),
           name = factor(gsub('.%-Saturation-TDG','',name)))

  # OpsExc |>filter(grepl('SSFO',name)) |>mutate(name = as.factor(name)) |>summary() #,grepl('Sat',name)
  # TDGexcSub |> summary()
  # unique(OpsExc$name)
  # TDG!!!
  # Annual Excedence of values
  AnnTDGexctab <-
    ggplot( TDGexcSub |>
              filter(grepl('Ann',Month),!is.na(Exc)) |>
              mutate(Year = as.factor(Year),
                     TValsNum = factor(TValsNum,levels = unique(TValsNum))),
            aes(y = Exc,x=Year,colour = Year,group = Year)) + #shape = TValsNum,
    geom_point(alpha=0.8) +
    facet_grid(TValsNum~name,scales='free') +
    scale_colour_manual(values = clrs) +
    ylab('Ann. Days Above TDG [% Sat]') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='none',
          axis.text.x=element_text(angle=45,hjust=1)) #+
  AnnTDGexctab
  ggsave(plot=AnnTDGexctab,device = 'png',width=3,height=3,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'AnnTDGexcPoints.png',sep = '_')))

  # TDG!!!
  # Monthly Excedences
  MonTDGexctab <-
    ggplot( TDGexcSub |>
              filter(!grepl('Ann',Month),!is.na(Exc)) |>
              mutate(Year = as.factor(Year),
                     TValsNum = factor(TValsNum,levels = unique(TValsNum))),
            aes(y = Exc,x=Month,colour = Year,group = Year)) +
    geom_point(alpha=0.8) +
    facet_grid(TValsNum~name,scales='free') +
    scale_x_discrete(labels = AllMonthLabs) +
    scale_colour_manual(values = clrs) +
    ylab('Days/Month Above TDG [% Sat]') +
    theme(axis.title.x=element_blank(),
          strip.text.y = element_text(angle = 0),
          legend.position ='right',
          axis.text.x=element_text(angle=45,hjust=1)) #+
  MonTDGexctab
  ggsave(plot=MonTDGexctab,device = 'png',width=7,height=3,
         filename = file.path(RdataDir,paste(SaveName,sbnm,'MonTDGexcPoints.png',sep = '_')))

  # DatSums[['OpsChnkEggEmrg']]
  # DatSums[['OpsStlhdEggEmrg']]
   DatSums[['TDGexc']] <-  TDGexcSub
  # Gate Openings
  return(DatSums)

}

