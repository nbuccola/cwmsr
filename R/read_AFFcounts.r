
calcAFFAnnMetrics <- function(){
  print("Calculating AFF Count Metrics")
  xl <- xwd |>
    select(-contains('_S1'),-contains('Flow-In'),-contains('-StmBed')) |>
    # Merge in AFF counts
    inner_join(AFF) |>
    mutate(JDAY = as.numeric(strftime(Date,'%j')),
           Year = as.factor(strftime(Date,'%Y')))|>
    pivot_longer(cols = -c(Date,JDAY,Year)) |>
    filter(!grepl('SFCO|VIDO',name))  |>
    mutate(
      Data = factor(if_else(grepl('Elev-',name),'Elev',
                            if_else(grepl('Flow-',name),'Flow',
                                    if_else(grepl('Temp-',name),'Temp',
                                            if_else(grepl('TDG',name),'TDG',
                                                    if_else(grepl('CH|ST',name),'Returns',NA))))),
                    levels=c('Elev','Flow','Temp','Returns'),
                    ordered=T),
      name = gsub('-Forebay|-Water','',name),
      MoDy = strftime(Date,'%m-%d'))

  #dayExc <- function(Q){which(value>Q)[1]}

  xAnnAFFSumCH <- xl  |>
    filter(grepl('CH',name)) |>
    group_by(Year) |>
    reframe(Max = max(value[!is.na(value)]),
            DMax = JDAY[which(value[!is.na(value)]>=Max)[1]],
            Q50 = floor(Max/2),#quantile(value[!is.na(value)|value!=0],0.5),
            D50 = JDAY[which(value[!is.na(value)|value!=0]>Q50)[1]]
    )

  xAnnAFFSum <- xl  |>
    full_join(xAnnAFFSumCH) |>
    group_by(Year,name) |>
    reframe(M50 = mean(value[which(JDAY<=D50)],na.rm=T),
            #M75 = mean(value[which(JDAY<=D75& JDAY>=D50)],na.rm=T)#,
            MMax = mean(value[which(JDAY<=DMax & JDAY>=D50)],na.rm=T)
    ) |>
    full_join(xAnnAFFSumCH |>
                mutate(name = 'Total_CH')) |>
    filter(Year!=2021)

  xaxticks <- seq.Date(from = as.Date(paste0(min(as.character(xl$Year)),'-01-01')),
                       to = as.Date(paste0(min(as.character(xl$Year)),'-12-31')),by = 'month')

  custom_labels <- c("CGR.Elev" = "Lake\nLevel", "CGRO.Temp"="CGRO\nTemp(F)" ,
                     'CGR.Flow-Gen' = 'CGR Flow\nGen (cfs)','CGR.Flow-Spill' = 'CGR Flow\nSpill (cfs)',
                     'Cum_CH' = "Chinook\nCumulative\nTotal",'Cum_ST'='Steelhead\nTotal')

  # Plot daily averages with each year: WSELV, Flow, Fish
  figUpsOpsFishByYr <-
    ggplot(xl |>
             dplyr::filter(grepl('CGR.Flow-Gen|CGR.Flow-Spill',name) | grepl('Temp|CH',name),
                           !grepl('Mar|Apr',format(Date,'%b')),
                           Year!=2021),
           aes(x=JDAY,y = value,colour=Year,group = Year)) +
    geom_path(alpha = 0.6,linewidth=1.1) + #
    facet_grid(name~.,scales = 'free', switch = "y",
               labeller = as_labeller(custom_labels)) +
    geom_vline(aes(linetype = Quant,xintercept = JDAY,colour=Year),
               data = xAnnAFFSum |>
                 filter(grepl('CH',name)) |>
                 select(Year,D50) |> #D50,D75,
                 pivot_longer(-Year) |>
                 rename(Quant = name,JDAY=value)) +
    ylab('') +
    xlab('') +
    scale_x_continuous(breaks = as.numeric(strftime(xaxticks,'%j')),
                       labels = strftime(xaxticks,'%b')) +
    theme(#strip.text.y.left = element_text(angle = 45),
      axis.text.x=element_text(size = 10,angle=45,hjust=1),
      axis.text=element_text(size=12),
      axis.title=element_text(size=12,face="bold"),
      strip.placement = "left",
      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 9),
      strip.background = element_rect(fill=NA),
      legend.title = element_blank(),
      legend.position = 'top'
    ) +
    scale_color_manual(values = clrs) +
    ggtitle(paste0(Proj,' Operations and AFF Counts 2023-2025'))
  figUpsOpsFishByYr
  ggplot2::ggsave(plot = figUpsOpsFishByYr,
                  filename = file.path(LocalWd,paste0(Proj,'_USOps','.png')),
                  device='png',width=7,height=4)

  write.csv(xAnnAFFSum |>
              filter(grepl('CGR.Flow-Out',name) | grepl('Temp|Total_CH|Total_ST',name)) |>
              arrange(name) |>
              mutate(DMax = format(as.Date(DMax,origin = '2024-12-31'),'%m-%d'),
                     D50 = format(as.Date(D50,origin = '2024-12-31'),'%m-%d')),
            file=file.path(LocalWd,paste0(Proj,'_USOps','.csv')))

  # Plot daily average with each year: WSELV, Temp
  custom_labels <- c("CGR.Elev-Forebay" = "Lake\nLevel", "CGRO.Temp-Water"="CGRO\nTemp(F)")
  figTempOpsByYr <-
    ggplot(xwd |>
             select(-contains('_S1'),-contains('Flow-In'),-contains('-StmBed')) |>
             mutate(JDAY = as.numeric(strftime(Date,'%j')),
                    Year = as.factor(strftime(Date,'%Y')))|>
             pivot_longer(cols = -c(Date,JDAY,Year))  |>
             dplyr::filter(grepl('CGRO.Temp|Elev',name),
                           !grepl('Jan|Feb|Mar',format(Date,'%b')),
                           Year!=2021,Year!=2022),
           aes(x=JDAY,y = value,colour=Year,group = Year)) +
    geom_path(alpha = 0.6,linewidth=1) +
    facet_grid(name~.,scales = 'free', switch = "y",
               labeller = as_labeller(custom_labels)) +
    ylab('') +
    xlab('') +
    scale_x_continuous(breaks = as.numeric(strftime(xaxticks,'%j')),
                       labels = strftime(xaxticks,'%b')) +
    theme(#strip.text.y.left = element_text(angle = 45),
      axis.text.x=element_text(size = 10,angle=45,hjust=1),
      axis.text=element_text(size=12),
      axis.title=element_text(size=12,face="bold"),
      strip.placement = "left",
      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 9),
      strip.background = element_rect(fill=NA),
      legend.title = element_blank(),
      legend.position = 'top'
    ) +
    scale_color_manual(values = clrs) +
    ggtitle(paste0(Proj,' Operations and Temperature 2023-2025'))
  figTempOpsByYr
  ggplot2::ggsave(plot = figTempOpsByYr,
                  filename = file.path(LocalWd,paste0(Proj,'_TempOps','.png')),
                  device='png',width=6,height=3)



}



SumEmerge<-function(){
  # Calc emergence through each year and site with a function
  xl <- xwd |>
    mutate(JDAY = as.numeric(strftime(Date,'%j')),
           Year = as.factor(strftime(Date,'%Y')))|>
    pivot_longer(cols = -c(Date,JDAY,Year)) |>
    filter(grepl('CGRO.Temp',name))
  Atud <- xl %>%
    mutate(Year = as.character(Year)) |>
    filter(grepl('2023|2024',Year),!is.na(value)) |>
    select(JDAY,value,Year) %>%
    complete(Year) %>%
    expand_grid(SpawnDay = spawndays) %>%
    group_by(Year, SpawnDay) %>%
    reframe(atu.d = calcEmergenceTiming(tout = data.frame(JDAY = JDAY,T_F = value),
                                        atu.day = unique(SpawnDay))$atu.d[[1]],
            atu = format(as.Date(atu.d,origin = as.Date('2000-01-01')),'%m/%d')) %>%
    mutate(SpawnDay = as.factor(format(as.Date(as.numeric(SpawnDay),origin = as.Date('2000-01-01')),'%m/%d'))) %>%
    ungroup()

  # Average over all Spawn Days
  TdmSpawnAvg <- Atud %>%
    group_by(Year) %>%
    dplyr::summarize(atu.d = mean(atu.d,na.rm=T),
                     atu = format(as.Date(atu.d,origin = as.Date('2000-01-01')),'%m/%d')) %>%
    mutate(SpawnDay = 'Average') %>%
    full_join(Atud)
  # Average over all Years
  TdmYrAvg <- Atud %>%
    group_by(SpawnDay) %>%
    dplyr::summarize(atu.d = mean(atu.d,na.rm=T),
                     atu = format(as.Date(atu.d,origin = as.Date('2000-01-01')),'%m/%d')) %>%
    mutate(Year = 'Average') %>%
    full_join(TdmSpawnAvg)
  EmDyYrAvg <- TdmSpawnAvg %>%
    mutate(Year = factor(Year,levels=c('2023','2024'),ordered=T))


  write.csv(EmDyYrAvg |>
              select(-atu.d) |>
              pivot_wider(names_from = SpawnDay,values_from = atu),
            file=file.path(LocalWd,paste0(Proj,'_Emergence','.csv')))

  # # Table with the average estimated emergence differences from NAA (from 3 different spawn days)
  # EmrgncDifSDAvgTbl <- ggplot(  EmDyYrAvg ,
  #                             aes(x=Year, y=SpawnDay, fill=atu.d,label=atu)) +
  #   geom_tile(alpha=0.8) +
  #   geom_text(color='black',size=4) +
  #   #facet_grid(.~Alt,scales = 'free') +
  #   scale_y_discrete(limits=rev) +
  #   #scale_fill_gradient2(low = "orange",mid = "grey",  high = "darkblue",midpoint = 0) +
  #   ggtitle(paste0('Difference from NAA in Estimated Emergence Timing (Days)\nAverage of 3 Spawn Days: ',toString(spawndaysMD))) +
  #   theme(axis.title.x=element_blank(),
  #         strip.text.y = element_text(angle = 0),
  #         legend.position ='none',
  #         axis.text.x=element_text(angle=45,hjust=1))
  # EmrgncDifSDAvgTbl
  # ggsave(plot=EmrgncDifSDAvgTbl,device = 'png',width=10,height=5,
  #        filename=file.path(compDir,gsub('Comp','EmrgncDifTbl',pltNm)))
  #

  return(TdmYrAvg)
}



calcDSPassAnnMetrics <- function(){
  print("Calculating Downstream Passage Metrics")
  for(i in 2:nrow(RST)){
    if(is.na(RST$Date[i])){
      RST$Date[i] <- RST$Date[i-1] + 7*(RST$Week[i] - RST$Week[i-1])
    }
  }

  RST <- RST  |>
    select(-starts_with('CumCount'),-Week,-Year) |>
    mutate(Catch_RO = as.numeric(Catch_RO),
           Catch_PH = as.numeric(Catch_PH)) |>
    drop_na() |>
    mutate(JDAY = as.numeric(strftime(Date,'%j')),
           Year = as.factor(strftime(Date,'%Y')),
           Season = factor(if_else(JDAY>274 | JDAY<32,'Fall','Spring'),levels=c('Spring','Fall'),ordered=T)) |>
    arrange(Date) |>
    group_by(Year,Season) |>
    reframe(Date = Date,
            #Catch_RO = Catch_RO,
            #Catch_PH = Catch_PH,
            Cum_RO = cumsum(Catch_RO),
            Cum_PH = cumsum(Catch_PH))

  xl <- xwd |>
    select(-contains('_S1'),-contains('Flow-In'),-contains('-StmBed')) |>
    # Merge in RST counts
    full_join(RST |> select(-Season)) |>
    mutate(JDAY = as.numeric(strftime(Date,'%j')),
           Year = as.factor(strftime(Date,'%Y')),
           Season = factor(if_else(JDAY>274,'Fall','Spring'),levels=c('Spring','Fall'),ordered=T))|>
    pivot_longer(cols = -c(Date,JDAY,Year,Season)) |>
    mutate(
      Data = factor(if_else(grepl('Elev-',name),'Elev',
                            if_else(grepl('Flow-',name),'Flow',
                                    if_else(grepl('Temp-',name),'Temp',
                                            if_else(grepl('TDG',name),'TDG',
                                                    if_else(grepl('Cum',name),'RST',NA))))),
                    levels=c('Elev','Flow','Temp','RST'),
                    ordered=T),
      name = gsub('-Forebay|-Water','',name),
      MoDy = strftime(Date,'%m-%d')) |>
    filter(Year!=2021,Year!=2022)

  xAnnRSTSum <- xl  |>
    filter(grepl('Cum',name)) |>
    drop_na() |>
    filter(grepl('RO',name)) |>
    group_by(Year,Season) |>
    reframe(Max = max(value[!is.na(value)]),
            DMax = JDAY[which(value[!is.na(value)]>=Max)[1]],
            Q50 = floor(Max/2),#quantile(value[!is.na(value)|value!=0],0.5),
            D50 = JDAY[which(value[!is.na(value)|value!=0]>Q50)[1]])
  #mutate(name = gsub('Cum_','',name))

  xAnnRSTSumAll <- xl  |>
    full_join(xAnnRSTSum) |>
    group_by(Year,Season,name) |>
    reframe(M50 = mean(value[which(JDAY<=D50)],na.rm=T),
            MMax = mean(value[which(JDAY<=DMax & JDAY>=D50)],na.rm=T)) |>
    full_join(xAnnRSTSum |>
                mutate(name = 'RST_RO'))

  xaxticks <- seq.Date(from = as.Date(paste0(min(as.character(xl$Year)),'-01-01')),
                       to = as.Date(paste0(min(as.character(xl$Year)),'-12-31')),by = 'month')

  custom_labels <- c("CGR.Elev" = "Lake\nLevel",
                     'CGR.Flow-Gen' = 'PH\nFlow',
                     'CGR.Flow-Spill' = 'RO\nFlow',
                     'Cum_RO' = "RST\nCount\nRO",'Cum_PH'="RST\nCount\nPH")
  # Plot daily averages with each year: WSELV, Flow, Fish
  figDwnStrmOpsFishByYrSpring <-
    ggplot(xl |>
             drop_na() |>
             mutate(name = as.factor(name)) |>
             dplyr::filter(grepl('Gen|Spill|Elev|Cum',name),
                           Year!=2021,Year!=2022,
                           JDAY<213),
           aes(x=JDAY,y = value,colour=Year,group = Year)) +
    geom_path(alpha = 0.6,linewidth=1.3) +
    geom_point(alpha = 0.6,linewidth=1.3,
               data = xl |>
                 dplyr::filter(grepl('Cum',name),
                               Year!=2021,Year!=2022,
                               JDAY<213),
               aes(x = JDAY,y=value,colour = Year),shape = 1) +
    facet_grid(name~.,scales = 'free', switch = "y",
               labeller = as_labeller(custom_labels)) +
    geom_vline(aes(linetype = Season,xintercept = JDAY,colour=Year),
               data = xAnnRSTSumAll |>
                 filter(grepl('RST_RO',name),grepl('Spring',Season)) |> #
                 mutate(Season = gsub('Spring','Spring_D50',
                                      gsub('Fall','Fall_D50',Season))) |>
                 select(Year,Season,D50,name) |>
                 rename(Quant = name,JDAY=D50)) +
    ylab('') +
    xlab('') +
    scale_x_continuous(breaks = as.numeric(strftime(xaxticks,'%j')),
                       labels = strftime(xaxticks,'%b')) +
    theme(#strip.text.y.left = element_text(angle = 45),
      axis.text.x=element_text(size = 10,angle=45,hjust=1),
      axis.text=element_text(size=10),
      axis.title=element_text(size=12,face="bold"),
      strip.placement = "left",
      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 9),
      strip.background = element_rect(fill=NA),
      legend.title = element_blank(),
      legend.position = 'top'
    ) +
    scale_color_manual(values = clrs) +
    ggtitle(paste0(Proj,' Spring Operations and RST Counts 2023-2025'))
  figDwnStrmOpsFishByYrSpring
  ggplot2::ggsave(plot = figDwnStrmOpsFishByYrSpring,
                  filename = file.path(LocalWd,paste0(Proj,'_DSOpsSpring','.png')),
                  device='png',width=7,height=4)

  # Plot daily averages with Fall of each year: WSELV, Flow, Fish
  figDwnStrmOpsFishByYrFall <-
    ggplot(xl |>
             drop_na() |>
             mutate(name = as.factor(name)) |>
             dplyr::filter(grepl('Gen|Spill|Elev|Cum',name),
                           Year!=2021,Year!=2022,
                           JDAY>274),
           aes(x=JDAY,y = value,colour=Year,group = Year)) +
    geom_path(alpha = 0.6,linewidth=1.3) +
    geom_point(alpha = 0.6,linewidth=1.3,
               data = xl |>
                 dplyr::filter(grepl('Cum',name),
                               Year!=2021,Year!=2022,
                               JDAY>274),
               aes(x = JDAY,y=value,colour = Year),shape = 1) +
    facet_grid(name~.,scales = 'free', switch = "y",
               labeller = as_labeller(custom_labels)) +
    geom_vline(aes(linetype = Season,xintercept = JDAY,colour=Year),
               data = xAnnRSTSumAll |>
                 filter(grepl('RST_RO',name),!grepl('Spring',Season)) |> #
                 mutate(Season = gsub('Spring','Spring_D50',
                                      gsub('Fall','Fall_D50',Season))) |>
                 select(Year,Season,D50,name) |>
                 rename(Quant = name,JDAY=D50)) +
    ylab('') +
    xlab('') +
    scale_x_continuous(breaks = as.numeric(strftime(xaxticks,'%j')),
                       labels = strftime(xaxticks,'%b')) +
    theme(#strip.text.y.left = element_text(angle = 45),
      axis.text.x=element_text(size = 10,angle=45,hjust=1),
      axis.text=element_text(size=10),
      axis.title=element_text(size=12,face="bold"),
      strip.placement = "left",
      strip.text.x = element_text(size = 12),
      strip.text.y = element_text(size = 9),
      strip.background = element_rect(fill=NA),
      legend.title = element_blank(),
      legend.position = 'top'
    ) +
    scale_color_manual(values = clrs) +
    ggtitle(paste0(Proj,' Fall Operations and RST Counts 2023-2025'))
  figDwnStrmOpsFishByYrFall
  ggplot2::ggsave(plot = figDwnStrmOpsFishByYrFall,
                  filename = file.path(LocalWd,paste0(Proj,'_DSOpsFall','.png')),
                  device='png',width=7,height=4)


  write.csv(xAnnRSTSumAll |>
              filter(grepl('Gen|Spill|RST|Elev',name)) |>
              arrange(name) |>
              mutate(DMax = format(as.Date(DMax,origin = '2024-12-31'),'%m-%d'),
                     D50 = format(as.Date(D50,origin = '2024-12-31'),'%m-%d')),
            file=file.path(LocalWd,paste0(Proj,'_DSOps','.csv')))
}

