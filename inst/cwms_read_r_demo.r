# Demonstrate cwms_read_CDA.r usage
rm(list = ls())
library(devtools)
install_github("nbuccola/cwmsr")
install.packages('cwmsr')
library(cwmsr)
LocalWd <- 'C:/Users/g2echnb9/OneDrive - US Army Corps of Engineers/Desktop'
CDApath <- 'https://wm.nww.ds.usace.army.mil:8243/nwdp-data/'
# Setup input variables
beginDate <- strptime('2024-01-01',"%Y-%m-%d",tz = 'PST8PDT')
endDate <- strptime('2024-12-31',"%Y-%m-%d",tz = 'PST8PDT')
CWMSpaths <- c('DET.Elev-RuleCurve.Inst.~1Day.0.CENWP-CALC',
               'DET.Elev-Forebay.Inst.0.0.Best',
               'DET.Flow-Gen.Ave.1Hour.1Hour.Best',
               'DET.Flow-Spill.Ave.1Hour.1Hour.Best',
               'BCL.Flow-Out.Ave.1Hour.1Hour.Best',
               'BCLO.Temp-Water.Inst.0.0.Best') 
# Access CWMS data via CDA (CWMS Data Acquisition) 
x24 <- get_cwms(CWMSpaths,start_date=beginDate,end_date=endDate,
                CDApath=CDApath)

library(dplyr)
library(tidyr)
cwmsPivot_longer <- function(x){
  x |> 
    tidyr::pivot_longer(-Date) |>
    tidyr::separate(name, c("Site", "D1","D2","AvgPeriod","D4","D5"), sep = "\\.") |>
    dplyr::select(-D4,-D5) |> 
    dplyr::mutate(D1a = D1) |>
    tidyr::separate(D1, c("D1b", "D1c"), sep = "\\-") 
}
  
x = cwmsPivot_longer(x24) |>
# Reformat factors for plotting in GGplot
  dplyr::mutate(Site = factor(Site,levels = c('DET','BCL','BCLO'),ordered=T),
                D1a = as.factor(D1a),
                D1b = as.factor(gsub('Flow','Flow[cfs]',
                                     gsub('Elev','Elev[ft]',
                                          gsub('Temp','Temp[DegF]',D1b)))),
                D1c = as.factor(D1c),
                D2 = as.factor(D2))
figTitle <- "DET-BCL_2024_Temperature_Operations"

fig <-
  ggplot(x,
         aes(x=Date,y=value,colour=D1a)) +
  geom_path(alpha=0.6,linewidth = 1) +
  geom_point(alpha=0.6,size = 0.5) +
  facet_grid(D1b~.,scales = 'free', switch="y") +
  scale_x_datetime(breaks = seq.POSIXt(from = min(x$Date),to = max(x$Date),by = 'month'),
    date_labels = '%b'
  ) +
  xlab('') +
  ylab('') +
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.placement = "outside",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.background = element_rect(fill=NA),
        legend.title = element_blank()
        ) +
  ggtitle(figTitle)
fig

ggplot2::ggsave(plot = fig,filename = file.path(LocalWd,paste0(figTitle,'.png')),
                device='png',width=10,height=6)
