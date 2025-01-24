# cwmsr
R Functions to get CWMS data from the USACE CDA

2 current options for installation in R:

1) You can try using different parameter alternatives in the devtools package.
install.packages("devtools")
library(devtools)
install_github("nbuccola/cwmsr")

2) Another way is trying to download the zip file and install it with the normal install.packages() function in R with:
install.packages(file_name_and_path, repos = NULL, type="source")

Once installed in R, see inst/cwms_read_r_demo.r (or code below) for a demo!


# Demo of cwms_read.r

```r
library(devtools)
install_github("nbuccola/cwmsr")
install.packages('cwmsr')
library(cwmsr)
library(foreach)
LocalWd <- ''
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

x = x24
library(dplyr)
library(tidyr)
cwmsPivot_longer <- function(x){
  x |>
    tidyr::pivot_longer(-Date) |>
    mutate(name = gsub('.Ave|.1Hour|.CENWP-CALC|.~1Day|.Best|.Inst|.0|Best','',name)) |>
    tidyr::separate(name, c("Site", "D1"), sep = "\\.") |>
    tidyr::separate(D1, c("D1a", "D1b"), sep = "\\-")
}

x = cwmsPivot_longer(x24) |>
# Reformat factors for plotting in GGplot
  dplyr::mutate(Site = factor(Site,levels = c('DET','BCL','BCLO'),ordered=T),
                D1c = as.factor(paste(Site,D1a,D1b,sep = '-')),
                D1a = as.factor(gsub('Flow','Flow [cfs]',
                                     gsub('Elev','Elev [ft]',
                                          gsub('Temp','Temp [DegF]',D1a)))),
                D1b = as.factor(D1b)
                )
figTitle <- "DET-BCL_2024_Temperature_Operations"

library(ggplot2)
fig <-
  ggplot(x |>
           drop_na(),
         aes(x=Date,y=value,colour=D1c)) +
  geom_line(alpha=0.6,linewidth = 1) +
  #geom_point(alpha=0.6,size = 0.5) +
  facet_grid(D1a~.,scales = 'free', switch="y") +
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

ggplot2::ggsave(plot = fig,filename = file.path(LocalWd,paste0(figTitle,'.png')),
                device='png',width=10,height=6)
```
