# inspired by http://paulbutler.org/archives/visualizing-facebook-friends/

## acknowledgements (portions of code especially!)
# with help from http://web.stanford.edu/~cengel/cgi-bin/anthrospace/great-circles-on-a-recentered-worldmap-in-ggplot
# and from http://stackoverflow.com/questions/19621057/plotting-great-circle-paths

library(rworldmap)
library(WDI) # WORLD BANK INDICATORS
library(ggplot2)
library(sp)
library(plyr)
library(geosphere)

#produces a list of indicator IDs and names as a matrix
indicatorList <- WDIsearch('aid flows')
indicatorList

year <- 2012
mdat <- map_data('world')
donorISO3 <- substr("DC.DAC.FRAL.CD",start=8,stop=10)
donorISO3

dFdonor <- WDI(indicator="DC.DAC.FRAL.CD",start=year,end=year,extra=TRUE)
names(dFdonor)  
colnames(dFdonor)[colnames(dFdonor)=="DC.DAC.FRAL.CD"]<-"indicator"
#divide by 10^6 for million dollars
dFdonor$indicator <- dFdonor$indicator * 1/10^6

dFdonor<-subset(dFdonor, is.na(indicator)==FALSE & region!="Aggregates") #subsets data for complete observations - removes aggregates
names(dFdonor)
coordinates<-read.csv("Country_List_ISO_3166_Codes_Latitude_Longitude.csv") # file from here: https://opendata.socrata.com/dataset/Country-List-ISO-3166-Codes-Latitude-Longitude/mnkm-8ram

coordinates
dFdonor_plot<-merge(dFdonor, coordinates, by.x=c("iso3c"), by.y=c("Alpha.3.code"))
mdat <- map_data('world')
gmap <- ggplot() + geom_polygon(dat=mdat, aes(long, lat, group=group), colour="lightblue", size=0.1, alpha=0.3, fill=rgb(7,0,30,maxColorValue=255))+theme(panel.background=element_rect(fill=rgb(7,0,30,maxColorValue=255)),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),plot.background=element_rect(fill=rgb(7,0,30,maxColorValue=255)),axis.text=element_blank(),axis.ticks=element_blank())
gmap

gmap_nolabel <-gmap + geom_point(data=dFdonor_plot, aes(Longitude..average., Latitude..average., size=indicator, fill=indicator, alpha=0.05), shape=21, colour=NA) + scale_size_area(max_size=15,guide=FALSE)+guides(alpha=FALSE)+theme(legend.position="none")

gmap_nolabel

#now can I plot lines from the centroid of the donor to the centroids of the recipients
xDonor <- coordinates$Longitude..average.[ which(coordinates$Alpha.3.code==donorISO3) ]
yDonor <- coordinates$Latitude..average.[ which(coordinates$Alpha.3.code==donorISO3) ] 
xRecips <- dFdonor_plot$Longitude..average.[ which(dFdonor_plot$indicator > 0) ]
yRecips <- dFdonor_plot$Latitude..average.[ which(dFdonor_plot$indicator > 0) ]
amountRecips <- dFdonor_plot$indicator[ which(dFdonor_plot$indicator > 0) ]

gc<-gcIntermediate(c(xDonor,yDonor),dFdonor_plot[,c('Longitude..average.','Latitude..average.')], n=50, breakAtDateLine=FALSE, sp=TRUE)
gc.plot <- ggplot2:::fortify.SpatialLinesDataFrame(gc) # convert into something ggplot can plot
summary(gc.plot)
dFdonor_plot$id <-c(1:nrow(dFdonor_plot))
dFdonor_plot_new <- merge(gc.plot, dFdonor_plot, all.x=TRUE, by="id")

dFdonor_plot_new<-dFdonor_plot_new[dFdonor_plot_new$indicator>0, ]
summary(dFdonor_plot_new$indicator)

png("greatcircle_fra.png",width=2000,height=1000, res=NA, units="px")
gmap_nolabel+geom_line(data=dFdonor_plot_new,aes(x=long,y=lat,group=id),color=rgb(1,1,1,alpha=(sqrt(dFdonor_plot_new$indicator)/sqrt(sum(dFdonor_plot_new$indicator)))*10))
dev.off()
