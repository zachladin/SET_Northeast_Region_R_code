plotData<-function(dataIn){
  
  
  #summary function creates table of n, mean, var, SD, and SE
  summaryFunction <- function(DataIn, factor, response){
    summaryOut <- ddply(DataIn, factor, .fun = function(xx){
      c(n = length(xx[,response]),
        mean = mean(xx[,response],na.rm=TRUE),
        var = var(xx[,response],na.rm=TRUE),
        SD = sd(xx[,response],na.rm=TRUE),
        SE = sqrt(var(xx[,response])/length(xx[,response])),
        CV = sd(xx[,response],na.rm=TRUE)/mean(xx[,response],na.rm=TRUE),
        lwr = mean(xx[,response],na.rm=TRUE)-sqrt(var(xx[,response])/length(xx[,response]))*1.96,
        upr = mean(xx[,response],na.rm=TRUE)+sqrt(var(xx[,response])/length(xx[,response]))*1.96)
    })
    return(summaryOut)
    dev.off()
  }
  
  ##################################################################################################
  #get summary stats for SET all data 
  unitName="EBF"
  #subset data
  data.sub<-subset(delta.set.melt, Unit_Code==unitName)
  
  delta.set.melt<-data.sub
  message("Summarizing data.")
  delta.summary<-summaryFunction(DataIn=delta.set.melt, response="value",factor="year.visit")
  delta.summary$Year<-as.integer(substr(delta.summary$year.visit,1,4))
  delta.summary$year.visit<-as.factor(delta.summary$year.visit)
  #get global mean for delta elevation 
  global.mean<-mean(delta.summary$mean) # 6.33819
  global.sd<-sd(delta.summary$mean) # 8.084154
  global.se<-global.sd/sqrt(length(delta.summary$mean)) # 1.476
  
  ##################################################################################################
  nPoints<-length(unique(delta.set.melt$Plot_Name))
  
   message(paste("Generating and saving plots of data from ", unitName, ".",sep=""))
   #create plot.1 of delta height vs. year.visit
  SETplot.cummulative<-ggplot(delta.summary)+
    aes(x=year.visit, y=mean,group=1)+
    stat_smooth(method=lm,fullrange=FALSE, linetype=2, color="white")+
    geom_errorbar(ymax=delta.summary$mean+delta.summary$SE, ymin=delta.summary$mean-delta.summary$SE, width=0.2, size=0.3, color=I("grey30"))+     
    geom_line(color="deepskyblue",linetype=1, size=1.25)+
    geom_point(size=2.5, color="deepskyblue")+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"), 
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
    ylim(-5,30)+
    #xlim(2008.8,2014.2)
    labs(x="Year (visits)", y="Change in elevation (mm)")
  #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
  #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
  plot.1<-SETplot.cummulative+
    ggtitle(paste(unitName," SET data (n = ", nPoints,")",sep=""))
  
  #save figure
  myFilepath<-paste(getwd(), "Results","Unit_Results",sep="/")
  ggsave(plot.1, filename="SET_mean_delta_height_year_visit.pdf",path=myFilepath, width=9,height=6.5, limitsize=FALSE)
  ###################################################################
  #Plot SET data over time (at each pin) #for Wertheim (data.3)
  
  #get list of Plot_Name
  plotList<-sort(unique(delta.set.melt$Plot_Name))
  
  station1.data<-subset(delta.set.melt, Plot_Name==plotList[1])
  
  #Rcolorbrewer function
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  new.data<-station1.data
  SETplot<-ggplot(new.data)+
    aes(x=Year, y=value)+
    #geom_errorbar(ymax=summary$mean+summary$SE, ymin=summary$mean-summary$SE, width=0.1, size=0.5, color=I("grey50"))+     
    geom_line(aes(group=variable, color=variable),linetype=2, size=0.5, alpha=0.3)+
    stat_smooth(method="lm",aes(group=variable,color=variable),se=FALSE)+
    geom_point(size=2, aes(color=variable),alpha=0.7)+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    #theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"), 
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
    scale_color_manual("Pin", values=getPalette(9))+
    scale_fill_manual("Pin", values=getPalette(9))+
    #scale_x_continuous(limits=c(2004,2010),breaks=c(2004,2005,2006,2007,2008,2009,2010), "Year")+
    ylab("Change in elevation (mm)")
  
  SETplot+facet_grid(Plot_Name~Position_Name)+
    theme(panel.margin = unit(1, "lines"))+
    ggtitle("SET pin-level linear regression (Position x Station)")
  
  #save figure
  myFilepath<-paste(getwd(), "Results","Unit_Results",Unit_estimates,sep="/")
  ggsave(plot.1, filename="SET_mean_delta_height_year_visit.pdf",path=myFilepath, width=9,height=6.5, limitsize=FALSE)
  
  ###################################################################
  #create plot for Sachuest
  SETplot.cummulative<-ggplot(delta.summary)+
    aes(x=Year, y=mean)+
    geom_errorbar(ymax=delta.summary$mean+delta.summary$SE, ymin=delta.summary$mean-delta.summary$SE, width=0.1, size=0.5, color=I("grey20"))+     
    geom_line(color="blue",linetype=1, size=1)+
    stat_smooth(method=lm,fullrange=FALSE, linetype=2, color="black")+
    geom_point(size=4, color="blue")+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(family="Arial",angle = 0, hjust = 0.5,vjust=0.2, size=12, color="black"), 
          axis.text.y = element_text(family="Arial",size=12, color="black"),
          axis.title.x = element_text(family="Arial",size=15, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(family="Arial",angle = 90, vjust=1.2, size=15))+
    scale_x_continuous(limits=c(2003.8,2010.2),breaks=c(2004,2005,2006,2007,2008,2009,2010), "Year")+
    scale_y_continuous(limits=c(-7,12),breaks=c(-7,-6,-5,-4,-3,-2,-1,0,1,2,3,4,5,6,7,8,9,10,11,12), "Change in elevation (mm)")
  SETplot.cummulative+
    annotate("text", x = 2008.5, y = -5, label ="Elevation: 2.20 ± 1.43 mm/year")+
    ggtitle("Sachuest (SETs 4, 5, and 6)")
  
  
  #save figure
  myFilepath<-paste(getwd(), "Results","Unit_Results",Unit_estimates,sep="/")
  ggsave(plot.1, filename="SET_mean_delta_height_year_visit.pdf",path=myFilepath, width=9,height=6.5, limitsize=FALSE)
  
  ###################################################################
  #Plot SET data over time (at each pin) for Sachuest
  
  #Rcolorbrewer function
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  SETplot<-ggplot(delta.set.melt)+
    aes(x=Year, y=value)+
    #geom_errorbar(ymax=summary$mean+summary$SE, ymin=summary$mean-summary$SE, width=0.1, size=0.5, color=I("grey50"))+     
    geom_line(aes(group=variable, color=variable),linetype=2, size=0.5, alpha=0.3)+
    stat_smooth(method="lm",aes(group=variable,color=variable),se=FALSE)+
    geom_point(size=2, aes(color=variable),alpha=0.7)+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    #theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(family="Arial",angle = 45, hjust = 0.5,vjust=0.2, size=12, color="black"), 
          axis.text.y = element_text(family="Arial",size=12, color="black"),
          axis.title.x = element_text(family="Arial",size=15, hjust=0.5, vjust=0.2),
          axis.title.y = element_text(family="Arial",angle = 90, vjust=1.2, size=15))+
    scale_color_manual("Pin", values=getPalette(9))+
    scale_fill_manual("Pin", values=getPalette(9))+
    scale_x_continuous(limits=c(2004,2010),breaks=c(2004,2005,2006,2007,2008,2009,2010), "Year")+
    ylab("Change in elevation (mm)")
  
  SETplot+facet_grid(Station~Position)+
    theme(panel.margin = unit(1, "lines"))+
    ggtitle("SET pin-level linear regression (Position x Station)")
  
  #save figure
  myFilepath<-paste(getwd(), "Results","Unit_Results",Unit_estimates,sep="/")
  ggsave(plot.1, filename="SET_mean_delta_height_year_visit.pdf",path=myFilepath, width=9,height=6.5, limitsize=FALSE)
  
  
  
  
}