getSummariesAndPlotObserverEffects<-function(dataIn){

  set.data<-dataIn

  ######################################################
  #get regionList
  regionList<-sort(unique(as.character(set.data$RegionNameOrder)))

  #get unitList
  unitList<-sort(unique(as.character(set.data$Unit_Code)))

  #get siteList
  siteList<-sort(unique(as.character(set.data$Site_Name)))

  #get stationList
  stationList<-sort(unique(as.character(set.data$Plot_Name)))

  ###################################################################
  #Use function getSlopeObserver
  new.slopes<-try(getSlopeObserver(dataIn=set.data))
  head(new.slopes)

  ###################################################################
  #Get Overall Regional summary (All Regions)

  summary.regions<-summaryFunction(DataIn=new.slopes, factor="RegionNameOrder",response="slope")

  #get overall estimates of delta SET
  message("Saving Region-wide estimates and generating plots.")

  write.csv(summary.regions, file=paste(getwd(), "Results",paste("All", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  #plot overall estimates
  nStations=length(unique(set.data$Plot_Name))

  SETplot.cummulative<-ggplot(summary.all)+
    aes(x=year.visit, y=mean,group=1)+
    stat_smooth(method=lm,fullrange=FALSE, linetype=1, color="white")+
    geom_errorbar(ymax=summary.all$mean+summary.all$SE, ymin=summary.all$mean-summary.all$SE, width=0.2, size=0.6, color=I("grey30"))+
    geom_line(color="deepskyblue",linetype=1, size=1)+
    geom_point(size=2, color="deepskyblue")+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
    ylim(min(summary.all$mean-summary.all$SE)-2,max(summary.all$mean+summary.all$SE)+2)+
    #xlim(2002.1,2016.2)
    labs(x="Year (visits)", y="Change in elevation (mm)")
  #scale_x_discrete(limits=c(2002.1,2016.1),breaks=10, "Year")
  #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
  plot.1<-SETplot.cummulative+
    ggtitle(paste("Northeast Region"," SET data (n = ", nStations,")",sep=""))


  #save figure
  myFilepath<-paste(getwd(), "Results",sep="/")
  ggsave(plot.1, filename=paste("All", "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)
  ggsave(plot.1, filename=paste("All", "_delta_SET_year_visit.png",sep=""),path=myFilepath, width=6,height=5.5, limitsize=FALSE)

  ###################################################################
  #get estimates of delta SET by region
  message("Saving delta height (mm) estimates for each Region and generating plots.")

  region.delta.SET<-list()
  for(i in 1:length(regionList)){

    new.data<-subset(set.data, RegionName==regionList[i])
    regionName<-unique(as.character(new.data$RegionName))

    new.summary<-summaryDelta(dataIn=new.data)
    new.summary<-data.frame(RegionName=regionName, new.summary)

    write.csv(new.summary, file=paste(getwd(), "Results","Region_Results",regionName, paste(regionName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    region.delta.SET<-rbind(region.delta.SET, new.summary)

    #plot regional estimates
    nStations=length(unique(new.data$Plot_Name))

    SETplot.region<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      stat_smooth(method=lm,fullrange=FALSE, linetype=1, color="white")+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0.2, size=0.6, color=I("grey30"))+
      geom_line(color="deepskyblue",linetype=1, size=1)+
      geom_point(size=2, color="deepskyblue")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(min(new.summary$mean-new.summary$SE)-2,max(new.summary$mean+new.summary$SE)+2)+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Change in elevation (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.2<-SETplot.region+
      ggtitle(paste(regionName," SET data (n = ", nStations,")",sep=""))

    #save figure
    myFilepath<-paste(getwd(), "Results","Region_Results",regionName,sep="/")
    ggsave(plot.2, filename=paste(regionName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)
    ggsave(plot.2, filename=paste(regionName, "_delta_SET_year_visit.png",sep=""),path=myFilepath, width=6,height=5.5, limitsize=FALSE)

  }

  write.csv(region.delta.SET, file=paste(getwd(), "Results", paste("All_Regions", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  #get total # of stations
  nStations=length(unique(set.data$Plot_Name))

  #reorder by year.visit factor
  new.region.delta.SET<-with(region.delta.SET, region.delta.SET[order(RegionName,Year),])   #now plot all regions together

  SETplot.all.regions<-ggplot(new.region.delta.SET)+
    aes(x=Year, y=mean,group=1)+
    stat_smooth(method=lm,fullrange=TRUE, linetype=1, color="white")+
    geom_errorbar(ymax=new.region.delta.SET$mean+new.region.delta.SET$SE, ymin=new.region.delta.SET$mean-new.region.delta.SET$SE, width=0.2, size=0.6, color=I("grey30"))+
    geom_line(color="deepskyblue",linetype=1, size=1)+
    geom_point(size=2, color="deepskyblue")+
    theme(panel.background=element_rect(fill='white',color="black"))+
    theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
    theme(axis.line=element_line(color="black"))+
    theme(panel.background=element_rect(color="black"))+
    theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
          axis.text.y = element_text(size=12, color="black"),
          axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
          axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
    ylim(min(new.region.delta.SET$mean-new.region.delta.SET$SE)-2,max(new.region.delta.SET$mean+new.region.delta.SET$SE)+2)+
    xlim(2002,2016)+
    labs(x="Year (visits)", y="Change in elevation (mm)")
  #scale_x_discrete(limits=c(2002,2016),breaks=c(2002, 2004, 2006, 2008, 2010,2012,2014, 2016), "Year")
  #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
  plot.3<-SETplot.all.regions+facet_wrap(~RegionName,nrow=2,drop=FALSE,scales="fixed")+
    ggtitle(paste("Regional"," SET data (n = ", nStations,")",sep=""))

  #save figure (needs some tinkering to get year.visit factor to plot nicely with scales="fixed")
  myFilepath<-paste(getwd(), "Results","Region_Results",sep="/")
  ggsave(plot.3, filename=paste("By_region", "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=12,height=7, limitsize=FALSE)
  ggsave(plot.3, filename=paste("By_region", "_delta_SET_year_visit.png",sep=""),path=myFilepath, width=6,height=4, limitsize=FALSE)

  ###################################################################
  #get estimates of delta SET by refuge (Unit_Code)
  message("Saving delta height (mm) estimates for each Refuge (Unit_Code) and generating plots.")

  unit.delta.SET<-list()
  for(i in 1:length(unitList)){

    new.data<-subset(set.data, Unit_Code==unitList[i])
    unitName<-unique(as.character(new.data$Unit_Code))
    regionName<-unique(as.character(new.data$RegionName))
    stateName<-unique(as.character(new.data$State))

    new.summary<-summaryDelta(dataIn=new.data)
    new.summary<-data.frame(RegionName=regionName,State=stateName,Unit_Code=unitName, new.summary)

    write.csv(new.summary, file=paste(getwd(), "Results","Unit_Results",unitName, paste(unitName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    unit.delta.SET<-rbind(unit.delta.SET, new.summary)

    #plot regional estimates
    nStations=length(unique(new.data$Plot_Name))

    #now plot unit results
    SETplot.unit<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      stat_smooth(method=lm,fullrange=TRUE, linetype=1, color="white")+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0.2, size=0.6, color=I("grey30"))+
      geom_line(color="deepskyblue",linetype=1, size=1)+
      geom_point(size=2, color="deepskyblue")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(min(new.summary$mean-new.summary$SE)-2,max(new.summary$mean+new.summary$SE)+2)+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Change in elevation (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.4<-SETplot.unit+
      ggtitle(paste(unitName," SET data (n = ", nStations,")",sep=""))

    #save figure
    myFilepath<-paste(getwd(), "Results","Unit_Results",unitName,sep="/")
    ggsave(plot.4, filename=paste(unitName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)
    ggsave(plot.4, filename=paste(unitName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)

  }

  write.csv(unit.delta.SET, file=paste(getwd(), "Results", paste("All_Units", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #get estimates of delta SET by Site
  message("Saving delta height (mm) estimates for each Site (Site_Name) and generating plots.")

  site.delta.SET<-list()
  for(i in 1:length(siteList)){

    new.data<-subset(set.data, Site_Name==siteList[i])
    siteName<-unique(as.character(new.data$Site_Name))
    unitName<-unique(as.character(new.data$Unit_Code))
    regionName<-unique(as.character(new.data$RegionName))
    stateName<-unique(as.character(new.data$State))

    new.summary<-summaryDelta(dataIn=new.data)
    new.summary<-data.frame(RegionName=regionName,State=stateName,Unit_Code=unitName,Site_Name=siteName, new.summary)

    write.csv(new.summary, file=paste(getwd(), "Results","Site_Results",siteName, paste(siteName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    site.delta.SET<-rbind(site.delta.SET, new.summary)

    #plot regional estimates
    nStations=length(unique(new.data$Plot_Name))

    #now plot unit results
    SETplot.site<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      stat_smooth(method=lm,fullrange=TRUE, linetype=1, color="white")+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0.2, size=0.6, color=I("grey30"))+
      geom_line(color="deepskyblue",linetype=1, size=1)+
      geom_point(size=2, color="deepskyblue")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(min(new.summary$mean-new.summary$SE)-2,max(new.summary$mean+new.summary$SE)+2)+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Change in elevation (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.5<-SETplot.site+
      ggtitle(paste(siteName," SET data (n = ", nStations,")",sep=""))

    #save figure
    myFilepath<-paste(getwd(), "Results","Site_Results",siteName,sep="/")
    ggsave(plot.5, filename=paste(siteName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)

  }


  write.csv(site.delta.SET, file=paste(getwd(), "Results", paste("All_Sites", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #get estimates of delta SET by Station
  message("Saving delta height (mm) estimates for each Station (Plot_Name) and generating plots.")

  station.delta.SET<-list()
  for(i in 1:length(stationList)){

    new.data<-subset(set.data, Plot_Name==stationList[i])
    stationName<-unique(as.character(new.data$Plot_Name))
    siteName<-unique(as.character(new.data$Site_Name))
    unitName<-unique(as.character(new.data$Unit_Code))
    regionName<-unique(as.character(new.data$RegionName))
    stateName<-unique(as.character(new.data$State))

    new.summary<-summaryDelta(dataIn=new.data)
    new.summary<-data.frame(RegionName=regionName,State=stateName,Unit_Code=unitName,Site_Name=siteName,Plot_Name=stationName, new.summary)

    write.csv(new.summary, file=paste(getwd(), "Results","Station_Results",stationName, paste(stationName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    station.delta.SET<-rbind(station.delta.SET, new.summary)

    #now plot unit results
    SETplot.station<-ggplot(new.summary)+
      aes(x=year.visit, y=mean,group=1)+
      stat_smooth(method=lm,fullrange=TRUE, linetype=1, color="white")+
      geom_errorbar(ymax=new.summary$mean+new.summary$SE, ymin=new.summary$mean-new.summary$SE, width=0.2, size=0.6, color=I("grey30"))+
      geom_line(color="deepskyblue",linetype=1, size=1)+
      geom_point(size=2, color="deepskyblue")+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 55, hjust = 1.2,vjust=1.2, size=10, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=13, hjust=0.5, vjust=1.9),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=13))+
      ylim(min(new.summary$mean-new.summary$SE)-2,max(new.summary$mean+new.summary$SE)+2)+
      #xlim(200.8,2014.2)
      labs(x="Year (visits)", y="Change in elevation (mm)")
    #scale_x_discrete(limits=c(2009,2014),breaks=c(2009,2010,2011,2012,2013,2014,2015), "Year")+
    #scale_y_continuous(limits=c(-5,35),breaks=c(-5,0,5,10,15,20,25,30,35), "Change in elevation (mm)")
    plot.6<-SETplot.station+
      ggtitle(paste(stationName," SET data.",sep=""))

    #save figure
    myFilepath<-paste(getwd(), "Results","Station_Results",stationName,sep="/")
    ggsave(plot.6, filename=paste(stationName, "_delta_SET_year_visit.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)

    #############################################################################
    #use linear mixed-effects model to account for observer bias in pin-level regressions

    #############################################################################
    #make table of Total # of observers for each SET station

    #find SETs where more than 1 observer took measurements
    new.df<-table(set.data$Plot_Name,set.data$Last_Name)
    as.data.frame(new.df)
    head(new.df)
    new.df.2<-ifelse(new.df>1,1,0)
    totalObs<-rowSums(new.df.2)

    new.df.3<-data.frame(SET_num=seq(1,nrow(new.df.2)),
                                       Plot_Name=row.names(new.df.2),totalObs)

    row.names(new.df.3)<-NULL
    head(new.df.3)

    setCovs<-set.data[,c("RegionNameOrder", "State","Unit_Code","Site_Name","Plot_Name")]
    setCovs1<-unique(setCovs)
    NumObsTable<-merge(new.df.3, setCovs1, by="Plot_Name")
    NumObsTable.out<-NumObsTable[,c("RegionNameOrder","State","Unit_Code","Site_Name",
                                    "Plot_Name","totalObs")]
    colnames(NumObsTable.out)<-c("Region","State","Unit_Code","Site","Station","Total_Observers")
    head(NumObsTable.out)
    write.csv(NumObsTable.out,file=paste(getwd(),"Results","Tables","TotalObs.csv",sep="/"),row.names=FALSE)
    #############################################################################

head(all.slopes.out)
#############################################################################
#now plots
for(i in 1:length(stationList)){

  #for plotting raw data
  new.data<-subset(set.data, Plot_Name==stationList[i])
  head(new.data)

  siteName<-unique(new.data$Site_Name)
  stationName<-unique(new.data$Plot_Name)

  #get slopes
  new.all.slopes<-subset(all.slopes.out, Plot_Name==stationList[i])
  head(new.all.slopes)

  #save pin-level slopes that account for observer effects
  write.csv(new.all.slopes, file=paste(getwd(), "Results","Station_Results",stationName, paste(stationName, "_delta_SET_year.csv",sep=""),sep="/"),row.names=FALSE)

  #ALSO NEED YEAR_VISIT HERE
  #write.csv()

  #get pospinList
  pospinList<-unique(sort(as.character(new.data$pos.pin)))

    PinRegressionPlot<-ggplot(new.data)+
      aes(x=Year, y=value)+
      #geom_errorbar(ymax=summary$mean+summary$SE, ymin=summary$mean-summary$SE, width=0.1, size=0.5, color=I("grey50"))+
      geom_line(aes(group=variable, color=variable),linetype=2, size=0.5, alpha=0.3)+
      #stat_smooth(method="lm",aes(group=variable,color=variable),se=FALSE)+
      geom_abline(data=new.all.slopes, aes(slope=slope, intercept=intercept,color=variable),size=1)+
      geom_point(size=2, aes(color=variable),alpha=0.7)+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      #theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5,vjust=0.2, size=12, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
      #scale_color_manual("Pin", values=getPalette(9))+
      #scale_fill_manual("Pin", values=getPalette(9))+
      #scale_x_continuous(limits=c(2004,2010),breaks=c(2004,2005,2006,2007,2008,2009,2010), "Year")+
      ylab("Change in elevation (mm)")

    plot.7<-PinRegressionPlot+facet_grid(Plot_Name~Position_Name)+
      theme(panel.margin = unit(1, "lines"))+
      ggtitle(paste(stationName, " SET pin-level linear regression (Position x Station)"))

    #save figure
    myFilepath<-paste(getwd(), "Results","Station_Results",stationName,sep="/")
    ggsave(plot.7, filename=paste(stationName, "_Pin_slope_year_OBSeffects.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)




#create plots of pin-level linear models (to look at slopes)

    PinRegressionPlot<-ggplot(new.data)+
      aes(x=Year, y=value)+
      #geom_errorbar(ymax=summary$mean+summary$SE, ymin=summary$mean-summary$SE, width=0.1, size=0.5, color=I("grey50"))+
      geom_line(aes(group=variable, color=variable),linetype=2, size=0.5, alpha=0.3)+
      stat_smooth(method="lm",aes(group=variable,color=variable),se=FALSE)+
      geom_point(size=2, aes(color=variable),alpha=0.7)+
      theme(axis.line=element_line(color="black"))+
      theme(panel.background=element_rect(fill='white',color="black"))+
      theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
      #theme(panel.background=element_rect(color="black"))+
      theme(axis.text.x = element_text(angle = 45, hjust = 0.5,vjust=0.2, size=12, color="black"),
            axis.text.y = element_text(size=12, color="black"),
            axis.title.x = element_text(size=15, hjust=0.5, vjust=0.2),
            axis.title.y = element_text(angle = 90, vjust=1.2, size=15))+
      #scale_color_manual("Pin", values=getPalette(9))+
      #scale_fill_manual("Pin", values=getPalette(9))+
      #scale_x_continuous(limits=c(2004,2010),breaks=c(2004,2005,2006,2007,2008,2009,2010), "Year")+
      ylab("Change in elevation (mm)")

    plot.8<-PinRegressionPlot+facet_grid(Plot_Name~Position_Name)+
      theme(panel.margin = unit(1, "lines"))+
      ggtitle(paste(stationName, " SET pin-level linear regression (Position x Station)"))

    #save figure
    myFilepath<-paste(getwd(), "Results","Station_Results",stationName,sep="/")
    ggsave(plot.8, filename=paste(stationName, "_Pin_slope_year.pdf",sep=""),path=myFilepath, width=9,height=6.5, limitsize=FALSE)


  }

  #write.csv(station.delta.SET, file=paste(getwd(), "Results", paste("All_Stations", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

}