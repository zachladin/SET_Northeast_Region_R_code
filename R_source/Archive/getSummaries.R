getSummaries<-function(dataIn){

  set.data<-dataIn

  ######################################################
  #get regionList
  regionList<-sort(unique(as.character(set.data$RegionName)))

  #get unitList
  unitList<-sort(unique(as.character(set.data$Unit_Code)))

  #get siteList
  siteList<-sort(unique(as.character(set.data$Site_Name)))

  #get stationList
  stationList<-sort(unique(as.character(set.data$Plot_Name)))

  ###################################################################
  #get overall estimates of delta SET
  summary.all<-summaryDelta(dataIn=set.data)
  summary.all<-data.frame(RegionName="All",Unit_Code="All",summary.all)
  write.csv(summary.all, file=paste(getwd(), "Results",paste("All", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #get estimates of delta SET by region
  region.delta.SET<-list()
  for(i in 1:length(regionList)){

    new.data<-subset(set.data, RegionName==regionList[i])
    regionName<-unique(as.character(new.data$RegionName))

    new.summary<-summaryDelta(dataIn=new.data)
    new.summary<-data.frame(RegionName=regionName, new.summary)

    write.csv(new.summary, file=paste(getwd(), "Results","Region_Results",regionName, paste(regionName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    region.delta.SET<-rbind(region.delta.SET, new.summary)
  }

  write.csv(region.delta.SET, file=paste(getwd(), "Results", paste("All_Regions", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #get estimates of delta SET by refuge (Unit_Code)
  unit.delta.SET<-list()
  for(i in 1:length(regionList)){

    new.data<-subset(set.data, Unit_Code==unitList[i])
    unitName<-unique(as.character(new.data$Unit_Code))
    regionName<-unique(as.character(new.data$RegionName))
    stateName<-unique(as.character(new.data$State))

    new.summary<-summaryDelta(dataIn=new.data)
    new.summary<-data.frame(RegionName=regionName,State=stateName,Unit_Code=unitName, new.summary)

    write.csv(new.summary, file=paste(getwd(), "Results","Unit_Results",unitName, paste(unitName, "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

    unit.delta.SET<-rbind(unit.delta.SET, new.summary)
  }

  write.csv(unit.delta.SET, file=paste(getwd(), "Results", paste("All_Units", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #get estimates of delta SET by Site
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
  }

  write.csv(site.delta.SET, file=paste(getwd(), "Results", paste("All_Sites", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)

  ###################################################################
  #get estimates of delta SET by Station


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
  }

  write.csv(station.delta.SET, file=paste(getwd(), "Results", paste("All_Stations", "_delta_SET_year_visit.csv",sep=""),sep="/"),row.names=FALSE)


}