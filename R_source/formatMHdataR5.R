#Format SET data function SETformat for Region 4
formatMHdataR5<-function(dataIn, dataDir){

  myDir<-as.character(dataDir)
  
  message("Reading raw data in.")
  new.data<-dataIn
  
  #rename DataProcessingLevelCode to DataProcessingLevelLabel
  #names(new.data)[names(new.data)=="DataProcessingLevelCode"]<-"DataProcessingLevelLabel"
  
  #add proper Station_Name column, read in SET station data
  SETcoords.1<-read.csv(file=paste(myDir,"/Data/SET Stations.csv",sep=""), header=TRUE, skip=3)
  
  #get data.frame of StationCode and StationName
  stationName.df<-unique(SETcoords.1[,c("StationName","StationCode")])
  colnames(stationName.df)<-c("Station_Name","Station_Code")
  
  #merge with new.data
  new.data<-merge(new.data, stationName.df, by="Station_Name",all.x=TRUE)
  
  
  #look at whether or not data has been accepte
  unique(new.data$DataProcessingLevelLabel)
  
  #keep only "Accepted" data
  #new.data.sub<-subset(new.data, DataProcessingLevelLabel=="Accepted")
  new.data.sub<-new.data
  
  #Remove rows if they contain NAs or blank cells.
  data.1<-subset(new.data.sub, EventDateTimeWithOffset != "")

  #create station.event.mh column
  data.1$station.event.mh<-paste(data.1$Station_Name, data.1$EventDateTimeWithOffset, data.1$MarkerHorizonID,sep="_")
  
  #convert DateEstablished from yyyy-mm-dd to mm/dd/yyyy format
  data.1$DateEstablished<-paste(read.table(text=as.character(data.1$DateEstablished),sep="-")$V2,
                                read.table(text=as.character(data.1$DateEstablished),sep="-")$V3,
                                read.table(text=as.character(data.1$DateEstablished),sep="-")$V1,
                                sep="/")
  
  
  #add DateEstablished into EventDate column
  
  #get list of MarkerHorizonID
  
  markerHorizonList<-unique(data.1$MarkerHorizonID)
  
  save.mh<-list()
  for(i in 1:length(markerHorizonList)){
    
    #print(markerHorizonList[i])
    
    new.mh.data<-subset(data.1, MarkerHorizonID==markerHorizonList[i])
    
    #get list of cores
    coreList<-unique(new.mh.data$CoreObservationNumber)
    
    save.core<-list()
    for(j in 1:length(coreList)){
      
      #print(coreList[j])
      
      #now look at unique CoreObservations
      new.label.data<-unique(subset(new.mh.data, CoreObservationNumber == coreList[j]))
      
      #add date established and measure of 0 
      names(new.label.data)
      date.est<-data.frame("Refuge"=unique(new.label.data$Refuge),
                           "Site_Name" = unique(new.label.data$Site_Name),
                           "Station_Name" = unique(new.label.data$Station_Name),
                           "Station_Code" = unique(new.label.data$Station_Code),
                           "EventDateTimeWithOffset" = unique(new.label.data$DateEstablished),
                           "MarkerHorizonID"= unique(new.label.data$MarkerHorizonID),
                           "MarkerHorizonLabel" = unique(new.label.data$MarkerHorizonLabel), 
                           "DateEstablished" = unique(new.label.data$DateEstablished),
                           "CoreObservationNumber" =  0,
                           "DepthToBenchmark_mm" = 0,    
                           "Core_Condition" = "NA",
                           "Core_Type" = NA,
                           "Core_Notes" = "Established_feldspar",            
                           "DataProcessingLevelDate1" = unique(new.label.data$DataProcessingLevelDate1)[1],
                           "DataProcessingLevelLabel" = unique(new.label.data$DataProcessingLevelLabel)[1],
                           "DataProcessingLevelNote" = NA,
                           "station.event.mh" = paste(unique(new.label.data$Station_Name), unique(new.label.data$DateEstablished),unique(new.label.data$MarkerHorizonID),sep="_"))
      
      #add to new.label.data
      new.label.data.2<-rbind(date.est, new.label.data)
      
      #add Delta column (Difference of measurement from baseline (0)), to new.label.data.2
      new.label.data.2$Delta_MH_mm<-new.label.data.2$DepthToBenchmark_mm-0
      
      #compile
      save.core<-rbind(save.core, new.label.data.2)
    }
    
    save.mh<-rbind(save.mh, save.core)
      
  }
  
  #get all unique records
  save.mh.out<-unique(save.mh)
    
  
  #Add Year, Month, and Day columns to data.
  message("Adding year, month, and day columns to data.")
  date.data<-data.frame(EventDate=save.mh.out$EventDateTimeWithOffset)
  date.data$EventDate<-as.character(date.data$EventDate)
  date.string<-read.table(text=as.character(date.data$EventDate), sep="/",colClasses = "character")
  colnames(date.string)<-c("Month","Day","Year")
  date.string$Year<-as.factor(date.string$Year)

  date.string$Date<-as.Date(as.character(paste(date.string$Year,
                                                   date.string$Month,
                                                   date.string$Day,sep="-"),
                                             format = "%Y-%b-%d"))
  #Make column for ordinal day.
  date.string$ord.Day <- as.integer(format(date.string$Date, "%j"))

  #Compile data
  data.2<-cbind(save.mh.out,date.string)
  
  #####################################################################################################

  #Create new columns for "station.core.year", "station.year", and "station.year.day", using paste() function.
  data.2$station.mh.year<-paste(data.2$Station_Name, data.2$MarkerHorizonID, data.2$Year,sep=".")
  data.2$station.year<-as.factor(paste(data.2$Station_Name, data.2$Year,sep="."))
  data.2$station.year.ord.day<-as.factor(paste(data.2$Station_Name, data.2$Year, data.2$ord.Day,sep="."))
  #View first few rows of data.frame.
  head(data.2)

  data.out<-data.2
  ########################################################################################################
  #check that each station has more than 1 visit
  message("Checking data for inconsistencies.")
  check.data<-aggregate(Year~MarkerHorizonID, data=data.out, FUN=function(x){length(unique(x))})
  check.data$enoughVisits<-ifelse(check.data[,2]>1,"Yes","No")

  #remove any plots with only 1 year.visit
  check.data.keep<-check.data[check.data$enoughVisits!="No",]

  #subset data out by list of check.data.keep
  keepMarkerHorizons<-unique(as.character(check.data.keep$MarkerHorizonID))

  #data.out$Station_Name<-as.character(data.out$Station_Name)
  data.out.keep<-data.out[data.out$MarkerHorizonID %in% keepMarkerHorizons,]
  data.out.keep$Station_Name<-as.factor(as.character(data.out.keep$Station_Name))

  names(data.out.keep)
  
  #fix NWR names
  #remove "National Wildlife Refuge" from RefugeName or Wildlife Managment Area
  data.out.keep$Refuge<-trimws(gsub("Wildlife Management Area","",gsub("National Wildlife Refuge","",gsub("National Wildlife Refuges","",data.out.keep$Refuge))))
  
  #remove "NWR" from Site_Name
  data.out.keep$Site_Name<-trimws(gsub("NWR","",data.out.keep$Site_Name))
  
###################################################################
#Read in R5_SET to get location info

#add Lat/Long if needed to rawData
  #rename Longitude and Latitude columns
  names(SETcoords.1)[names(SETcoords.1)=="StationLongitude"]<-"Longitude"
  names(SETcoords.1)[names(SETcoords.1)=="StationLatitude"]<-"Latitude"
  

SETstates<-read.csv(file=paste(myDir,"/Data/R5_SET.csv",sep=""),header=TRUE)

SETstates.sub<-unique(SETstates[,c("State","Refuge")])

#merge with SETcoords
SETcoords<-merge(SETcoords.1, SETstates.sub,by="Refuge",all.x=TRUE)

R5_coords<-subset(SETcoords, RegionNumber==5)
R5_coords.sub<-unique(R5_coords[,c("State","StationName","StationCode","Latitude","Longitude")])
colnames(R5_coords.sub)<-c("State","Station_Name","Station_Code","Latitude","Longitude")

#Merge data.out.keep and Region.lookup.

  #Add Longitude and Latitude coords for making maps.
  message("Adding State and Lat/Long coords for SET stations to data.")

  #make data.out.keep$Station_Name a character
  data.out.keep$Station_Name<-as.character(data.out.keep$Station_Name)
  
  #Merge new.data.out with XYdata.
  data.out.2<-merge(data.out.keep, R5_coords.sub, by=c("Station_Name","Station_Code"),all.x=TRUE)
  
  #Save data (as a .csv file).
  message("Saving SET with delta height data to .csv file.")
  write.csv(data.out.2, file=paste(myDir,"Data",paste(paste("All_R4_MH_data_formatted_",Sys.Date(),sep=""),"csv",sep="."),sep="/"), row.names=FALSE)


  return(data.out.2)

}