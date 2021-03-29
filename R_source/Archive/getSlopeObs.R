getSlopeObserver<-function(DataIn){
  set.data<-DataIn

  dataCovs<-unique(set.data[,c("State","Unit_Code")])

  ######################################################
  message("Compiling lists from data.")
  #get refugeList
  refugeList<-sort(unique(as.character(set.data$Unit_Code)))


  ######################################################

  message("Fitting linear model to get slopes of delta height (mm) at each pin, and averaging for each SET station.")

  all.pins<-list()
  all.positions<-list()
  all.stations<-list()
  all.refuges<-list()
  for(j in 1:length(refugeList)){
    refuge.data<-subset(set.data,Unit_Code==refugeList[j])

    refugeName<-as.character(unique(refuge.data$Unit_Code))
    #get smiUnitList
    smiUnitList<-sort(unique(as.character(refuge.data$Site_Name)))

  refuge.pins<-list()
  refuge.positions<-list()
  refuge.stations<-list()
  for(k in 1:length(smiUnitList)){

    smi.data<-subset(refuge.data, Site_Name==smiUnitList[k])

    smiUnitName<-as.character(unique(smi.data$Site_Name))

    #get stationList
    stationList<-sort(unique(as.character(smi.data$Plot_Name)))

    #get refuge covariates
    refugeCovs<-unique(smi.data[,c("State","Unit_Code",
                                    "Site_Name","Plot_Name","Lat","Long")])


  smiUnit.pin.slopes<-list()
  smiUnit.positions<-list()
  smiUnit.stations<-list()
  for(i in 1:length(stationList)){

    new.data<-subset(smi.data, Plot_Name==stationList[i])

    stationName<-as.character(unique(new.data$Plot_Name))

    #get pospinList
    pospinList<-unique(sort(as.character(new.data$pos.pin)))

    #get covariates
    stationCovs<-unique(new.data[,c("State","Unit_Code",
                                    "Site_Name","Plot_Name","Lat","Long")])

    positionCovs<-unique(new.data[,c("State","Unit_Code","Site_Name","Plot_Name","Position_Name","Lat","Long")])

    regResults.out<-list()
    for(k in 1:length(pospinList)){
      sub.data<-subset(new.data, pos.pin==pospinList[k])
      sub.data$Last_Name<-as.factor(as.character(sub.data$Last_Name))
      sub.data$year.visit<-as.integer(as.factor(sub.data$year.visit))

        sub.dataCovs<-unique(sub.data[,c("State","Unit_Code",
                                         "Site_Name","Plot_Name","Position_Name","variable","pos.pin")])

        mod.1<-tryCatch({
          lmer(value ~ year.visit + (1|Last_Name),data=sub.data)
        },error=function(cond2){
          cond2=lm(value ~ year.visit, data=sub.data)
        })

        #get slope over years
        try(slope<-coef(summary(mod.1))[ , "Estimate"][2])
        try(intercept<-coef(summary(mod.1))[ , "Estimate"][1])

        #combine slope with station covs
        regResults<-data.frame(sub.dataCovs,slope=slope,intercept=intercept)

        #compile regResults over each pin
        regResults.out<-rbind(regResults.out, regResults)
      }

     #save station results
     write.csv(regResults.out, file=paste(getwd(), "Results","Station_Results",stationName,paste(stationName,"pin-level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

     #get means for each position
     station.positions<-summaryFunction(DataIn=regResults.out, factor="Position_Name", response="slope")
     station.positions.out.1<-merge(station.positions, positionCovs, by="Position_Name")
     station.positions.out<-station.positions.out.1[,c("State","Unit_Code","Site_Name","Plot_Name","Lat","Long",
                                                   "Position_Name","n","mean","var","SD","SE","CV","lwr","upr")]

     #save compiled Position-level slopes
     write.csv(station.positions.out, file=paste(getwd(),"Results","Station_Results",stationName,paste(stationName,"Position_level_slopes_Obs.csv",sep="_"),sep="/"),row.names=FALSE)

     #get station means
     station.slopes<-summaryFunction(DataIn=station.positions.out, factor="Plot_Name",response="mean")
     station.merge.1<-merge(station.slopes, refugeCovs, by="Plot_Name")
     station.merge<-station.merge.1[,c("State","Unit_Code","Site_Name","Plot_Name","Lat","Long",
                                       "n","mean","var","SD","SE","CV","lwr","upr")]

     write.csv(station.merge, file=paste(getwd(), "Results","Station_Results",stationName,paste(stationName,"Station-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)


     #compile pin-level slopes for each smiUnit
     smiUnit.pin.slopes<-rbind(smiUnit.pin.slopes,regResults.out)

      #compile position means
      smiUnit.positions<-rbind(smiUnit.positions,station.positions.out)

      #compile station means
      smiUnit.stations<-rbind(smiUnit.stations, station.merge)

   }

  message("Saving output to .csv file.")
  write.csv(smiUnit.pin.slopes, file=paste(getwd(), "Results","SMI_unit_Results",smiUnitName, paste(smiUnitName,"pin-level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

  write.csv(smiUnit.positions, file=paste(getwd(), "Results","SMI_unit_Results",smiUnitName, paste(smiUnitName,"position","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

  #compile results for each refuge

  refuge.pins<-rbind(refuge.pins,smiUnit.pin.slopes)
  write.csv(refuge.pins, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Pin-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  refuge.positions<-rbind(refuge.positions,smiUnit.positions)
  write.csv(refuge.positions, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Position-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  refuge.stations<-rbind(refuge.stations, smiUnit.stations)
  write.csv(refuge.stations, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Station-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  #get refuge-level means
  refuge.slopes<-summaryFunction(DataIn=refuge.stations, factor="Unit_Code",response="mean")
  refuge.merge.1<-merge(refuge.slopes, dataCovs, by="Unit_Code")
  refuge.merge<-refuge.merge.1[,c("State","Unit_Code", "n","mean","var","SD","SE","CV","lwr","upr")]
  write.csv(refuge.merge, file=paste(getwd(), "Results","Refuge_Results",refugeName,paste(refugeName,"Refuge-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)

  }


  #compile all results
  all.pins<-rbind(all.pins, refuge.pins)

  all.positions<-rbind(all.positions, refuge.positions)

  all.stations<-rbind(all.stations, refuge.stations)

  all.refuges<-rbind(all.refuges,refuge.merge )

  }
  #save files
  write.csv(all.pins, file=paste(getwd(), "Results","Tables",paste("All","Pin-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.positions, file=paste(getwd(), "Results","Tables",paste("All","Position-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.stations, file=paste(getwd(), "Results","Tables",paste("All","Station-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.refuges, file=paste(getwd(), "Results","Tables",paste("All","Refuge-level","slopes.Obs.csv",sep="_"),sep="/"),row.names=FALSE)


  return(all.stations)
}
