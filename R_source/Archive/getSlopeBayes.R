getSlopeBayesNoObs<-function(DataIn){
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
        #get positionList
        positionList<-unique(sort(as.character(new.data$Position_Name)))

        #get pospinList
        pospinList<-unique(sort(as.character(new.data$pos.pin)))

        #get covariates
        stationCovs<-unique(new.data[,c("State","Unit_Code",
                                        "Site_Name","Plot_Name","Lat","Long")])

        positionCovs<-unique(new.data[,c("State","Unit_Code","Site_Name","Plot_Name","Position_Name","Lat","Long")])

       pos.pinCovs<-unique(new.data[,c("State","Unit_Code","Site_Name","Plot_Name","Position_Name","variable","pos.pin","Lat","Long")])

        regResults.out<-list()
        # for(k in 1:length(pospinList)){
        #   sub.data<-new.data
        #   sub.data<-subset(new.data, Position_Name==positionList[k])
        #   sub.data$Last_Name<-as.factor(as.character(sub.data$Last_Name))
        #   sub.data$year.visit<-as.integer(as.factor(sub.data$year.visit))
        #
        #   sub.dataCovs<-unique(sub.data[,c("State","Unit_Code",
        #                                    "Site_Name","Plot_Name","Position_Name","variable","Lat","Long","pos.pin")])


          #runJAGSmodel function
          out<-tryCatch({runJAGSmodelYearVisit(dataIn=new.data, modelIn="modelTest.txt")
          },error=function(cond2){
            cond2=NA
          })

          #save MCMC output
          try(dput(out,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"NoObs.mcmc.output.R",sep="."), sep="/")))

          message("Compiling JAGS MCMC output, and getting posterior summary statistics.")

          #get summary of output
          new.bayes<-summary(out)
          as.data.frame(new.bayes$statistics)

          #save summary of mcmc chains
          try(dput(new.bayes,file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"NoObs.summary.output.R",sep="."), sep="/")))

          #compile results (pin-level slopes)

          #get summary of intercepts (b0)
          pos.pin.intercepts<-new.bayes$statistics[1:36,]
          #get summary of slopes (b1)
          pos.pin.slopes<-new.bayes$statistics[39:74,]

          #combine summary of b0 and b1
          pos.pin.out<-data.frame(Slope=pos.pin.slopes[,1],Intercept=pos.pin.intercepts[,1])

          #add pin.pos covs
          pos.pin.out.1<-data.frame(pos.pinCovs,pos.pin.out)

          #compile results (position-level slopes)
          position.means<-new.bayes$statistics[77:80,]
          position.out<-data.frame(positionCovs, position.means)


          #compile results (station-level slopes)
          station.mean<-new.bayes$statistics[89,]

          station.out<-data.frame(stationCovs, t(station.mean))
        }

        #save station results
        write.csv(pos.pin.out.1, file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"Bayes_Pin_level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)


        #save compiled Position-level slopes
        write.csv(position.out, file=paste(getwd(),"Results","Results_Bayes","Station_Results",stationName,paste(stationName,"Bayes_Position_level_slopes.csv",sep="_"),sep="/"),row.names=FALSE)

        write.csv(station.out, file=paste(getwd(), "Results","Results_Bayes","Station_Results",stationName,paste(stationName,"Bayes_Station_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)


        #compile pin-level slopes for each smiUnit
        smiUnit.pin.slopes<-rbind(smiUnit.pin.slopes,pos.pin.out.1)

        #compile position means
        smiUnit.positions<-rbind(smiUnit.positions,position.out)

        #compile station means
        smiUnit.stations<-rbind(smiUnit.stations, station.out)

      }

      message("Saving output to .csv file.")
      write.csv(smiUnit.pin.slopes, file=paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",smiUnitName, paste(smiUnitName,"Bayes_Pin_level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

      write.csv(smiUnit.positions, file=paste(getwd(), "Results","Results_Bayes","SMI_unit_Results",smiUnitName, paste(smiUnitName,"Bayes_Position_level","slopes.pins.out.csv",sep="_"),sep="/"),row.names=FALSE)

      #compile results for each refuge

      refuge.pins<-rbind(refuge.pins,smiUnit.pin.slopes)
      write.csv(refuge.pins, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Pin_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)

      refuge.positions<-rbind(refuge.positions,smiUnit.positions)
      write.csv(refuge.positions, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Position_level_slopes.csv",sep="_"),sep="/"),row.names=FALSE)

      refuge.stations<-rbind(refuge.stations, smiUnit.stations)
      write.csv(refuge.stations, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Station_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)

      #get refuge-level means
      refuge.slopes<-summaryFunction(DataIn=refuge.stations, factor="Unit_Code",response="mean")
      refuge.merge.1<-merge(refuge.slopes, dataCovs, by="Unit_Code")
      refuge.merge<-refuge.merge.1[,c("State","Unit_Code", "n","mean","var","SD","SE","CV","lwr","upr")]
      write.csv(refuge.merge, file=paste(getwd(), "Results","Results_Bayes","Refuge_Results",refugeName,paste(refugeName,"Bayes_Refuge_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)

    }


    #compile all results
    all.pins<-rbind(all.pins, refuge.pins)

    all.positions<-rbind(all.positions, refuge.positions)

    all.stations<-rbind(all.stations, refuge.stations)

    all.refuges<-rbind(all.refuges,refuge.merge )

  }
  #save files
  write.csv(all.pins, file=paste(getwd(), "Results","Tables",paste("All","Bayes_Pin_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.positions, file=paste(getwd(), "Results","Tables",paste("All","Bayes_Position_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.stations, file=paste(getwd(), "Results","Tables",paste("All","Bayes_Station-level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)
  write.csv(all.refuges, file=paste(getwd(), "Results","Tables",paste("All","Bayes_Refuge_level","slopes.csv",sep="_"),sep="/"),row.names=FALSE)


  return(all.stations)
}



###########
#test
test=as.numeric(list(3.293112,-10.03286, -15.09080, 10.96627))
mean(test)
