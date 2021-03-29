#function to run JAGS model
runJAGSmodelYearVisitTest<-function(dataIn, modelIn){
  modelIn="modelNoObs.txt"
  new.data<-sub.data.2

  stationName<-as.character(unique(new.data$Plot_Name))

  posList<-sort(unique(as.character(new.data$Position_Name)))

  for(k in 1:length(posList)){

    pos.data<-subset(new.data, Position_Name==posList[1])

  #number of rows of data
  Ndata<-nrow(pos.data)

  #number of Stations
  stations<-as.integer(factor(pos.data[,"Plot_Name"], levels=unique(pos.data[,"Plot_Name"])))
  Nstations<-length(unique(stations))

  #number of within-year sites (visits)
  visits<-as.integer(factor(pos.data[,"Visit"], levels=unique(pos.data[,"Visit"])))
  Nvisits<-length(unique(visits))

  #number of positions (n=4)
  pos<-as.integer(factor(pos.data[,"Position_Name"], levels=unique(pos.data[,"Position_Name"])))
  Npos<-length(unique(pos))

  #number of pins per position (n=9)
  pins<-as.integer(factor(pos.data[,"variable"], levels=unique(pos.data[,"variable"])))
  Npins<-length(unique(pins))

  #number of pos.pin (n=36)
  pos.pins<-as.integer(factor(pos.data[,"pos.pin"], levels=unique(pos.data[,"pos.pin"])))
  Npos.pins<-length(unique(pos.pins))

  #observer
  obs<-as.integer( factor( pos.data[,"Last_Name"] ,levels=unique(pos.data[,"Last_Name"])))

  #data (x=years, y=delta SET values)
  #x=as.numeric(pos.data[,"Year"])
  x=as.numeric(pos.data[,"year.visit"]) #try with year.visit
  y=as.numeric(pos.data[,"value"])

  ###############################################################################################################################
  # Standardize data (divide by SD) to make initialization easier.

  # xM = mean( x )
  # xSD = sd( x )
  # yM = mean( y )
  # ySD = sd( y )
  # zx = ( x - xM ) / xSD
  # zy = ( y - yM ) / ySD

  # Specify data, as a list.
  dataList = list(
    Ndata = Ndata,
    Nstations = Nstations,
    stations = stations,
    Npos = Npos,
    pos = pos,
    Npins = Npins,
    pins = pos.pins,
    obs=obs,
    x = x ,
    y = y
  )

  ###############################

  params = c("station_mu","station_tau","position_mu","position_tau","b0","b1","tau" , "b0_mu","b0_tau", "b1_mu","b1_tau","m","d")
  adaptSteps = 500              # Number of steps to "tune" the samplers.
  burnInSteps = 2000            # Number of steps to "burn-in" the samplers.
  nChains = 3                   # Number of chains to run.
  numSavedSteps=30000           # Total number of steps in chains to save.
  thinSteps=1                   # Number of steps to "thin" (1=keep every step).
  nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
  # Create, initialize, and adapt the model:
  jagsModel=NULL

  jagsModel = jags.model( modelIn , data=dataList , inits=NULL ,
                          n.chains=nChains , n.adapt=adaptSteps )

  # Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )

  # The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  out = coda.samples( jagsModel , variable.names=params ,
                      n.iter=nPerChain , thin=thinSteps )


  return(out)
  }
}