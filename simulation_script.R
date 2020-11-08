#necessary packages to be installed
#install.packages("fitdistrplus")
#install.packages("actuar")

#Variable to study
consumptionData<-csv$yourVariableInStudy

#determine a parameters to weibull distribution
fw <- fitdist(consumptionData, "weibull")
summary(fw)
plot(fw)

#determine a parameters to gamma distribution 
fitg <- fitdist(consumptionData, "gamma")
summary(fitg)
plot(fitg)

#determine a parameters to a Log-Normal distribution
fitln <- fitdist(consumptionData, "lnorm")
summary(fitln)
plot(fitln)

#determine a parameters to a Log-logistic distribution
ft_llogis <- fitdist(consumptionData,"llogis")
summary(ft_llogis)
plot(ft_llogis)

#Create graphics
plot.legend <- c("Weibull", "Log-Normal", "Gamma","Log-logistic", "Normal")
par(mfrow = c(2, 2))
#Create a histogram plot with all distributions
denscomp(list(fw, fitln,fitg,ft_llogis), legendtext = plot.legend) 
#Create a qq plot with all distributions
qqcomp(list(fw, fitln,fitg, ft_llogis), legendtext = plot.legend) 
#Create a pp plot plot with all distributions
ppcomp(list(fw, fitln,fitg,ft_llogis), legendtext = plot.legend)

# Determination the goodness of fit statistics and goodness of fit criteria
gofstat(list(fw, fitg, fitln,ft_llogis))

#after obtaining the distribution parameters, data can be simulated
#simulation weibull distribution
simulationWeibull<-rweibull(n="number of observations to simulation", shape="value determined of fitfunction", scale = "value determined of fitfunction")
#simulation gamma distribution
simulationGamma<- rgamma(n="number of observations to simulation",  shape="value determined of fitfunction", rate = "value determined of fitfunction")
#simulation log-logistic distribution
simulationLogLogistic<- rllogis(n="number of observations to simulation", shape="value determined of fitfunction", scale = "value determined of fitfunction")
#simulation log-normal  distribution
simulationLogNormal<- rlnorm(n="number of observations to simulation", meanlog="value determined of fitfunction", sdlog = "value determined of fitfunction")


######example to create a multiple datasets with breaks
datasetSimulation<-(nrow=1:numberOfLinesTheDataset)
createAFile <- list.files(pattern="*.csv") 
file_list<-1
while (file_list<numberMaximumOfDatasets){
  for(i in datasetSimulation){
    #n corresponds to a maximum of lines that we want to simulate each break 
    datasetSimulation[1:n]<-rweibull(n="this number is determined per =n-1", shape=numberDetermined, scale = numberDetermined)
    #break 1 (repeat this process for the number of breaks you want to simulate)
    # when you want to simulate the breaks, for the shape and scale parameters the % that is desired in the break is decreased
    datasetSimulation[n1:n2]<-rweibull(n="this number is determined per =n2-n1", shape=breakApplication, scale = breakApplication)
    #break 2 (repeat this process for the number of breaks you want to simulate)
    datasetSimulation[n2:n3]<-rweibull(n="this number is determined per =n2-n1", shape=breakApplication, scale = breakApplication)
    #example to aplicate a breaks
    #the values obtain previously
    datasetSimulation[1:182]<-rweibull(n=183, shape=0.85, scale = 7.38)
    #break1 (example to decreasing 50% the parameters estimated) 
    datasetSimulation[184:365]<-rweibull(n=182, shape=0.425, scale = 3.68)
    
    mypath <- file.path("path to save a dataset",paste("name of file", file_list, ".csv"))
  }  
  write.csv2(datasetSimulation,  file = mypath)
  file_list<-file_list+1;
}
