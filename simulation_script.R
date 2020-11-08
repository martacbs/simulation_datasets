#necessary packages to be installed
#install.packages("fitdistrplus")
#install.packages("actuar")

#Variable to study
consumptionData<-csv$yourVariableInStudy

#estimate a parameters to weibull distribution
fw <- fitdist(consumptionData, "weibull")
#Summary of data the weibull distribution
summary(fw)
#Plot's to analyze the data to adequate a distribution (histogram,q-q plot, cdf's and p-p plot)
plot(fw)

#estimate a parameters to gamma distribution 
fitg <- fitdist(consumptionData, "gamma")
#Summary of data the gamma distribution
summary(fitg)
#Plot's to analyze the data to adequate a distribution (histogram,q-q plot, cdf's and p-p plot)
plot(fitg)

#estimate a parameterst o normal distribution
fdn <- fitdist(consumptionData, "norm")
#Summary of data the gamma distribution
summary(fdn)
#Plot's to analyze the data to adequate a distribution (histogram,q-q plot, cdf's and p-p plot)
plot(fdn)

#estimate a parameters to a Log-Normal distribution
fitln <- fitdist(consumptionData, "lnorm")
#Summary of data the Log-Normal distribution
summary(fitln)
#Plot's to analyze the data to adequate a distribution (histogram,q-q plot, cdf's and p-p plot)
plot(fitln)

ft_llogis <- fitdist(consumptionData,"llogis")
#Summary of data the log-logistic distribution
summary(ft_llogis)
#Plot's to analyze the data to adequate a distribution (histogram,q-q plot, cdf's and p-p plot)
plot(ft_llogis)

plot.legend <- c("Weibull", "Log-Normal", "Gamma","Log-logistic", "Normal")
par(mfrow = c(2, 2))
#Create a histogram plot with all distributions
denscomp(list(fw, fitln,fitg,ft_llogis, fdn), legendtext = plot.legend) 
#Create a qq plot with all distributions
qqcomp(list(fw, fitln,fitg, ft_llogis, fdn), legendtext = plot.legend) 
#Create a pp plot plot with all distributions
ppcomp(list(fw, fitln,fitg,ft_llogis,fdn), legendtext = plot.legend)

#Determination the goodness of fit statistics and goodness of fit criteria
gofstat(list(fw, fitg, fitln,ft_llogis,fdn))

#after obtaining the distribution parameters, we will simulate the data
#simulation weibull distribution
simulationWeibull<-rweibull(n="number of observations to simulation", shape="value determined of fitfunction", scale = "value determined of fitfunction")
#simulation gamma distribution
simulationGamma<- rgamma(n="number of observations to simulation",  shape="value determined of fitfunction", rate = "value determined of fitfunction")
#simulation log-logistic distribution
simulationLogLogistic<- rllogis(n="number of observations to simulation", shape="value determined of fitfunction", scale = "value determined of fitfunction")
#simulation log-normal  distribution
simulationLogNormal<- rlnorm(n="number of observations to simulation", meanlog="value determined of fitfunction", sdlog = "value determined of fitfunction")

#example to create a multiple datasets with breaks
datasetSimulation<-(nrow=1:numberOfLinesTheDataset)
createAFile <- list.files(pattern="*.csv") 
file_list<-1
while (file_list<numberMaximumOfDatasets){
  for(i in datasetSimulation){
    datasetSimulation[1:183]<-rweibull(n=183, shape=numberDetermined, scale = numberDetermined)
    #break 1
    datasetSimulation[184:365]<-rweibull(n=182, shape=breakApplication, scale = breakApplication)
    #break 2
    datasetSimulation[366:548]<-rweibull(n=183, shape=breakApplication, scale = breakApplication)
    #break 3
    datasetSimulation[549:730]<- rweibull(n=182, shape=breakApplication, scale = breakApplication)
    #break 4
    datasetSimulation[731:914]<- rweibull(n=183, shape=breakApplication, scale = breakApplication)
    mypath <- file.path("path to save a dataset",paste("name of file", file_list, ".csv"))
  }  
  write.csv2(datasetSimulation,  file = mypath)
  file_list<-file_list+1;
}
