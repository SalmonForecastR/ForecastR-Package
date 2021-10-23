# Pooled SibReg DEV

library(tidyverse)
#library(meboot)
require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")



names(data.withage$data)
data.withage$data$"Age 4" %>% left_join(data.withage$data$"Age 3" %>% select(Brood_Year,Age_3), by= "Brood_Year")

tmp <- data.withage$data$"Age 4"









##################
# TESTING



# internal functions
forecastR:::getSets(3:6)
forecastR:::getSets(5:6)
forecastR:::getSets(2:8)

sibreg.df <- forecastR:::prepSibRegData(data.by.age = data.withage$data)
sibreg.df
write.csv(sibreg.df,"sibreg.prep.out.csv",row.names = FALSE)

# augmented prepData()
data.withage <- prepData(data.withage.raw,out.labels="v2")
names(data.withage)
data.withage$sibreg.in
names(data.withage$data)
data.withage$data$"Age 3"



# SIMPLE SIBREG (Not Pooled vs Pooled) --------------------------------------------------------------------------

# fit the simple model (revised calc for simple sibreg using new data object)

simple.sibreg.fitmodel.out <- fitModel(model= "SibRegSimple",
															data = data.withage$data,
															data.sibreg = data.withage$sibreg.in,
															settings = NULL,
															tracing=TRUE)

names(simple.sibreg.fitmodel.out)
simple.sibreg.fitmodel.out$settings
simple.sibreg.fitmodel.out$"Age 3"
simple.sibreg.fitmodel.out$"Age 4"
simple.sibreg.fitmodel.out$"Age 5"


# fit the pooled model (revised calc for simple sibreg using new data object)


pooled.sibreg.fitmodel.out <- fitModel(model= "SibRegPooledSimple",
																			 data = data.withage$data,
																			 data.sibreg = data.withage$sibreg.in,
																			 settings = list(max.pool = 3),
																			 tracing=TRUE)

names(pooled.sibreg.fitmodel.out)
pooled.sibreg.fitmodel.out$settings
pooled.sibreg.fitmodel.out$"Age 4"
pooled.sibreg.fitmodel.out$"Age 5"
pooled.sibreg.fitmodel.out$"Age 6"


# SIMPLE SIBREG (Not Pooled vs Pooled) --------------------------------------------------------------------------


# fit the regular logpower sibreg model (using new data object)

logpower.sibreg.fitmodel.out <- fitModel(model= "SibRegLogPower",
																			 data = data.withage$data,
																			 data.sibreg = data.withage$sibreg.in,
																			 settings = NULL,
																			 tracing=TRUE)

names(logpower.sibreg.fitmodel.out)
logpower.sibreg.fitmodel.out$"Age 3"
logpower.sibreg.fitmodel.out$"Age 4"
logpower.sibreg.fitmodel.out$"Age 5"


# fit the pooled model (revised calc for simple sibreg using new data object)


pooled.logpower.sibreg.fitmodel.out <- fitModel(model= "SibRegPooledSimple",
																			 data = data.withage$data,
																			 data.sibreg = data.withage$sibreg.in,
																			 settings = list(max.pool = 3),
																			 tracing=TRUE)

names(pooled.logpower.sibreg.fitmodel.out)
pooled.logpower.sibreg.fitmodel.out$"Age 4"
pooled.logpower.sibreg.fitmodel.out$"Age 5"
pooled.logpower.sibreg.fitmodel.out$"Age 6"




## CalcFC fn testing

data.withage$specs$forecastingyear
data.withage$sibreg.in

forecastR:::sub.fcdata(fit = pooled.logpower.sibreg.fitmodel.out  ,
					 data = data.withage$data,
					 data.sibreg = data.withage$sibreg.in,
					 fc.yr = data.withage$specs$forecastingyear,
					 pred = NULL, cov = NULL)


forecastR:::sub.pt.fc(fit = pooled.logpower.sibreg.fitmodel.out,
											data.source = data.withage$data,
											data.sibreg = data.withage$sibreg.in,
											fc.yr = data.withage$specs$forecastingyear,
											fit.settings = NULL,
											pred. = NULL, cov.= NULL)


calcFC(fit.obj= pooled.logpower.sibreg.fitmodel.out,
			 data = data.withage$data,
			 data.sibreg = data.withage$sibreg.in,
			 fc.yr= data.withage$specs$forecastingyear,
			 settings = pooled.logpower.sibreg.fitmodel.out$settings,
			 tracing=FALSE,
			 predictors = NULL,
			 covariates = NULL )



