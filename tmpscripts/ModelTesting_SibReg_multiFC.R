####################################################################
# multiFit Testing for SibReg Variations
###################################################################

library(forecastR)
library(dplyr)
library(purrr)
library(tidyr)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")


settings.use <- list(Naive1 = list(model.type="Naive",settings=list(avg.yrs=1)),
										 Naive3 = list(model.type="Naive",settings=list(avg.yrs=3)),
										 Naive5 = list(model.type="Naive",settings=list(avg.yrs=5)),
										 SibRegSimple = list(model.type="SibRegSimple",settings=NULL),
										 SibRegLogPower =  list(model.type="SibRegLogPower",settings=NULL),
										 SibRegPooledSimple =  list(model.type="SibRegPooledSimple",settings=list(max.pool = 3)),
										 SibRegPooledLogPower =  list(model.type="SibRegPooledLogPower",settings=list(max.pool = 3)),
										 SibRegComplex =  list(model.type="SibRegComplex",settings=list(tol.AIC = 0.75, tol.r.sq = 0.02,incl.base.eq = FALSE))
										 )


multiresults.ptfconly <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
																 do.retro=FALSE,retro.min.yrs=15,
																 out.type="short",
																 int.type = "None", int.n = 100,
																 boot.type = "meboot",
																 tracing=TRUE)

multiresults.ptfconly


multiresults.predint<- multiFC(data.file=data.withage.raw,settings.list=settings.use,
															 do.retro=FALSE,retro.min.yrs=15,
															 out.type="short",
															 int.type = "Prediction", int.n = 100,
															 boot.type = "meboot",
															 tracing=TRUE)
multiresults.predint


multiresults.retro <- multiFC(data.file=data.withage.raw,settings.list=settings.use,
															 do.retro=TRUE,retro.min.yrs=15,
															 out.type="short",
															 int.type = "Retrospective", int.n = 100,
															 boot.type = "meboot",
															 tracing=TRUE)

multiresults.retro





#####################

library(forecastR)
library(dplyr)
library(purrr)
library(tidyr)


data.withage$sibreg.in


test <- bootSeries(series = na.omit(data.withage$sibreg.in$Pooled_5_4), boot.type = "meboot", boot.n = 10 , plot.diagnostics = FALSE,plot.type="sample" )
dim(test$series.boot)



boot.test <- doBoot(data= data.withage , args.fitmodel= list(model= "SibRegSimple",settings=NULL),
			 args.calcfc = list(fc.yr= data.withage$specs$forecastingyear,  settings = NULL),
			 args.boot = list(boot.type="meboot", boot.n=100, plot.diagnostics=FALSE),  full.out = TRUE, plot.out=FALSE)








multiresults.boot<- multiFC(data.file=data.withage.raw,settings.list=settings.use,
														do.retro=FALSE,retro.min.yrs=15,
														out.type="short",
														int.type = "Bootstrap", int.n = 100,
														boot.type = "meboot",
														tracing=TRUE)
