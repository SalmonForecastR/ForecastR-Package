# COmplex SibReg - Testing Wrapper Functions

library(forecastR)
library(tidyverse)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")

names(data.withage)
names(data.withage$data)
head(data.withage$data)
head(data.withage$sibreg.in)



#forecastR:::prepSibRegData(data.withage$data)

"SibRegComplex"


complex.sibreg.fitmodel.out <- fitModel(model= "SibRegComplex",
																			 data = data.withage$data,
																			 data.sibreg = data.withage$sibreg.in,
																			 settings = list(tol.AIC = 0.75, tol.r.sq = 0.02,incl.base.eq = FALSE),
																				# NOTE: adding base equation to settings argument inside fitModel(), b/c equation is specific to age class
																			 tracing=FALSE)


names(complex.sibreg.fitmodel.out)

complex.sibreg.fitmodel.out$`Age 5`$data


names(complex.sibreg.fitmodel.out$`Age 5`)
complex.sibreg.fitmodel.out$`Age 5`$formula
complex.sibreg.fitmodel.out$`Age 3`$formula
complex.sibreg.fitmodel.out$`Age 5`$model.selection

complex.sibreg.fitmodel.out[["Age 5"]]$model.selection

complex.sibreg.fitmodel.out$`Age 5`$sib.pred.used


names(complex.sibreg.fitmodel.out$`Age 5`$coefficients)
names(data.withage$data$`Age 4`)
names(data.withage$data$`Age 5`)

calcFC(fit.obj= complex.sibreg.fitmodel.out,
			 data = data.withage$data,
			 data.sibreg = data.withage$sibreg.in,
			 fc.yr= data.withage$specs$forecastingyear,
			 settings = pooled.logpower.sibreg.fitmodel.out$settings,
			 tracing=FALSE,
			 predictors = NULL,
			 covariates = data.withage$covariates )



calcFC(fit.obj= complex.sibreg.fitmodel.out,
			 data = data.withage$data,
			 data.sibreg = data.withage$sibreg.in,
			 fc.yr= data.withage$specs$forecastingyear-8,
			 settings = pooled.logpower.sibreg.fitmodel.out$settings,
			 tracing=FALSE,
			 predictors = NULL,
			 covariates = data.withage$covariates )




###############





test.df <- data.withage$data$`Age 6` %>%
						left_join(data.withage$data$`Age 5`  %>%
				     select(Brood_Year,all_of("Age_5")), by= "Brood_Year")
test.df
