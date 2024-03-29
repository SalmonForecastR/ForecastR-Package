# INITIAL DEVELOPMENT OF COVAR FOR TOTAL ABD ()

library(forecastR)
library(tidyverse)

#data.noagewithcovar.raw <- read.csv("NoAge_Covar_Test_INPUT.csv", stringsAsFactors = FALSE)
data.noagewithcovar.raw <- read.csv("NoAge_Covar_Test_INPUT_Fixed_Covar_Labels.csv", stringsAsFactors = FALSE)

head(data.noagewithcovar.raw)
data.noagewithcovar.raw$Run_Year




data.noagewithcovar <- prepData(data.noagewithcovar.raw,out.labels="v2")

names(data.noagewithcovar)
names(data.noagewithcovar$data)
head(data.noagewithcovar$data)
data.noagewithcovar$data$Total
data.noagewithcovar$covariates
!is.null(data.noagewithcovar$covariates)

data.noagewithcovar$data$Total


names(data.noagewithcovar$covariates)[-1]


data.noagenocovar <- prepData(data.noagewithcovar.raw %>% select(-starts_with("Cov_")),out.labels="v2")
names(data.noagenocovar )
data.noagenocovar$covariates
!is.null(data.noagenocovar$covariates)


fc.yr <- data.noagewithcovar$specs$forecastingyear

fit1 <- glm(Total ~  Cov_npgosum2 + Cov_EV + Cov_epnpsum2,
						data = data.noagewithcovar$data$Total,
						family = "poisson")
						#family = poisson(link = "log")) seems to give identical results
summary(fit1)
sort(names(summary(fit1)))

summary(fit1)$adj.r.sq
summary(fit1)$aic
sort(names(fit1))

data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022)


# testing ciTools pkg
library(ciTools)
?add_ci

df <- data.noagewithcovar$data$Total %>% select(Run_Year,Total,Cov_npgosum2,Cov_EV,Cov_epnpsum2)

df_ints <- df %>% add_ci(fit = fit1, names = c("lwr", "upper"), alpha = 0.1)

df_ints





fc.fit1 <- predict.glm(object = fit1,
											 newdata = data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022),
											 type = "response",
											 se.fit = TRUE)
#fc.fit1
#fc.fit1$fit + fc.fit1$se.fit
#confint(fit1,level =0.8)

df_ints <- data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022) %>%
	add_ci(fit = fit1, names = c("lwr", "upr"), alpha = 0.2) %>%
	dplyr::rename(fit = pred)

df_ints %>% select(fit, lwr,upr) %>% unlist()




# manual calc check


# coeff
7.990486 # int
0.032722 # Cov_npgo_sum_2
0.846238 # Cov_EV
0.016200 # Cov_epnp_sum_2

# values for 2022 (RUN YEAR? VERIFY HOW HANDLED FOR OTHER COVAR MODELS!!)
#Run_Year Cov_npgo_sum_2    Cov_EV						Cov_epnp_sum_2
#2022      -2.106708				0.4154066         -0.104


fc.2022 <- exp(7.990486 + 0.032722 * -2.106708 + 0.846238 * 0.4154066 +  0.016200  * -0.104)
fc.2022

par(mfrow=c(2,2))


plot(data.noagewithcovar$data$Total$Run_Year,data.noagewithcovar$data$Total$Total,
		 bty="n", xlab="Run Year", ylab="Avg Esc", type="o",pch=19, col="darkblue", xlim=c(1990,2025))
points(2022,fc.2022,pch=18, col="red", cex=2)
abline(v=2022,col="red",lty=2)


for(i in 2:4){

plot(data.noagewithcovar$covariates[,1],data.noagewithcovar$covariates[,i],bty="n",pch=19,type="o",
		 main = names(data.noagewithcovar$covariates[i]),xlim=c(1990,2025),
		 xlab="Run Year", ylab="Covar Value")
	abline(v=2022,col="red",lty=2)

}






######################################################################
library(tidyverse)

# Just checking the alt model form generation
test.list <- c("a","b","c")
base.eq <- TRUE

covars.combos <- test.list # individual covars

if(length(test.list)>=2){
	covars.combos <- c(covars.combos,
										 combn(test.list,2) %>% apply(MARGIN = 2, paste,collapse = " + "), #pairs
										 combn(test.list,2) %>% apply(MARGIN = 2, paste,collapse = " * ")  # pairs with interaction
	)
}

if(length(test.list)>=3){
	covars.combos <- c(covars.combos,
										 combn(test.list,3) %>% apply(MARGIN = 2, paste,collapse = " + "), # triples
										 paste("( ", combn(test.list,3) %>% apply(MARGIN = 2, paste,collapse = " + ")," )^2")  # triples with pairwise interaction
	)
}




if(base.eq){eq.list <- c("Age_4 ~ -1 + Age_3",paste("Age_4 ~ -1 + Age_3",covars.combos,sep=" + "))} # also include the "no cov" option in the candidate models
if(!base.eq){eq.list <- paste("Age_4 ~ -1 + Age_3",covars.combos,sep=" + ")} # only include variations of the covariate model


eq.list



################################################################################################################
# FUNCTION CHECKING
library(tidyverse)
# data check

forecastR:::noage.covar.datacheck
forecastR:::noage.covar.datacheck(data.noagewithcovar,tracing = TRUE)



# model fit (incl model selection)

forecastR:::noage.covar.est


test.fit <- forecastR:::noage.covar.est(data.noagewithcovar$data$Total,
																				settings = list(glm.family = "poisson",
																							 tol.AIC = 0.75,
																							 tol.r.sq = 0.02,
																							 base.eq ="Total ~"),
														tracing=FALSE)


test.fit





##################################
# FitModel


test.fitmodel <- fitModel(model= "NoAgeCovar",
										 data = data.noagewithcovar$data,
										 data.sibreg = NULL,
										 settings = list(glm.family = "poisson",
										 								tol.AIC = 0.75,
										 								tol.r.sq = 0.02,
										 								base.eq ="Total ~"),
										 tracing=FALSE)


test.fitmodel
sort(names(test.fitmodel))
sort(names(test.fitmodel$Total))
test.fitmodel$Total$model.selection




#####################################
# calcFC


names(data.noagewithcovar$data)
data.noagewithcovar$data[["Total"]]

sort(names(fit1))
sort(names(test.fitmodel$Total))
test.fitmodel$Total$family

str(test.fitmodel$Total)

list.combo <- c(list(model.type = "NoAgeCovar",
										 formula = "eq.use",
										 var.names = paste("Total", "Text",sep = ","),
										 est.fn = paste("glm() with family=","Text")),
								list(glm.obj = test.fitmodel$Total) )

names(list.combo)
list.combo



predict.glm(object = fit1,
											 newdata = data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022),
											 type = "response")

predict.glm(object = test.fitmodel$Total$glm.obj,
						newdata = data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022),
						type = "response")
sort(names(test.fitmodel$Total$glm.obj))

names(test.fitmodel[["Total"]]$glm.obj) # $coefficients)


test.fc.out <- calcFC(fit.obj= test.fitmodel,
			 data = data.noagewithcovar$data,
			 data.sibreg = NULL,
			 fc.yr= data.noagewithcovar$specs$forecastingyear,
			 settings = test.fitmodel$settings,
			 tracing=FALSE,
			 predictors = NULL,
			 covariates = data.noagewithcovar$covariates )


test.fc.out




####################################################################
# multiFit Testing



settings.use <- list(Naive1 = list(model.type="Naive",settings=list(avg.yrs=1)),
										 Naive3 = list(model.type="Naive",settings=list(avg.yrs=3)),
										 Naive5 = list(model.type="Naive",settings=list(avg.yrs=5)),
										 NoAgeCovar = list(model.type = "NoAgeCovar",
										 									settings = list(glm.family = "poisson",	tol.AIC = 0.75,	tol.r.sq = 0.02,
										 									base.eq ="Total ~")	)
											)


settings.use.naiveonly <- list(Naive1 = list(model.type="Naive",settings=list(avg.yrs=1)),
										 Naive3 = list(model.type="Naive",settings=list(avg.yrs=3)),
										 Naive5 = list(model.type="Naive",settings=list(avg.yrs=5))
										 )





#######
# POINT FORECAST TESTING



multiresults.ptfconly <- multiFC(data.file=data.noagewithcovar.raw , # remember that prepData happens inside of multiFC
																 settings.list=settings.use,
																 do.retro=FALSE,retro.min.yrs=15,
																 out.type="short",
																 int.type = "None", int.n = 100,
																 boot.type = "meboot",
																 tracing=TRUE)

multiresults.ptfconly



#######################
# RETROSPECTIVE INTERVAL TESTING


multiresults.retro <- multiFC(data.file=data.noagewithcovar.raw,settings.list=settings.use,
															do.retro=TRUE,retro.min.yrs=15,
															out.type="short",
															int.type = "Retrospective", int.n = 100,
															boot.type = "meboot",
															tracing=TRUE)

multiresults.retro


#######################
# BOOTSTRAP INTERVAL TESTING

multiresults.boot<- multiFC(data.file=data.noagewithcovar.raw,settings.list=settings.use,
															 do.retro=FALSE,retro.min.yrs=15,
															 out.type="short",
															 int.type = "Bootstrap", int.n = 100,
															 boot.type = "meboot",
															 tracing=TRUE)
multiresults.boot



#######################
# PREDICTION INTERVAL TESTING


# these may not be possible / or comparable:
# https://stackoverflow.com/questions/14423325/confidence-intervals-for-predictions-from-logistic-regression


multiresults.predint<- multiFC(data.file=data.noagewithcovar.raw,settings.list=settings.use,
															 do.retro=FALSE,retro.min.yrs=15,
															 out.type="short",
															 int.type = "Prediction", int.n = 100,
															 boot.type = "meboot",
															 tracing=TRUE)


multiresults.predint




