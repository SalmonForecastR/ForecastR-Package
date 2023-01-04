# INITIAL DEVELOPMENT OF COVAR FOR TOTAL ABD ()

library(forecastR)
library(tidyverse)

#data.noagewithcovar.raw <- read.csv("NoAge_Covar_Test_INPUT.csv", stringsAsFactors = FALSE)
data.noagewithcovar.raw <- read.csv("NoAge_Covar_Test_INPUT_Fixed_Covar_Labels.csv", stringsAsFactors = FALSE)

head(data.noagewithcovar.raw)

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

fit1 <- glm(Total ~  Cov_npgosum2 + Cov_EV + Cov_epnpsum2, data = data.noagewithcovar$data$Total, family = "poisson")
summary(fit1)
sort(names(summary(fit1)))

summary(fit1)$adj.r.sq
summary(fit1)$aic


data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022)


fc.fit1 <- predict.glm(object = fit1, newdata = data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022), type = "response")
fc.fit1



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


tmp.fitmodel <- fitModel(model= "Naive",
													data = data.noagewithcovar$data,
													data.sibreg = NULL,
													settings = list(avg.yrs = 3),
													tracing=FALSE)
tmp.fitmodel



test.fitmodel <- fitModel(model= "NoAgeCovar",
										 data = data.noagewithcovar$data,
										 data.sibreg = NULL,
										 settings = list(glm.family = "poisson",
										 								tol.AIC = 0.75,
										 								tol.r.sq = 0.02,
										 								base.eq ="Total ~"),
										 tracing=FALSE)




forecastR:::sub.fcdata



