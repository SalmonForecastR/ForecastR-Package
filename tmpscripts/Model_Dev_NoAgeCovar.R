# INITIAL DEVELOPMENT OF COVAR FOR TOTAL ABD ()

library(forecastR)
library(tidyverse)

data.noagewithcovar.raw <- read.csv("NoAge_Covar_Test_INPUT.csv", stringsAsFactors = FALSE)
head(data.withage.raw)

data.noagewithcovar <- prepData(data.noagewithcovar.raw,out.labels="v2")

names(data.noagewithcovar)
names(data.noagewithcovar$data)
head(data.noagewithcovar$data)
data.noagewithcovar$covariates

fc.yr <- data.noagewithcovar$specs$forecastingyear

fit1 <- glm(Total ~  Cov_npgo_sum_2 + Cov_EV + Cov_epnp_sum_2, data = data.noagewithcovar$data$Total, family = "poisson")
summary(fit1)


data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022)


fc.fit1 <- predict(object = fit1, newdata = data.noagewithcovar$covariates %>% dplyr::filter(Run_Year == 2022), type = "response")
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
