# Complex SibReg DEV

library(tidyverse)
#library(meboot)
require(forecastR)

data.withage.raw <- read.csv("inst/extdata/FinalSampleFile_WithAge_exclTotal_covariates_Test.csv", stringsAsFactors = FALSE)
tail(data.withage.raw)

data.withage <- prepData(data.withage.raw,out.labels="v2")



names(data.withage$data)

test.df <- data.withage$data$"Age 4" %>% left_join(data.withage$data$"Age 3" %>% select(Brood_Year,Age_3), by= "Brood_Year")


#FN DEV


#--------------------------------------------------------------------------------
# DIY
# Note: https://stats.stackexchange.com/questions/19271/different-ways-to-write-interaction-terms-in-lm

settings.df <- data.frame(Label = c("None","Normalized","Standardized","Log","LogNormalized","LogStandardized"),
													type = c("none",  "normalize", "standardize","none",  "normalize", "standardize"),
													logfirst = c(FALSE,FALSE,FALSE,TRUE,TRUE,TRUE) )


# start looping through alternative transformations

for(j in 1:dim(settings.df)[1]){

	print("------------------------------------------------")
	print(settings.df$Label[j])
	print("------------------------------------------------")


test.df.use <- test.df



test.df.use[,grepl("Cov_",names(test.df.use))] <- transformDF(X = test.df.use[,grepl("Cov_",names(test.df.use))],
						 type = settings.df$type[j], logfirst = settings.df$logfirst[j])





# this will be inside the function call -------------------
X <- test.df.use
# print(X)

tol.AIC <- 0.75 # include in shortlist any with x% prob that this version will "minimize information loss" (i.e. best fit)
tol.r.sq <- 0.02  # include in shortlist any with adj.r.sq >=  max(adj.r.sq)
incl.base.eq <- TRUE

eq.base <- "Age_4 ~ -1 + Age_3"
covars.list <- names(X)[grepl("Cov_",names(X))]

ages.list <- names(X)[grepl("Age_",names(X))]

# expand.grid(covars.list,covars.list, stringsAsFactors = FALSE)

covars.combos <- c( covars.list, # individual covars
									 combn(covars.list,2) %>% apply(MARGIN = 2, paste,collapse = " + "), #pairs
									 combn(covars.list,2) %>% apply(MARGIN = 2, paste,collapse = " * "),  # pairs with interaction
									 combn(covars.list,3) %>% apply(MARGIN = 2, paste,collapse = " + "), # triples
									 paste("( ", combn(covars.list,3) %>% apply(MARGIN = 2, paste,collapse = " + ")," )^2")  # triples with pairwise interaction
										#paste(covars.list, collapse = " + "), # all covars additive
										#	paste(covars.list, collapse = " * ") # all covars with interaction term
										)
covars.combos

if(incl.base.eq){eq.list <- c(eq.base,paste(eq.base,covars.combos,sep=" + "))} # also include the "no cov" option in the candidate models
if(!incl.base.eq){eq.list <- paste(eq.base,covars.combos,sep=" + ")} # only include variations of the covriate model
eq.list


diy.out <- data.frame(ID = 1:length(eq.list), equ = eq.list, numCoeff = NA, adj.r.sq = NA, AIC = NA)

for(i in 1:length(eq.list)){
#print("------------------")
#print(eq.list[i])

fit.tmp <- lm(eq.list[i],  X)
fit.tmp
diy.out$adj.r.sq[i] <- round(summary(fit.tmp)$adj.r.squared,3)
diy.out$AIC[i] <- round(AIC(fit.tmp),3)
diy.out$numCoeff[i] <- length(fit.tmp$coefficients)


}

diy.out <- diy.out %>%
						mutate(diffAIC = min(AIC)-AIC) %>%
						mutate(probAIC = exp(diffAIC/2)) %>%
						mutate(rankAIC = rank(AIC,ties.method="min"),rankRsq = rank(-adj.r.sq,ties.method="min")) %>%
						mutate(shortAIC = probAIC >= tol.AIC, shortRsq = adj.r.sq >= (max(adj.r.sq) - tol.r.sq)  ) %>%
						mutate(shortBoth = shortAIC & shortRsq)

# REVISED!!!!!!!!!!!!!!!!!!!!!!
# if shortBoth = TRUE, then a model is on the shortlist for both AIC and r2
# 1) if have only 1 TRUE for shortBoth, pick that one
# 2) if have multiple TRUE in shortBoth, go by highest R2 (not min number coeff!)
# if have none in shortBoth, look at shortAIC = TRUE, and from that list pick the highest R2

diy.out$selected <- FALSE

# OLD
#select.idx <- diy.out$shortAIC & diy.out$numCoeff == min(diy.out$numCoeff[diy.out$shortAIC])
#if(sum(select.idx)>1){select.idx <- select.idx & diy.out$AIC == min(diy.out$AIC[select.idx]) }

# NEW
n.shortboth <- sum(diy.out$shortBoth)
#print(n.shortboth)

if(n.shortboth == 1){ diy.out$selected[diy.out$shortBoth] <- TRUE}

if(n.shortboth > 1){ diy.out$selected[diy.out$shortBoth & diy.out$rankRsq == min(diy.out$rankRsq[diy.out$shortBoth]) ] <- TRUE  }

if(n.shortboth == 0){diy.out$selected[diy.out$shortAIC & diy.out$rankRsq == min(diy.out$rankRsq[diy.out$shortAIC]) ] <- TRUE  }


#print(diy.out)

paste(paste(ages.list,collapse = ","), covars.combos[diy.out$selected],sep = ",")
diy.out$equ[diy.out$selected]

# end of function call ------------------------------------------------

if(j == 1) {

diy.summary <- data.frame(Label =  settings.df$Label[j],
													Type = settings.df$type[j],
													Log = settings.df$logfirst[j],
													diy.out)
}

if(j > 1) {

	diy.summary <- bind_rows(diy.summary,
													 data.frame(Label =  settings.df$Label[j],
														Type = settings.df$type[j],
														Log = settings.df$logfirst[j],
														diy.out)
														)
}


} # end looping through alt transforms



diy.summary <- diy.summary %>%
	mutate(diffAICAll = min(AIC)-AIC) %>%
	mutate(probAICAll = exp(diffAICAll/2)) %>%
	mutate(rankAICAll = rank(AIC,ties.method="min"),
				 rankRsqAll = rank(-adj.r.sq,ties.method="min")) %>%
	mutate(shortAICAll = probAICAll >= tol.AIC,
				 shortRsqAll = adj.r.sq >= (max(adj.r.sq) - tol.r.sq)  )



write.csv(diy.summary ,"ComplexSib_SampleOutput.csv",row.names=FALSE)


#################
# FUNCTION TESTING

"SibRegComplex"

complex.fit <- forecastR:::estimation.functions$SibRegComplex$estimator(X = test.df.use,
																												 settings= list(tol.AIC = 0.75,
																												 tol.r.sq = 0.02, base.eq = "Age_4 ~ -1 + Age_3",
																												 incl.base.eq = TRUE)
																												 )


complex.fit


forecastR:::estimation.functions$SibRegComplex$pt.fc(
	fit.obj = complex.fit ,
	data = test.df.use,
	settings = NULL)



#--------------------------------------------------------------------------------
# STepAIC
# Examples from https://stat.ethz.ch/R-manual/R-patched/library/MASS/html/stepAIC.html


# Example 1
library(MASS)
cpus1 <- cpus
for(v in names(cpus)[2:7])
	cpus1[[v]] <- cut(cpus[[v]], unique(quantile(cpus[[v]])),
										include.lowest = TRUE)
cpus1
cpus0 <- cpus1[, 2:8]  # excludes names, authors' predictions
cpus.samp <- sample(1:209, 100)
cpus.lm <- lm(log10(perf) ~ ., data = cpus1[cpus.samp,2:8])
cpus.lm2 <- stepAIC(cpus.lm, trace = FALSE)
cpus.lm2$anova

# Examnple 2
quine.hi <- aov(log(Days + 2.5) ~ .^4, quine)
quine.nxt <- update(quine.hi, . ~ . - Eth:Sex:Age:Lrn)
quine.stp <- stepAIC(quine.nxt,
										 scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
										 trace = FALSE)
quine.stp$anova




