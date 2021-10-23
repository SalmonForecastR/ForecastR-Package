
#' @title apply the transformVar() function to all numeric columns in a data frame
#'
#' @param X is a data frame to transform using \code{\link{transformVar}}.
#' @param type "none", "standardize" (rescale to 0-1), or "normalize" (rescale to mean=0, sd =1)
#' @param logfirst  if TRUE, do a log-transform BEFORE applying either standardize or normalize
#'
#' @details If any zeroes in the data, then the log-transform causes problems. 
#' Using the strategy from Perry et al 2021 
#' \link{(https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0245941)}
#' which replaces 0 with a random value greater than 0 and less than one-half of the
#' lowest non-zero value in the data for that variable. If the data has negative values, a 
#' constant equal to min(x)* 1.01 is added to all values (i.e. smallest value is a 
#' bit larger than 0)
#'
#' @return A data frame with numeric columns transformed
#' @export
#'
#' @examples
#' transformDF(X = mtcars, type = "standardize", logfirst = FALSE)
#' transformDF(X = mtcars, type = "normalize", logfirst = TRUE)

transformDF <- function(X,type,logfirst = FALSE){
# 
# this function 
# note: applying the same arguments to each col

X.out <- X
num.idx <- unlist(lapply(X, is.numeric))
#print("---------")
#print(X)
#print(str(X))
#print(num.idx)

X.out[,num.idx] <- lapply(X.out,transformVar,
							 type = type,
							 logfirst = logfirst,
							 warning.print = FALSE)
return(X.out)

}




#' @title transform a numeric vector
#'
#' @param x is a numeric vector
#' @param type "none", "standardize" (rescale to 0-1), or "normalize" (rescale to mean=0, sd =1)
#' @param logfirst  if TRUE, do a log-transform BEFORE applying either standardize or normalize
#' @param warning.print  if TRUE print a warning if Perry et al 2021 0 fix is triggered
#' 
#' @details Typically called from within  \code{\link{transformDF}}. If any zeroes in the data,
#' then the log-transform causes problems. 
#' Using the strategy from Perry et al 2021 
#' \link{(https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0245941)}
#' which replaces 0 with a random value greater than 0 and less than one-half of the
#' lowest non-zero value in the data for that variable. If the data has negative values, a 
#' constant equal to min(x)* 1.01 is added to all values (i.e. smallest value is a 
#' bit larger than 0).
#'
#'
#' @return A transformed vector
#' @export
#'
#' @examples
#' transformVar(X = mtcars$mpg, type = "standardize", logfirst = FALSE)
#' transformVarX = mtcars$mpg, type = "normalize", logfirst = TRUE)

transformVar <- function(x,type,logfirst = FALSE, warning.print = FALSE){


x.use <- x

if(logfirst){
	
	neg.flag <- min(x, na.rm=TRUE) < 0
	if(neg.flag){ x.use <- x - min(x, na.rm=TRUE) *1.01}
	
	
	zero.idx <- x.use == 0
	zero.idx[is.na(zero.idx)] <- FALSE
	if(sum(zero.idx) > 0 & warning.print) {warning(paste0(sum(zero.idx)," records of 0 were replaced with a random value greater than 0 and less than one-half of the lowest non-zero value in the data for that variable, as per Perry et al 2021"))}
  x.use[zero.idx] <- runif(sum(zero.idx,na.rm = TRUE),0.00000001, min(x.use[!zero.idx],na.rm = TRUE)/2)
  x.use <- log(x.use)
	} # end if logfirst



if(type == "normalize"){
# convert to [0,1]
	x.use <- (x.use - min(x.use, na.rm=TRUE))/(max(x.use, na.rm=TRUE) - min(x.use, na.rm=TRUE))
	}


if(type == "standardize"){
	# convert to mean = 0, sd = 1
	x.use <- (x.use - mean(x.use, na.rm=TRUE))/sd(x.use,na.rm=TRUE)
}

if(type == "none"){
	# only do the log above (if logfirst = TRUE)
	x.use <- x.use
}



x.out <- x.use
return(x.out)

} # end transformVar
