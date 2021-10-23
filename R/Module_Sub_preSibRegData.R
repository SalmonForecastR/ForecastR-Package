#' get all possible sets of pooled sibreg predictor variables
#' @param x a continuous vector of age classes in the data set
#' @keyword Internal
#' @NoRd
getSets <- function(x){
# x is a vector of integers (strictly increasing, no gaps) with the ages in the data set (e.g. [2,3,4,5,6])

# Pooling 2
if(max(x)-min(x) >= 2 ){	pool2.ages <- (min(x)+2) : max(x)	}
if(max(x)-min(x) < 2 ){ pool2.ages <- NULL	}

# Pooling 3
if(max(x)-min(x) >= 3 ){	pool3.ages <- (min(x)+3) : max(x)	}
if(max(x)-min(x) < 3){ pool3.ages <- NULL	}

out.list <-list(pool2= pool2.ages,pool3= pool3.ages)

return(out.list)

}


#' generate single df with all individual age data and pooled sibreg inputs lined up by brood year
#' @param data.by.age  this is the $data element of the prepData() output,called tmpsub internally
#' @keyword Internal
#' @NoRd

prepSibRegData <- function(data.by.age){
# data.by.age = data element generated inside prepData()
# this fn also used inside prepData()

sibreg.src <- data.by.age %>% lapply(function(x){x[,grepl("Brood_Year|Age_",names(x))  ]}) %>%
		reduce(left_join, by = "Brood_Year") # using https://daranzolin.github.io/2016-12-10-join-list-dataframes/

sets.do <- getSets(sibreg.src %>% select(-Brood_Year) %>% names() %>% gsub("Age_","",.) %>% as.numeric())

# pool 2
if(!is.null(sets.do$pool2)){
		for(age.do in sets.do$pool2 ){
			ages.pool <- age.do - c(1,2)
			sibreg.src[[paste0("Pooled_",paste(ages.pool,collapse="_"))]] <- rowSums(sibreg.src[,paste0("Age_",ages.pool)])
		}  # end looping through ages
	} # end doing pool2

# pool 3
if(!is.null(sets.do$pool3)){
		for(age.do in sets.do$pool3 ){
			ages.pool <- age.do - c(1,2,3)
			sibreg.src[[paste0("Pooled_",paste(ages.pool,collapse="_"))]] <- rowSums(sibreg.src[,paste0("Age_",ages.pool)])
		}  # end looping through ages
	} # end doing pool3


return(sibreg.src)

}

