library(tidyverse)
library(data.table)

na_introduce <- function(x, prop = 0.05){
	if(is.vector(x) & !is.list(x)){
		na_place <- sample(x = 1:length(x), size = floor(length(x)*prop), replace = FALSE)
		x[na_place] <- NA
		return(x)

	}else if(is.data.frame(x)){
		df <- as.data.frame(x)
		ncols <- ncol(df)
		for(i in 1:ncols){
			x <- df[,i]
			na_place <- sample(x = 1:length(x), size = floor(length(x)*prop), replace = FALSE)
			x[na_place] <- NA
			df[,i] <- x
		}
		return(df)
	}else if(is.list(x)){
		stop("A list can not be evaluated. Please introduce a vector instead.")
	}else{stop("Unrecognized Format.")}
}


na_replace <- function(x){
	if(is.vector(x) & !is.list(x)){
		new_x <- x
		w <- which(is.na(x))
		n <- sum(is.na(x))
		y <- x[!is.na(x)]
		rep <- sample(x = y, size = length(w), replace = TRUE)
		for(i in w) new_x[i] <- sample(x = y, size = 1, replace = TRUE)
		return(new_x)
	}else if(is.data.frame(x)){
		df <- as.data.frame(x)
		ncols <- ncol(df)
		for(i in 1:ncols){
			x <- df[,i]
			if(sum(is.na(x)) > 0){
				new_x <- x
				w <- which(is.na(x))
				n <- sum(is.na(x))
				y <- x[!is.na(x)]
				rep <- sample(x = y, size = length(w), replace = TRUE)
				for(i in w) new_x[i] <- sample(x = y, size = 1, replace = TRUE)
				df[,i] <- new_x
			}
		}
		return(df)
	}else if(is.list(x)){
		stop("A list can not be evaluated. Please introduce a vector instead.")
	}else{stop("Unrecognized Format.")}
}


na_map <- function(x){

	require(tidyverse)
	require(viridis)

	x %>%
		is.na() %>%
		melt() %>%
		ggplot(data = .,
					 aes(x = Var2,
					 		y = Var1)) +
		geom_raster(aes(fill = value)) +
		theme_minimal() +
		theme(axis.text.x  = element_text(angle=45, vjust=0.5)) +
		labs(x = "Variables in Dataset",
				 y = "Rows / observations") +
		scale_fill_brewer(name = "", labels = c("Present", "Missing"), type = "div", palette = 4, direction = -1)

}



