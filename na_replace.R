na_replace <- function(x){
	if(is.vector(x) & !is.list(x)){
		new_x <- x
		w <- which(is.na(x))
		y <- x[!is.na(x)]
		for(i in w) new_x[i] <- sample(x = y, size = 1, replace = TRUE); cat(paste0("... ", floor(i/length(w)*100), "% ... \n"))
		return(new_x)
	}else if(is.data.frame(x)){
		df <- as.data.frame(x)
		ncols <- ncol(df)
		for(i in 1:ncols){
			cat(paste0("... ", floor(i/ncols*100), "% ... \n"))
			x <- df[i]
			if(sum(is.na(x)) > 0){
				new_x <- x
				w <- which(is.na(x))
				y <- x[!is.na(x)]
				for(k in w) new_x[k,] <- sample(x = y, size = 1, replace = TRUE)
				df[i] <- new_x
			}
		}
		return(df)
	}else if(is.list(x)){
		stop("A list can not be evaluated. Please introduce a vector instead.")
	}else{stop("Unrecognized Format.")}
}
