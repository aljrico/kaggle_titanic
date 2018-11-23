feature_engineering <- function(tr_te, type = "tree"){

	tr_te$Embarked[c(62,830)] <- 'C'
	median_fare <- tr_te %>%
		filter(Pclass == 3) %>%
		filter(Embarked == 'S') %>%
		.$Fare %>%
		median(na.rm=T)

	tr_te$Fare[1044] <- median_fare

	tr_te <- na_replace(tr_te)

	# Deck
	tr_te$Deck<-sapply(tr_te$Cabin, function(x) strsplit(x, NULL)[[1]][1])
	tr_te$Cabin_length <- nchar(tr_te$Cabin) %>% as.numeric()
	tr_te$Cabin <- gsub("[^0-9]","",tr_te$Cabin)

	# Mother & Children
	tr_te$Child[tr_te$Age < 18] <- 'Child'
	tr_te$Child[tr_te$Age >= 18] <- 'Adult'

	tr_te$Mother <- 'nomother'
	tr_te$Mother[tr_te$Sex == 'female' & tr_te$Parch > 0 & tr_te$Age > 18] <- 'Mother'

	# Finish by factorizing our two new factor variables
	tr_te$Child  <- factor(tr_te$Child)
	tr_te$Mother <- factor(tr_te$Mother)

	# Title
	rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 'Major', 'Sir', 'Jonkheer')
	tr_te$Title <- gsub('(.*, )|(\\..*)', '', tr_te$Name)
	tr_te$Title[tr_te$Title == 'Mlle']        <- 'Miss'
	tr_te$Title[tr_te$Title == 'Ms']          <- 'Miss'
	tr_te$Title[tr_te$Title == 'Mme']         <- 'Mrs'
	tr_te$Title[tr_te$Title == 'Rev']         <- 'Rev'
	tr_te$Title[tr_te$Title == 'Dr']        	<- 'Dr'
	tr_te$Title[tr_te$Title == 'Master']      <- 'Master'
	tr_te$Title[tr_te$Title %in% rare_title]  <- 'Rare_Title'

	# Surname
	tr_te$surname <- sapply(tr_te$Name, function(x) strsplit(x, split = '[,.]')[[1]][1])

	# Ethnicity
	tr_te <- tr_te %>% as_tibble()
	ethnicitys <- predict_race(tr_te["surname"], surname.only = TRUE) %>% as_tibble()
	tr_te <- ethnicitys %>%
		dplyr::mutate(ethnicity = colnames(ethnicitys %>% dplyr::select(-surname))[apply(ethnicitys %>% dplyr::select(-surname),1,which.max)]) %>%
		dplyr::mutate(ethnicity = ifelse(ethnicity == "pred.whi", "White", ethnicity)) %>%
		dplyr::mutate(ethnicity = ifelse(ethnicity == "pred.bla", "Black", ethnicity)) %>%
		dplyr::mutate(ethnicity = ifelse(ethnicity == "pred.asi", "Asian", ethnicity)) %>%
		dplyr::mutate(ethnicity = ifelse(ethnicity == "pred.his", "Hispanic", ethnicity)) %>%
		dplyr::mutate(ethnicity = ifelse(ethnicity == "pred.oth", "Other", ethnicity)) %>%
		select(ethnicity) %>%
		cbind(tr_te) %>%
		select(-surname) %>%
		as_tibble()

	tr_te$Ticket <- NULL
	tr_te$PassengerId <- NULL
	tr_te$Cabin <- tr_te$Cabin %>% as.numeric()
	tr_te$Name <- NULL

	tr_te <-
		tr_te %>%
		mutate_all(funs(replace(., is.na(.), 0)))

	tr_te <-
		tr_te %>%
		mutate_all(funs(replace(., is.na(.), "")))

	data <- tr_te
	features <- colnames(data)
	for(f in features) {
		if ((class(data[[f]])=="factor") || (class(data[[f]])=="character")) {
			levels <- unique(data[[f]])
			data[[f]] <- (factor(data[[f]], levels=levels))
		}
	}
	tr_te <- data
	rm(data);gc()


	if(type == "deep"){
		data <- tr_te
		features <- colnames(data)
		chrs <- c()
		for(f in features) {
			if ((class(data[[f]])=="factor") || (class(data[[f]])=="character")) {
				chrs <- c(chrs,f)
				levels <- unique(data[[f]])
				data[[f]] <- (factor(data[[f]], levels=levels))

				for(level in unique(data[[paste0(f)]])){
					data[paste("dummy", f, level, sep = "_")] <- ifelse(data[[f]] == level, 1, 0)
				}

			}
		}
		tr_te <- data
	}else{
		tr_te %<>% numerise_data()
	}

	tr_te[is.na(tr_te)] <- 0
	rm(data)
	return(tr_te)
}
