install.packages("jsonlite")
library("jsonlite")
install.packages("tidyverse")
library(tidyverse)

individus<-250
individus<-250%/%50

for (i in 1:individus){
	url_req <- paste("https://api.jikan.moe/v3/top/anime/", i, sep="")
	list_json1 <- fromJSON(url_req)

	if (missing(tibble1) == TRUE){
		tibble1 <- list_json1$top
	}else{
		inter_json <- 
	}
}

df_json1 <- as_tibble(inter_json, validate = F) 
print(df_json1)
