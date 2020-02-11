install.packages('jsonlite')
install.packages('tidyverse')

library('jsonlite')
library('tidyverse')

individus <- 250 %/% 50

for (i in 1:individus) {
	url_req <- paste("https://api.jikan.moe/v3/top/anime/", i, sep="")
	list_json1 <- fromJSON(url_req)

	if (exists('tibble1') == FALSE){
	  tibble1 <- tibble()
	  tibble1 <- as_tibble(rbind(tibble1, as_tibble(list_json1$top)))
	} else {
	  tibble1 <- as_tibble(rbind(tibble1, as_tibble(list_json1$top)))
	}
}

#df_json1 <- as_tibble(inter_json, validate = F) 
#print(df_json1)
