install.packages(c('jsonlite', 'tidyverse', 'rvest'))

library('jsonlite')
library('tidyverse')
library('rvest')


individus <- 250 %/% 50

for (i in 1:individus) {
  # .../top/type/page/subtype
	url_req <- paste("https://api.jikan.moe/v3/top/anime/", i, "/movie", sep="")
	list_json <- fromJSON(url_req)

	if (exists('t_anime') == FALSE) {
	  t_anime <- tibble()
	}
	# Concatener par ligne (pour 5 pages de 50 individus)
	t_anime <- as_tibble(rbind(t_anime, as_tibble(list_json$top)))
}

# Supprimer colonnes inutiles
cols.to.drop <- c('type', 'image_url', 'episodes')
t_anime <- t_anime %>% select(-one_of(cols.to.drop))

