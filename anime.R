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

# id pour l'url
anime_id <- as.list(t_anime$mal_id)

# Test de fonction web scrapping pour les genres
serieData <- function(n) {
  url <- paste("https://myanimelist.net/anime/", anime_id[[n]], sep="")
  webpage <- read_html(url)
  
  # Recuperer les genres sous forme de liste
  genres <- webpage %>% html_nodes("[itemprop='genre']") %>% html_text() %>% as.list()
  
  return(genres)
}

# Ajout d'une colonne genre au tibble
t_anime <- add_column(t_anime, genres=0)

# Remplir la colonne 'genres'
for (i in 1:nrow(t_anime)) {
  l <- serieData(i)
  Sys.sleep(runif(1,0.75,1.5))
  t_anime$genres[i] <- list(l)
}


# A voir pour Studio, Source, Duration, Favorite si possible


