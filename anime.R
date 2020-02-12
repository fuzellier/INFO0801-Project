#################################
#                               #
# Dataset films d'animation jap #
#                               #
#################################


install.packages(c('jsonlite', 'tidyverse', 'rvest', 'polite'))

library(jsonlite)
library(tidyverse)
library(rvest)    # scrape a site
library(polite)   # respectful webscraping

# Make our intentions known to the website
mal_bow <- bow(url="https://myanimelist.net/", force=T)
print(mal_bow)


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
  
  # Recuperer les genres
  genres <- webpage %>% html_nodes("[itemprop='genre']") %>% html_text(trim=T) %>% list()
  # Recuperer le nom du studio
  studio <- webpage %>% html_nodes(".studio") %>% html_text(trim=T)
  # Recuper le rang en termes de popularite
  popularity <- webpage %>% html_nodes(".popularity strong") %>% html_text(trim=T) %>% substr(2, nchar(webpage)) %>% as.integer()
  
  return(list(genres, studio, popularity))
}

# Ajout de colonnes pour le web scrapping
t_anime <- add_column(t_anime, genres='', studio='', popularity='')

# Remplir les nouvelles colonnes
for (i in 1:nrow(t_anime)) {
  l <- serieData(i)
  t_anime$genres[i] <- l[[1]]
  t_anime$studio[i] <- l[[2]]
  t_anime$popularity[i] <- l[[3]]
}

# A voir pour Source, Duration, Favorite si possible


