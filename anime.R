#################################
#                               #
# Dataset films d'animation jap #
#                               #
#################################


install.packages(c('jsonlite', 'tidyverse', 'rvest', 'polite', 'data.table'))

library(jsonlite)
library(tidyverse)
library(rvest)      # scrape a site
library(polite)     # respectful webscraping
library(data.table)

# Make our intentions known to the website
session <- bow(url="https://myanimelist.net/", force=T)

# Nb pages (et donc iterations)
iter <- 250 %/% 50

for (i in 1:iter) {
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

# Scrapping
serieData <- function(n, session) {
  url <- paste("https://myanimelist.net/anime/", anime_id[[n]], sep="")
  webpage <- nod(session, url) %>% scrape(verbose=TRUE)
  
  # Nb de membres ayant attribues une note
  rating_count <- webpage %>% html_nodes("[itemprop='ratingCount']") %>% html_text(trim=T) %>% as.integer()
  # Recuperer les genres
  genres <- webpage %>% html_nodes("[itemprop='genre']") %>% html_text(trim=T) %>% list()
  # Recuperer le nom du studio
  studio <- webpage %>% html_nodes(".studio") %>% html_text(trim=T)
  # Recuperer le rang en termes de popularite
  popularity <- webpage %>% html_nodes(".popularity strong") %>% html_text(trim=T) %>% substr(2, nchar(webpage)) %>% as.integer()
  # Recuperer le rang global
  global_rank <- webpage %>% html_nodes(".ranked strong") %>% html_text(trim=T) %>% substr(2, nchar(webpage))
  
  return(list(rating_count, genres, studio, popularity, global_rank))
}

# Ajout de nouvelles colonnes
t_anime <- add_column(t_anime, rating_count='', genres='', 
                      studio='', popularity='', global_rank='')

# Remplir les nouvelles colonnes
for (i in 1:nrow(t_anime)) {
  l <- serieData(i, session)
  if(length(l[[1]]) > 0){t_anime$rating_count[i] <- l[[1]]}
  if(length(l[[2]]) > 0){t_anime$genres[i] <- l[[2]]}
  if(length(l[[3]]) > 0){t_anime$studio[i] <- l[[3]]}
  if(length(l[[4]]) > 0){t_anime$popularity[i] <- l[[4]]}
  if(length(l[[5]]) > 0){t_anime$global_rank[i] <- l[[5]]}
}

# A voir pour Source, Duration, Favorite si possible



# Current directory
curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Save a single R object in rds file
saveRDS(t_anime, file=paste0(curr_dir, '/anime.rds'))
# Restore it under a different name
anime_tbl <- readRDS(file=paste0(curr_dir, '/anime.rds'))

# Write in TSV file, handle list-type column thanks to fwrite
fwrite(t_anime, file=paste0(curr_dir, '/anime.tsv'), sep="\t", sep2=c("", " ", ""))

