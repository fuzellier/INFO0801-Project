#################################
#                               #
# Dataset films d'animation jap #
#                               #
#################################


install.packages(c('jsonlite', 'tidyverse', 'rvest', 'polite', 'data.table', 'lubridate'))
install.packages("openxlsx", dependencies=TRUE)

library(jsonlite)
library(tidyverse)
library(rvest)      # scrape a site
library(polite)     # respectful webscraping
library(data.table)
library(openxlsx)
library(lubridate)

# Make our intentions known to the website
session <- bow(url="https://myanimelist.net/", force=TRUE)

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
cols.to.drop <- c('type', 'image_url', 'episodes', 'end_date')
t_anime <- t_anime %>% select(-one_of(cols.to.drop))

# Nombre de colonnes
nb_cols <- ncol(t_anime)

# Changement du format de date
startdate <- parse_date_time(t_anime$start_date, "my")
newdate <- format(startdate, format="%m-%Y")
t_anime$start_date <- newdate

# id pour l'url
anime_id <- as.list(t_anime$mal_id)

# Extraire nouvelles donnees de l'API
animeAPI <- function(n) {
  url <- paste("https://api.jikan.moe/v3/anime/", anime_id[[n]], sep="")
  list_json <- fromJSON(url)
  
  source <- list_json$source
  duration <- list_json$duration
  rating <- list_json$rating
  favorites <- list_json$favorites
  
  return(list(source, duration, rating, favorites))
}

# Ajout de nouvelles colonnes pour donnees de l'API
t_anime <- add_column(t_anime, source='', duration='', rating='', favorites='')

# Nombre de colonnes ajoutees
nb_addCols_api <- t_anime[, {nb_cols+1}:ncol(t_anime)] %>% length()

# Remplir les nouvelles colonnes
for (i in 1:nrow(t_anime)) {
  for (j in 1:nb_addCols_api) {
    l_api <- animeAPI(i)
    Sys.sleep(0.2) # avoid HTTP error 429
  
    if(length(l_api[[j]]) > 0){t_anime[i, (ncol(t_anime)-nb_addCols_api) + j] <- l_api[[j]]}
    else{t_anime[i, (ncol(t_anime)-nb_addCols_api) + j] <- "NA"}
  }
}


# Scrapping
animeData <- function(n, session) {
  url <- paste("https://myanimelist.net/anime/", anime_id[[n]], sep="")
  webpage <- nod(session, url) %>% scrape(verbose=TRUE)
  
  # Nb de membres ayant attribues une note
  rating_count <- webpage %>% html_nodes("[itemprop='ratingCount']") %>% html_text(trim=T) %>% as.integer()
  # Recuperer le nom du studio principal
  studio <- webpage %>% html_nodes(".studio a:first-child") %>% html_text(trim=T)
  # Recuperer le rang en termes de popularite
  popularity <- webpage %>% html_nodes(".popularity strong") %>% html_text(trim=T) %>% substr(2, nchar(webpage)) %>% as.integer()
  # Recuperer le rang global
  global_rank <- webpage %>% html_nodes(".ranked strong") %>% html_text(trim=T) %>% substr(2, nchar(webpage)) %>% as.integer()
  
  return(list(rating_count, studio, popularity, global_rank))
}

# Ajout de nouvelles colonnes pour les donnees scrapped
t_anime <- add_column(t_anime, rating_count='', studio='', popularity='', global_rank='')

# Nombre de colonnes ajoutees pour le scrapping
nb_cols <- nb_addCols_api + nb_cols + 1
nb_addCols_scrap <- t_anime[, nb_cols:ncol(t_anime)] %>% length()

# Une nouvelle fois, remplir les nouvelles colonnes
for (i in 1:nrow(t_anime)) {
  for (j in 1:nb_addCols_scrap) {
    l_scrap <- animeData(i, session)
    
    if(length(l_scrap[[j]]) > 0){t_anime[i, (ncol(t_anime)-nb_addCols_scrap) + j] <- l_scrap[[j]]}
    else{t_anime[i, (ncol(t_anime)-nb_addCols_scrap) + j] <- "NA"}
  }
}


# Ajout d'une colonne 'genres'
t_anime <- add_column(t_anime, genres='')

# Remplir colonnes 'genres'
for (i in 1:nrow(t_anime)) {
  url_genres <- paste("https://myanimelist.net/anime/", anime_id[[i]], sep="")
  webpage <- nod(session, url_genres) %>% scrape(verbose=TRUE)
  
  # Recuperer les genres sous forme de liste
  genres_list <- webpage %>% html_nodes("[itemprop='genre']") %>% html_text(trim=T) %>% list()
  
  if(length(genres_list) > 0){t_anime$genres[i] <- genres_list}
  else{t_anime$genres[i] <- "NA"}
}


# Current directory
curr_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# Save a single R object in rds file
saveRDS(t_anime, file=paste0(curr_dir, '/anime.rds'))
# Restore it under a different name
anime_tbl <- readRDS(file=paste0(curr_dir, '/anime.rds'))

# Write in XLSX file
openxlsx::write.xlsx(anime_tbl, file=paste0(curr_dir, "/anime.xlsx"))
