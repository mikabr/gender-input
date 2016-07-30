library(dplyr)
library(purrr)
library(feather)
library(SnowballC)

pull <- function(x,y) {x[,if(is.name(substitute(y))) deparse(substitute(y)) else y, drop = FALSE][[1]]}

childes_db <- do.call(src_mysql, yaml::yaml.load_file("db.yaml"))
childes_tbl <- tbl(childes_db,
                   sql("SELECT filename, speaker, gender, child, corpus, gloss FROM words"),
                   n = -1)

childes_words <- childes_tbl %>%
  filter(speaker != "CHI") %>%
  collect()


childes_words_gender <- childes_words %>%
  filter(gender != "")

# mappify <- function(df, col, path) {
#   value_list <- df[[col]] %>% unique() %>% sort()
#   write(value_list, file = sprintf("%s/%s_map.txt", path, col))
#   value_map <- setNames(1:length(value_list), value_list)
#   value_index <- df[[col]] %>% map_int(~value_map[[.x]])
#   write_feather(setNames(data_frame(index = value_index), col),
#                 path = sprintf("%s/%s_index.feather", path, col))
#   return()
# }

mappify <- function(df, col, path) {
  value_list <- df[[col]] %>% unique() %>% sort()
  write(value_list, file = sprintf("%s/%s_map.txt", path, col))
  value_map <- data_frame(col = value_list, index = 1:length(value_list)) %>%
    setNames(c(col, "index"))
  value_index <- df %>% left_join(value_map) %>% select(index)
  write_feather(setNames(value_index, col),
                path = sprintf("%s/%s_index.feather", path, col))
  return()
}

mappify(childes_words_gender, "gender", "data/unstemmed")
mappify(childes_words_gender, "filename", "data/unstemmed")
mappify(childes_words_gender, "gloss", "data/unstemmed")
mappify(childes_words_gender, "child", "data/unstemmed")
mappify(childes_words_gender, "corpus", "data/unstemmed")

stem <- function(x) wordStem(x) %>% gsub("'", "", .)

childes_stems <- childes_words_gender %>%
  filter(!(substr(gloss, 1, 1) %in% c("&", "0", "")),
         nchar(gloss) < 20) %>%
  mutate(gloss = gsub("-.*", "", gloss),
         gloss = ifelse(stem(gloss) == "", gloss, stem(gloss)))

mappify(childes_stems, "gender", "data/stemmed")
mappify(childes_stems, "filename", "data/stemmed")
mappify(childes_stems, "gloss", "data/stemmed")
mappify(childes_stems, "child", "data/stemmed")
mappify(childes_stems, "corpus", "data/stemmed")

