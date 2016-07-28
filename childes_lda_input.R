library(dplyr)
library(purrr)

childes_db <- do.call(src_mysql, yaml::yaml.load_file("db.yaml"))
childes_tbl <- tbl(childes_db,
                   sql("SELECT filename, speaker, gender, gloss FROM words"),
                   n = -1)

childes_words <- childes_tbl %>%
  filter(speaker != "CHI") %>%
  collect()

childes_words_gender <- childes_words %>%
  filter(gender != "")

mappify <- function(col) {
  value_list <- childes_words_gender[[col]] %>% unique() %>% sort()
  write(value_list, file = "data/")
  value_index <- childes_words_gender[[col]] %>%
    map_int(~which(.x == value_list))
  value_index
}

gg <- mappify("gender")
ff <- mappify("filename")
