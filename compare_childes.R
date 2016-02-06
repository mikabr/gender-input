library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(langcog)
library(yaml)

# Connect to childes db on remote server, as specific in db.yaml config file
childes_db <- do.call(src_mysql, yaml.load_file("db.yaml"))

# Get words table
suppressWarnings(
childes_words <- tbl(childes_db, "words")
)

# Compute count of each word
childes_counts <- childes_words %>%
  filter(speaker != "CHI") %>%
  group_by(filename, gender, gloss) %>%
  summarise(n = n()) %>%
  as.data.frame()

comp_childes_counts <- childes_counts %>%
  rename(stephan_count = n) %>%
  filter(!(gloss %in% c("0", "laughs", "babbles", "cries", "sings", "giggles"))) %>%
  mutate(filename = unlist(map(strsplit(filename, "\\/"),
                               function(parts) unlist(strsplit(parts[length(parts)], "\\."))[1])),
         gloss = unlist(map(strsplit(gloss, "\\+"), function(parts) parts[1])),
         gloss = tolower(gloss),
         gloss = ifelse(substr(gloss, 1, 1) == "0", substr(gloss, 2, length(gloss)), gloss)) %>%
  group_by(filename, gender, gloss) %>%
  summarise(stephan_count = sum(stephan_count)) %>%
  filter(filename %in% c("01", "02", "03", "04", "05", "06", "07", "08",
                         "09", "10", "11", "12", "13", "14", "15", "16",
                         "17", "18", "19", "20", "eric1", "eric2", "eric3",
                         "gia2", "gia3", "gia4", "gia5"))

# Load files in data directory outputted by get_childes_counts.py
files <- list.files("data")
words <- c()
nltk_counts <- map_df(files, function(file) {
  file_info <- unlist(strsplit(file, "[_\\.]"))
  doc <- read_csv(sprintf("data/%s", file)) %>%
    mutate(filename = file_info[1],
           #age = file_info[2],
           gender = file_info[3],
           word = tolower(word)) %>%
    group_by(filename, gender, word) %>%
    summarise(count = sum(count))
  for (word in doc$word) {
    if (word %in% names(words)) {
      words[word] <<- words[word] + 1
    } else {
      words[word] <<- 1
    }
  }
  return(doc)
}) %>%
  rename(gloss = word, nltk_count = count) %>%
  filter(!(gloss %in% c("laughs", "babbles", "cries", "sings", "giggles")))

# Join counts form the two sources
all_counts <- full_join(
  comp_childes_counts %>% select(-gender),
  nltk_counts %>% select(-gender)
)

# Look at words that appear in one source but not the other
all_counts %>%
  filter(is.na(nltk_count) | is.na(stephan_count)) %>%
  View()

# Look at words by difference between counts
all_counts %>%
  mutate(diff = abs(nltk_count - stephan_count)) %>%
  filter(diff != 0) %>%
  ungroup() %>%
  arrange(desc(diff)) %>%
  View()

# Plot words by one count against the other
ggplot(all_counts, aes(x = stephan_count, y = nltk_count, colour = filename)) +
  geom_point() +
  scale_colour_solarized() +
  theme_mikabr()
