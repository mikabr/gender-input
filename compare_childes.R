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
  as.data.frame() %>%
  rename(stephan_count = n) %>%
  mutate(filename = unlist(map(strsplit(filename, "\\/"),
                               function(parts) unlist(strsplit(parts[length(parts)], "\\."))[1])))

# Load files in data directory outputted by get_childes_counts.py
files <- list.files("data")
words <- c()
nltk_counts <- map_df(files, function(file) {
  file_info <- unlist(strsplit(file, "[_\\.]"))
  doc <- read_csv(sprintf("data/%s", file)) %>%
    mutate(filename = file_info[1],
           #age = file_info[2],
           gender = file_info[3])
  for (word in doc$word) {
    if (word %in% names(words)) {
      words[word] <<- words[word] + 1
    } else {
      words[word] <<- 1
    }
  }
  return(doc)
}) %>%
  rename(gloss = word, nltk_count = count)

# Join counts form the two sources
all_counts <- full_join(
  childes_counts %>% select(-gender),
  nltk_counts %>% select(-gender)
)

# Look at words that appear in one source but not the other
all_counts %>%
  filter(is.na(stephan_count) | is.na(nltk_count)) %>%
  View

# Look at words by difference between counts
all_counts %>%
  mutate(diff = abs(nltk_count - stephan_count)) %>%
  filter(diff != 0) %>%
  arrange(desc(diff)) %>%
  View()

# Plot words by one count against the other
ggplot(all_counts, aes(x = stephan_count, y = nltk_count, colour = filename)) +
  geom_point() +
  scale_colour_solarized() +
  theme_mikabr()
