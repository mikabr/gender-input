library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(ggplot2)
library(langcog)
library(yaml)
library(assertthat)
library(gender)
theme_set(theme_mikabr())

# Connect to childes db on remote server, as specific in db.yaml config file
childes_db <- do.call(src_mysql, yaml.load_file("db.yaml"))

# Get words table
#childes_words <- tbl(childes_db, "words", n = -1)
childes_words <- tbl(childes_db, sql("SELECT filename, speaker, gender, hex(gloss) AS hex_gloss, gloss FROM words"), n = -1)

# Compute count of each word
childes_counts <- childes_words %>%
  filter(speaker != "CHI") %>%
  group_by(filename, gender, gloss, hex_gloss) %>%
  summarise(n = n()) %>%
  select(-hex_gloss) %>%
  collect()

comp_childes_counts <- childes_counts %>%
  ungroup() %>%
  rename(stephan_count = n) %>%
  filter(!(substr(gloss, 1, 1) == "0"), gloss != "&=laughs") %>%
  mutate(filename = gsub(".cha", "", filename) %>%
           strsplit("\\/") %>%
           map(~paste(.x[5:length(.x)], collapse = "/")) %>%
           unlist(),
         gloss = gsub("&", "", gloss))

# Load files in data directory outputted by get_childes_counts.py
#files <- list.files("data_unstemmed", full.names = TRUE, recursive = TRUE)
files <- list.files("data", full.names = TRUE, recursive = TRUE)
#words <- c()
nltk_counts <- map_df(files, function(file) {
  print(file)
  #file_info <- unlist(strsplit(file, "[_\\.]"))
  file_parts <- file %>% gsub(".csv", "", .) %>% strsplit("/") %>% unlist()
  doc <- read.csv(file, stringsAsFactors = FALSE) %>%
    mutate(filename = paste(file_parts[2:length(file_parts)], collapse = "/"),
           #age = file_info[2],
           #gender = file_info[3],
           count = as.numeric(count))
#   for (word in doc$word) {
#     if (word %in% names(words)) {
#       words[word] <<- words[word] + 1
#     } else {
#       words[word] <<- 1
#     }
#   }
  return(doc)
}) %>%
  rename(gloss = word, nltk_count = count)

setdiff(comp_childes_counts$filename, nltk_counts$filename)
setdiff(nltk_counts$filename, comp_childes_counts$filename)

# Join counts form the two sources
all_counts <- full_join(
  comp_childes_counts %>% select(-gender),
  nltk_counts %>% select(-gender)
) %>%
  replace(is.na(.), 0)

# Look at words by difference between counts
all_counts %>%
  mutate(diff = abs(nltk_count - stephan_count)) %>%
  filter(diff != 0) %>%
  ungroup() %>%
  #  arrange(filename) %>%
  arrange(desc(diff)) %>%
  #  write_csv("diff_counts.csv")
  View()



corpus_counts <- comp_childes_counts %>%
  group_by(gloss) %>%
  summarise(n = sum(stephan_count)) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number())

ggplot(corpus_counts, aes(x = log(rank), y = log(n))) +
  geom_jitter() +
  theme_mikabr()

corpus_counts <- nltk_counts %>%
  group_by(gloss) %>%
  summarise(n = sum(nltk_count)) %>%
  arrange(desc(n)) %>%
  mutate(rank = row_number())

ggplot(corpus_counts, aes(x = log(rank), y = log(n))) +
  geom_jitter() +
  theme_mikabr()


metadata <- read_csv("metadata.csv")

gender_data <- nltk_counts %>%
  left_join(metadata) %>%
  mutate(name = map(strsplit(filename, "/"), ~.x[length(.x)]) %>%
           unlist() %>%
           substr(1, nchar(.) - 2))

missing_sex <- gender_data %>%
  filter(sex == "None") %>%
  select(filename, name) %>%
  distinct()
#write_csv(missing_sex, "missing_sex.csv")
coded_sex <- read_csv("missing_sex.csv") %>% rename(coded_sex = sex)

#gender_guesses <- gender(missing_sex$name) %>% select(name, gender)

gender_counts <- gender_data %>%
  left_join(coded_sex) %>%
  mutate(gender = ifelse(sex != "None", sex, coded_sex)) %>%
  filter(!is.na(gender)) %>%
  group_by(gender, gloss) %>%
  summarise(count = sum(nltk_count))

gender_freqs <- gender_counts %>%
#  spread(gender, count, fill = 0) %>%
#  filter(female > 10, male > 10) %>%
#  gather(gender, count, female, male) %>%
  group_by(gender) %>%
  mutate(freq = count / sum(count)) #%>%
  #select(-count) %>%
  #spread(gender, freq, fill = 0)


# similarity <- function(A, B) {
#   assert_that(are_equal(length(A), length(B)))
#   sum(A * B) / (sqrt(sum(A ^ 2)) * sqrt(sum(B ^ 2)))
# }
#
# gf <- gender_freqs %>%
#   filter(female != 0, male != 0)
# chisq.test(gf$female, gf$male)
# similarity(gf$female, gf$male)
#
# gf_random <- gf %>%
#   gather(gender, freq, female, male) %>%
#   group_by(gloss) %>%
#   mutate(gender = sample(c("female", "male"))) %>%
#   spread(gender, freq)
# chisq.test(gf_random$female, gf_random$male)
# similarity(gf_random$female, gf_random$male)

safe_log <- function(x) ifelse(x == 0, NA, log(x))

gender_freqs <- gender_counts %>%
  group_by(gender) %>%
  mutate(freq = count / sum(count),
         log_freq = safe_log(freq))

gender_mut_inf <- gender_counts %>%
  spread(gender, count, fill = 0) %>%
  filter(female != 0, male != 0) %>%
  mutate(gsq = gsq(female, male),
         mut_inf = log(female / sum(female) * (sum(female) + sum(male)) / (female + male))) %>%
  rename(female_count = female, male_count = male)

threshold <- 1.5
ggplot(gender_freqs, aes(x = log_female, y = log_male, label = gloss)) +
  geom_jitter() +
  geom_abline(slope = 1, intercept = 0, colour = "grey", linetype = "dashed") +
  xlab("Log probability in speech to female children") +
  ylab("Log probability in speech to male children") +
  coord_fixed() +
  theme_mikabr()

filter_gender_freqs <- gender_freqs %>%
  filter(log_female > -13, log_male > -13)
ggplot(filter_gender_freqs, aes(x = log_female, y = log_odds, label = gloss)) +
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
  geom_jitter(data = filter(filter_gender_freqs, log_odds < 1.5), size = 0.5) +
  geom_label(data = filter(filter_gender_freqs, log_odds > 1.5), size = 2,
             position = position_jitter(1, 1),
             label.size = 0.1, label.padding = unit(0.1, "lines"))

test_gender_freqs <- gender_counts %>%
  spread(gender, count, fill = 0) %>%
  rename(female_count = female, male_count = male) %>%
  filter(female_count > 5, male_count > 5,
         !grepl("^[[:upper:]]+$", substr(gloss, 1, 1))) %>%
  mutate(total = female_count + male_count,
         female_test = unlist(map2(female_count, total,
                                   ~binom.test(.x, .y, 0.6, "greater")$p.value)),
         male_test = unlist(map2(male_count, total,
                                 ~binom.test(.x, .y, 0.6, "greater")$p.value)),
         female_freq = female_count / sum(female_count),
         male_freq = male_count / sum(male_count),
         log_female = safe_log(female_freq),
         log_male = safe_log(male_freq),
         log_odds_male = log_female / log_male,
         log_odds_female = log_male / log_female)

ggplot(test_gender_freqs, aes(x = log_female, y = log_odds_female, label = gloss)) +
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
  geom_jitter(data = filter(test_gender_freqs, female_test > 0.01), size = 0.5) +
  geom_label(data = filter(test_gender_freqs, female_test < 0.01), size = 3,
             #position = position_jitter(1, 1),
             label.size = 0.1, label.padding = unit(0.1, "lines"))

ggplot(test_gender_freqs, aes(x = log_male, y = log_odds_male, label = gloss)) +
  geom_hline(yintercept = 1, color = "grey", linetype = "dashed") +
  geom_jitter(data = filter(test_gender_freqs, male_test > 0.01), size = 0.5) +
  geom_label(data = filter(test_gender_freqs, male_test < 0.01), size = 3,
             #position = position_jitter(1, 1),
             label.size = 0.1, label.padding = unit(0.1, "lines"))

complete_gender_freqs <- gender_freqs %>%
  filter(!is.na(log_female), !is.na(log_male),
         !grepl("^[[:upper:]]+$", substr(gloss, 1, 1)))

freq_model_male <- lm(log_male ~ log_female, data = filter_gender_freqs, y = TRUE)
freq_fit_male <- data.frame(residual = freq_model_male$residuals,
                            log_male = freq_model_male$y,
                            gloss = filter_gender_freqs$gloss)
ggplot(freq_fit_male, aes(x = log_male, y = residual, label = gloss)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter(data = filter(freq_fit_male, residual < 2)) +
  geom_label(data = filter(freq_fit_male, residual > 2), size = 2,
             position = position_jitter(1, 1),
             label.size = 0.1, label.padding = unit(0.1, "lines"))

freq_model_female <- lm(log_female ~ log_male, data = complete_gender_freqs, y = TRUE)
freq_fit_female <- data.frame(residual = freq_model_female$residuals,
                              log_female = freq_model_female$y,
                              gloss = complete_gender_freqs$gloss)
ggplot(freq_fit_female, aes(x = log_female, y = residual, label = gloss)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dashed") +
  geom_jitter(data = filter(freq_fit_female, residual < 3)) +
  geom_label(data = filter(freq_fit_female, residual > 3), size = 2,
             position = position_jitter(1, 1),
             label.size = 0.1, label.padding = unit(0.1, "lines"))

classifly
