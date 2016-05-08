library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)
library(langcog)
theme_set(theme_mikabr())

files <- list.files("data")
words <- c()
docs <- map(files, function(file) {
  doc <- read_csv(sprintf("data/%s", file))
  for (word in doc$word) {
    if (word %in% names(words)) {
      words[word] <<- words[word] + 1
    } else {
      words[word] <<- 1
    }
  }
  return(doc)
  })
names(docs) <- files

N <- length(docs)
idf <- data.frame(word = names(words),
                  idf = log(N/words),
                  row.names = NULL)

get_similarity <- function(words0, words1) {
  components <- words0 %>%
    rename(count0 = count) %>%
    inner_join(rename(words1, count1 = count)) %>%
    left_join(idf) %>%
    mutate(tfidf0 = count0 * idf,
           tfidf1 = count1 * idf)
  sum(components$tfidf0 * components$tfidf1) / (sqrt(sum(components$tfidf0 ^ 2)) * sqrt(sum(components$tfidf1 ^ 2)))
}

pairs <- expand.grid(file0 = files, file1 = files) %>%
  rowwise() %>%
  mutate(similarity = get_similarity(docs[[file0]], docs[[file1]]))

pairs_data <- pairs %>%
  mutate(file0 = unlist(strsplit(as.character(file0), ".csv")),
         file1 = unlist(strsplit(as.character(file1), ".csv"))) %>%
  separate(file0, c("child0", "age0", "sex0"), sep = "_") %>%
  separate(file1, c("child1", "age1", "sex1"), sep = "_") %>%
  mutate(age0 = as.numeric(age0),
         age1 = as.numeric(age1))

pairs_data %>%
  filter(age0 %in% c(20, 22),
         age1 %in% c(20, 22)) %>%
  ggplot(aes(x = child0, y = child1, colour = sex0)) +
    geom_tile(aes(fill = similarity))

pairs_data %>%
  filter(age0 == 28, age1 == 28) %>%
  ggplot(aes(x = similarity, y = child0, colour = sex1)) +
    facet_wrap(~sex0) +
    geom_point() +
    scale_colour_solarized()

pairs_data %>%
  filter(age0 == 28, age1 == 28) %>%
  ggplot(aes(x = child0, y = similarity, colour = sex1)) +
  facet_wrap(~sex0, scales = "free_x") +
  geom_boxplot() +
  geom_jitter() +
  scale_colour_solarized()

pairs_mat <- pairs_data %>%
  mutate(distance = 1 - similarity) %>%
  select(child0, child1, distance) %>%
  spread(child1, distance)
rownames(pairs_mat) <- pairs_mat$child0
pairs_mat <- pairs_mat %>% select(-child0)

pairs_fit <- cmdscale(pairs_mat, eig = TRUE, k = 2)
pair_fit_df <- as.data.frame(pairs_fit$point)
pair_fit_df$child <- rownames(pair_fit_df)
rownames(pair_fit_df) <- NULL

pairs_data %>%
  select(child0, age0, sex0) %>%
  rename(child = child0, age = age0, sex = sex0) %>%
  distinct() %>%
  right_join(pair_fit_df) %>%
  ggplot(aes(x = V1, y = V2, colour = factor(sex))) +
    geom_point() +
    scale_colour_solarized()
