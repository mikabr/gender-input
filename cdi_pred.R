library(wordbankr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(langcog)
library(readr)
library(SnowballC)
library(boot)
library(purrr)
theme_set(theme_mikabr() +
            theme(panel.grid = element_blank(),
                  strip.background = element_blank()))
font <- "Open Sans"

eng_ws <- get_instrument_data(instrument_language = "English",
                              instrument_form = "WS",
                              administrations = TRUE,
                              iteminfo = TRUE,
                              mode = "local")

eng_wg <- get_instrument_data(instrument_language = "English",
                              instrument_form = "WG",
                              administrations = TRUE,
                              iteminfo = TRUE,
                              mode = "local")

fit_gender_diffs <- function(dataset) {

  gender_props <- dataset %>%
    filter(!is.na(sex), type == "word") %>%
    mutate(produces = value == "produces",
           understands = (value == "produces" | value == "understands"),
           definition = gsub("\\*", "", definition)) %>%
    select(-value) %>%
    gather(measure, value, understands, produces) %>%
    group_by(measure, sex, age, lexical_class, definition) %>%
    summarise(num_true = sum(value, na.rm = TRUE),
              num_false = n() - num_true,
              prop = num_true / n())


  fit_group <- function(group_data) {
    model <- glm(cbind(num_true, num_false) ~ age, family = "binomial",
                 data = group_data)
    width = 0.1
    ages <- seq(min(group_data$age), max(group_data$age), by = width)
    fits <- predict(model, newdata = data.frame(age = ages)) %>% boot::inv.logit()
    data_frame(area = sum(fits * width))

  }

  gender_fits <- gender_props %>%
    group_by(measure, sex, lexical_class, definition) %>%
    nest() %>%
    mutate(fit = map(data, fit_group)) %>%
    select(-data) %>%
    unnest()

  gender_fits %>%
    spread(sex, area) %>%
    rename(female_area = Female, male_area = Male) %>%
    mutate(area_diff = female_area - male_area)

}

gender_diffs_wg <- fit_gender_diffs(eng_wg)
gender_diffs_ws <- fit_gender_diffs(eng_ws)

gender_diffs <- bind_rows(
  mutate(gender_diffs_wg, form = "WG"),
  mutate(gender_diffs_ws, form = "WS")
) %>%
  filter(form == "WG" | measure == "produces") %>%
  mutate(form_measure = paste(form, measure, sep = "_"))

top_female <- gender_diffs %>%
  filter(measure == "understands") %>%
  arrange(desc(area_diff)) %>%
  slice(1:20)

gender_props %>%
  filter(measure == "understands", definition %in% top_female$definition) %>%
  mutate(definition = factor(definition, levels = top_female$definition)) %>%
  ggplot(aes(x = age, y = prop, colour = sex)) +
  facet_wrap(~definition) +
  geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "binomial")) +
  scale_colour_solarized()

top_male <- gender_diffs %>%
  filter(measure == "understands") %>%
  arrange(area_diff) %>%
  slice(1:20)

gender_props %>%
  filter(measure == "understands", definition %in% top_male$definition) %>%
  mutate(definition = factor(definition, levels = top_male$definition)) %>%
  ggplot(aes(x = age, y = prop, colour = sex)) +
  facet_wrap(~definition) +
  geom_smooth(method = "glm", se = FALSE,
              method.args = list(family = "binomial")) +
  scale_colour_solarized()

ggplot(gender_diffs, aes(x = area_diff, fill = lexical_class)) +
  facet_wrap(~form_measure) +
  geom_freqpoly(alpha = 0.5) +
  #geom_density(alpha = 0.5) +
  scale_fill_solarized()

bins <- 30
gender_diffs %>%
  mutate(area_diff_interval = cut(area_diff, breaks = bins)) %>%
  ggplot(aes(x = area_diff)) +
  #geom_text(aes(label = definition), position = "dodge") +
  geom_histogram(bins = bins)

threshold <- 0.99
plt_gender_diffs <- gender_diffs %>%
  group_by(form_measure, lexical_class) %>%
  mutate(area_diff_scaled = as.numeric(scale(area_diff)),
         label = gsub("(.*) \\(.*\\)", "\\1", definition),
         outlier = pnorm(abs(area_diff_scaled)) > threshold) %>%
  ungroup() %>%
  mutate(lexical_class = `levels<-`(factor(lexical_class),
                                    list("Nouns" = "nouns",
                                         "Adjectives & Verbs" = c("adjectives", "verbs"),
                                         "Function Words" = "function_words",
                                         "Other" = "other"))) #%>%
  #filter(lexical_class != "Function Words")

gender_diff_means <- plt_gender_diffs %>%
  group_by(form_measure, lexical_class) %>%
  summarise(mean_area_diff = mean(area_diff))

ggplot(plt_gender_diffs, aes(x = lexical_class, y = area_diff,
                             colour = lexical_class)) +
  facet_wrap(~form_measure) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_label(aes(label = label),
             data = filter(plt_gender_diffs, outlier),
             position = position_jitter(width = 1),
             # position = position_jitter(width = 1, height = 1),
             label.padding = unit(0.15, "lines"), label.r = unit(0.05, "lines"),
             family = font, size = 2) +
  geom_jitter(data = filter(plt_gender_diffs, !outlier), colour = "grey",
              size = 0.4, alpha = 0.5) +
  geom_segment(aes(x = as.numeric(lexical_class) - 0.25,
                   xend = as.numeric(lexical_class) + 0.25,
                   y = mean_area_diff, yend = mean_area_diff),
               data = gender_diff_means, size = 1) +
  scale_colour_solarized(guide = FALSE) +
  labs(x = "", y = "Area between girls' and boys' growth curves")

binwidth <- 0.1
ggplot(plt_gender_diffs, #%>% filter(form_measure == "WS_produces"),
       aes(x = area_diff, colour = lexical_class)) +
  #coord_flip() +
  coord_fixed(ratio = 2 * binwidth, ylim = c(0, 16)) +
  facet_grid(lexical_class ~ form_measure) +
  #facet_grid(lexical_class ~ .) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_vline(aes(xintercept = mean_area_diff, colour = lexical_class),
             alpha = 0.5,
             data = filter(gender_diff_means)) + #, form_measure == "WS_produces")) +
  geom_dotplot(aes(fill = lexical_class), method = "histodot",
               binwidth = binwidth, dotsize = 0.5, stackratio = 1.5) +
  geom_text_repel(aes(label = label, y = 0),
             data = filter(plt_gender_diffs, outlier), #form_measure == "WS_produces"),
           #  label.padding = unit(0.15, "lines"), label.r = unit(0.05, "lines"),
             segment.size = 0.3, point.padding = unit(0.5, "lines"),
             arrow = arrow(length = unit(0.03, 'npc')), nudge_y = 5,
             family = font, size = 2.5) +
  scale_colour_solarized(guide = FALSE) +
  scale_fill_solarized(guide = FALSE) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Area between girls' and boys' growth curves")



stem <- function(x) wordStem(x) %>% gsub("'", "", .)

special_cases <- read_csv("english_special_cases.csv", col_names = FALSE)

transforms <- c(
  function(x) gsub("(.*) \\(.*\\)", "\\1", x),
  function(x) gsub(" ", "_", x),
  function(x) gsub(" ", "+", x),
  function(x) gsub("(.+) \\1", "\\1", x)
)

special_case_map <- map_df(1:nrow(special_cases), function(i) {
  item <- special_cases$X1[i]
  options <- special_cases[i, 2:ncol(special_cases)] %>%
    as.character() %>%
    discard(is.na)
  trans_opts <- map(options, function(opt) map_chr(transforms, ~.x(opt))) %>%
    unlist() %>% unique()
  data_frame(item = rep(item, 2 * length(trans_opts)),
             gloss = c(trans_opts, stem(trans_opts)))
})

pattern_map <- gender_props$definition %>%
  map_df(function(item) {
    options <- item %>% strsplit("/") %>% unlist()
    options <- c(options, stem(options))
    trans_opts <- map(options, function(opt) map_chr(transforms, ~.x(opt))) %>%
      unlist() %>% unique()
    data_frame(item = rep(item, 2 * length(trans_opts)),
               gloss = c(trans_opts, stem(trans_opts))) %>%
      distinct()
  })

case_map <- bind_rows(special_case_map, pattern_map) %>% distinct()

na_log <- function(x) if (x == 0) NA else log(x)
mod_gender_counts <- gender_counts %>%
  mutate(gloss = gsub("-.*", "", gloss),
         gloss = ifelse(stem(gloss) == "", gloss, stem(gloss))) %>%
  left_join(case_map) %>%
  mutate(item = ifelse(is.na(item), gloss, item))

plog <- function(x) x * log(x)
gsq <- function(female, male) {
  a <- female
  b <- male
  c <- sum(female)
  d <- sum(male)
  e_1 <- c * (a + b) / (c + d)
  e_2 <- d * (a + b) / (c + d)
  2 * (a * log(a / e_1) + b * log(b / e_2))
  # c <- sum(female) - female
  # d <- sum(male) - male
  # 2 * (plog(a) + plog(b) + plog(c) + plog(d) -
  #        plog(a + b) - plog(a + c) - plog(b + d) - plog(c + d) +
  #        plog(a + b + c + d))
}


mut_inf <- function(female, male) {
  log2(female / sum(female) * (sum(female) + sum(male)) / (female + male))
}

kl <- function(female, male) {
  p <- female / sum(female)
  q <- male / sum(male)
  p * log(p / q)
}

item_gender_counts <- mod_gender_counts %>%
  #select(item, log_female, log_male) %>%
  group_by(gender, item) %>%
  summarise(count = sum(count)) %>%
  spread(gender, count, fill = 0) %>%
  filter(female != 0, male != 0) %>%
  mutate(mut_inf = mut_inf(female, male),
         gsq = gsq(female, male),
         log_gsq = log(gsq),
         kl = kl(female, male),
         sig_gsq = gsq > 10.83) %>%
  rename(female_count = female, male_count = male)
# summarise(log_female = na_log(sum(exp(log_female))),
#           log_male = na_log(sum(exp(log_male))))

sum(item_gender_counts$gsq)
sum(item_gender_counts$kl) * 2 * sum(item_gender_counts$female_count) + sum(item_gender_counts$male_count)

ggplot(item_gender_counts, aes(x = gsq)) +
  geom_density()

#total <- gender_counts %>% split(.$gender) %>% map_dbl(~sum(.x$count))
min_count <- 20
#min_freq <- log(min_count / total)
gender_diff_freqs <- gender_diffs %>%
  rename(item = definition) %>%
  left_join(item_gender_counts) %>%
  filter(female_count > min_count, male_count > min_count) %>%
  mutate(label = gsub("(.*) \\(.*\\)", "\\1", item),
         lexical_class = `levels<-`(factor(lexical_class),
                                    list("Nouns" = "nouns",
                                         "Adjectives & Verbs" = c("adjectives", "verbs"),
                                         "Function Words" = "function_words",
                                         "Other" = "other")))

ggplot(filter(gender_diff_freqs, form_measure == "WS_produces"), aes(x = gsq)) +
  geom_density()

ggplot(filter(gender_diff_freqs, form_measure == "WS_produces"),
       aes(x = gsq, y = abs(kl))) +
  geom_text(aes(label = label))

gender_diff_models <- gender_diff_freqs %>%
  group_by(form_measure, lexical_class) %>%
  nest() %>%
#  mutate(model = map(data, ~lm(area_diff ~ mut_inf, data = .)),
#  mutate(model = map(data, ~lm(area_diff ~ gsq, data = .)),
  mutate(model = map(data, ~lm(area_diff ~ kl, data = .)),
         results = map(model, ~broom::tidy(.x) %>%
                         mutate(rsq = summary(.x)$adj.r.squared))) %>%
  select(form_measure, lexical_class, results) %>%
  unnest()

# ggplot(gender_diff_freqs, aes(x = mut_inf, y = area_diff)) +
#ggplot(gender_diff_freqs, aes(x = gsq, y = area_diff)) +
ggplot(filter(gender_diff_freqs, lexical_class == "Nouns"),
       aes(x = kl, y = area_diff)) +
  # facet_grid(lexical_class ~ form_measure) +
  facet_wrap(~form_measure) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey") +
  geom_text(aes(label = label, colour = lexical_class), size = 3, family = font) +
  #geom_smooth(aes(colour = lexical_class), method = "lm", se = FALSE) +
  geom_smooth(colour = "black", method = "lm", se = FALSE) +
  geom_text(aes(label = sprintf("bar(R)^2==%.2f", rsq)), parse = TRUE,
            x = 0, y = 1, family = font,
            data = filter(gender_diff_models, lexical_class == "Nouns")) +
  scale_colour_solarized(guide = FALSE) +
  #xlab("Mutual information between girl-directed & boy-directed speech (bits)") +
  ylab("Area between girls' & boys' growth curves")

outliers <- filter(plt_gender_diffs, form_measure == "WS_produces", outlier)$definition
gender_diff_freqs_ws <- gender_diff_freqs %>%
  filter(form_measure == "WS_produces") %>%
  mutate(outlier = item %in% outliers)

empty <- ggplot() + theme(panel.border = element_blank())

scatter <- ggplot(gender_diff_freqs_ws,
                  aes(x = mut_inf, y = area_diff)) +
  geom_point(aes(colour = lexical_class),
             data = filter(gender_diff_freqs_ws, !outlier),
             size = 0.5, alpha = 0.5) +
  geom_label(aes(colour = lexical_class, label = label),
             data = filter(gender_diff_freqs_ws, outlier),
             position = position_jitter(), size = 2) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  scale_colour_solarized(name = "") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1),
        legend.key = element_blank())

y_dist <- ggplot(gender_diff_freqs_ws, aes(x = area_diff)) + #, fill = lexical_class)) +
  coord_flip() +
  geom_density(alpha = 0.5) +
  #scale_fill_solarized(guide = FALSE) +
  theme(axis.title.y = element_blank())

x_dist <- ggplot(gender_diff_freqs_ws, aes(x = mut_inf)) + #, fill = lexical_class)) +
  geom_density(alpha = 0.5) +
  # scale_fill_solarized(guide = FALSE) +
  theme(axis.title.x = element_blank())

grid.arrange(x_dist, empty, scatter, y_dist,
             ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))



gathered_gender_diff_freqs_ws <- gender_diff_freqs_ws %>%
  filter(lexical_class == "Nouns") %>%
  mutate(female_freq = log(female_count / sum(female_count)),
         male_freq = log(male_count / sum(male_count))) %>%
  select(lexical_class, item, label, outlier, female_area, male_area, female_freq, male_freq) %>%
  gather(key, value, female_area, male_area, female_freq, male_freq) %>%
  extract(key, c("gender", "variable"), "([[:alnum:]]+)_([[:alnum:]]+)") %>%
  spread(variable, value)

scatter <- ggplot(gathered_gender_diff_freqs_ws,
                  aes(x = freq, y = area, colour = gender)) +
  geom_point(data = filter(gathered_gender_diff_freqs_ws, !outlier),
             size = 0.5, alpha = 0.5) +
  geom_label(aes(label = label),
             data = filter(gathered_gender_diff_freqs_ws, outlier),
             position = position_jitter(), size = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_colour_solarized(name = "") +
  theme(legend.position = c(0, 1), legend.justification = c(0, 1),
        legend.key = element_blank())

y_dist <- ggplot(gathered_gender_diff_freqs_ws, aes(x = area, fill = gender)) +
  coord_flip() +
  geom_density(alpha = 0.5) +
  scale_fill_solarized(guide = FALSE) +
  theme(axis.title.y = element_blank())

x_dist <- ggplot(gathered_gender_diff_freqs_ws, aes(x = freq, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_solarized(guide = FALSE) +
  theme(axis.title.x = element_blank())

grid.arrange(x_dist, empty, scatter, y_dist,
             ncol = 2, nrow = 2, widths = c(4, 1), heights = c(1, 4))

comp_diff_model <- lm(area_diff ~ mut_inf + lexical_class,
                      data = gender_diff_freqs %>% filter(measure == "understands"))
summary(comp_diff_model)

prod_diff_model <- lm(area_diff ~ mut_inf + lexical_class,
                      data = gender_diff_freqs %>% filter(measure == "produces"))
summary(prod_diff_model)

gender_diff_freqs %>%
  filter(measure == "understands") %>%
  mutate(fit = predict(diff_model, newdata = .),
         error = abs(area_diff - fit)) %>%
  select(measure, item, area_diff, freq_diff, fit, error) %>%
  View()




freq_comp <- uni_freqs %>%
  filter(language == "English") %>%
  ungroup() %>%
  select(lexical_category, uni_lemma, frequency) %>%
  rename(item = uni_lemma) %>%
  right_join(gender_props_freqs) %>%
  mutate(old_freq = log(frequency),
         mean_freq = (female_freq + male_freq) / 2,
         freq_diff = abs(mean_freq - old_freq))

ggplot(freq_comp, aes(x = old_freq, y = female_freq)) +
  geom_point()

gender_props_freqs %>%
  filter(!is.na(female_freq)) %>%
  ungroup() %>%
  select(female_prop, female_freq) %>%
  cor()

freq_comp %>%
  filter(female_freq > -14) %>%
  ggplot(aes(x = female_freq, y = female_prop)) +
  facet_wrap(~lexical_category) +
  geom_smooth(method = "lm") +
  geom_point()

ggplot(freq_comp, aes(x = female_freq, y = female_prop)) +
  geom_point()

lm(female_prop ~ female_freq, data = gender_props_freqs) %>% summary()
lm(female_prop ~ male_freq, data = gender_props_freqs) %>% summary()
lm(female_prop ~ female_freq + male_freq, data = gender_props_freqs) %>% summary()

nouns <- gender_props_freqs %>% filter(lexical_category == "nouns")
lm(female_prop ~ female_freq, data = nouns) %>% summary()
lm(female_prop ~ male_freq, data = nouns) %>% summary()
lm(female_prop ~ female_freq + male_freq, data = nouns) %>% summary()

cor(nouns$female_prop, nouns$female_freq, use = "complete.obs")
cor(nouns$female_prop, nouns$male_freq, use = "complete.obs")
cor(nouns$male_prop, nouns$male_freq, use = "complete.obs")
cor(nouns$male_prop, nouns$female_freq, use = "complete.obs")

lm(male_prop ~ female_freq, data = gender_props_freqs) %>% summary()
lm(male_prop ~ male_freq, data = gender_props_freqs) %>% summary()
