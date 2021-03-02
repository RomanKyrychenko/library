## ----setup, include=FALSE------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ------------------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidytext) # text tokenization
  library(stringr) # text transformations
  library(stringdist) # word distances
  library(text2vec) # document matrix and similarities
  library(dplyr) # data manipulations
  library(fuzzyjoin) # join using word distances
  library(readr) # read and write files
  library(readxl) # read excel files
  library(glmnet) # logistic regression
})


## ------------------------------------------------------------------------------------
text <- read_csv("https://raw.githubusercontent.com/RomanKyrychenko/library/master/data/pravda_sample.csv")

text %>% head()


## ------------------------------------------------------------------------------------
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(text$text, 
             preprocessor = prep_fun, 
             tokenizer = tok_fun, 
             progressbar = FALSE)
vocab = create_vocabulary(it_train)

head(vocab %>% arrange(desc(doc_count)))


## ------------------------------------------------------------------------------------
tokenized_text <- text %>%
  unnest_tokens(word, text)


## ------------------------------------------------------------------------------------
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

dim(dtm_train)


## ------------------------------------------------------------------------------------
vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm_train, tfidf)


## ------------------------------------------------------------------------------------
methods <- c("osa", "lv", "lcs", "cosine", "jaccard")

for (i in methods){
  cat(paste0(i, ":\t", stringdist("патріот", "патріотизм", method = i), "\n"))
}


## ------------------------------------------------------------------------------------
d1_d2_jac_sim = sim2(dtm_train, dtm_train, method = "jaccard", norm = "none")


## ------------------------------------------------------------------------------------
most_similar_text <- function(index, sim_matrix = d1_d2_jac_sim){
  cols <- 1:ncol(sim_matrix)
  sims <- sim_matrix[index, cols[cols != index]]
  cat("Similarity: ", max(sims))
  cat("\n")
  cat(text[index, 3][[1]])
  cat("\n")
  cat("\n")
  cat(text[as.numeric(names(which.max(sims))), 3][[1]])
}

most_similar_text(3)


## ------------------------------------------------------------------------------------
d1_d2_cos_sim = sim2(dtm_train_tfidf, dtm_train_tfidf, method = "cosine", norm = "l2")


## ------------------------------------------------------------------------------------
most_similar_text(2, d1_d2_cos_sim)


## ------------------------------------------------------------------------------------
tone_dict <- read_delim("https://raw.githubusercontent.com/lang-uk/tone-dict-uk/master/tone-dict-uk.tsv", delim = "\t", col_names = F) %>% 
  setNames(c("word", "tone"))

head(tone_dict)


## ------------------------------------------------------------------------------------
sent <- tokenized_text %>% 
  mutate(word = str_to_lower(word)) %>% 
  left_join(tone_dict) %>% 
  mutate(tone = ifelse(is.na(tone), 0, tone)) %>% 
  group_by(url, title) %>% 
  summarise(tone = mean(tone)) %>% 
  arrange(desc(tone))


## ------------------------------------------------------------------------------------
sent %>% 
  mutate(year = as.numeric(sapply(url, function(x) str_split(x, "/")[[1]][6]))) %>% 
  group_by(year) %>% 
  summarise(tone = mean(tone)*100, n = n())


## ------------------------------------------------------------------------------------
sent <- tokenized_text %>% 
  mutate(word = str_to_lower(word)) %>% 
  fuzzyjoin::stringdist_left_join(tone_dict, by = "word", method="cosine", max_dist = 0.1) %>% 
  mutate(tone = ifelse(is.na(tone), 0, tone)) %>% 
  group_by(url, title) %>% 
  summarise(tone = mean(tone)) %>% 
  arrange(desc(tone))


## ------------------------------------------------------------------------------------
sent %>% 
  mutate(year = as.numeric(sapply(url, function(x) str_split(x, "/")[[1]][5]))) %>% 
  group_by(year) %>% 
  summarise(tone = mean(tone)*100, n = n())


## ------------------------------------------------------------------------------------
#own path to excel file
sent_data <- read_excel("group_1.xlsx") %>% 
  select(text, `Hate Speech Detection`)


## ------------------------------------------------------------------------------------
prep_fun = tolower
tok_fun = word_tokenizer

it_train = itoken(sent_data$text, 
             preprocessor = prep_fun, 
             tokenizer = tok_fun, 
             progressbar = FALSE)
vocab = create_vocabulary(it_train)
vectorizer = vocab_vectorizer(vocab)
dtm_train = create_dtm(it_train, vectorizer)

tfidf = TfIdf$new()
dtm_train_tfidf = fit_transform(dtm_train, tfidf)


## ------------------------------------------------------------------------------------
glmnet_classifier = cv.glmnet(x = dtm_train_tfidf, 
                              y = sent_data$`Hate Speech Detection`, 
                              family = 'binomial', 
                              alpha = 1,
                              type.measure = "auc",
                              nfolds = 5,
                              thresh = 1e-3,
                              maxit = 1e3)


## ------------------------------------------------------------------------------------
print(paste("max AUC =", round(max(glmnet_classifier$cvm), 4)))

