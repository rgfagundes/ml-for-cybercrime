# Load packages
library(dplyr)
library(tidytext)
library(tidyr)
library(tm)
library(topicmodels)
library(reshape2)

# Build the document-term matrix
dtm <- tokens |>
  count(cdacordao, word) |>
  cast_dtm(document = cdacordao, term = word, value = n)

# Set the number of topics
num_topics <- 3

# Train the LDA model
lda_model <- LDA(dtm, k = num_topics, control = list(seed = 1234))

# Extract topic-word distributions
topics <- tidy(lda_model, matrix = "beta")

# Retrieve the top terms for each topic
top_terms <- topics |>
  group_by(topic) |>
  top_n(6, beta) |>
  ungroup() |>
  arrange(topic, -beta)

print(top_terms)

# Extract document-topic distributions
doc_topics <- tidy(lda_model, matrix = "gamma")

# Preview results
head(doc_topics)

# Identify the dominant topic for each document
dominant_doc_topics <- doc_topics |>
  group_by(document) |>
  top_n(1, gamma) |>
  ungroup() |>
  arrange(as.numeric(document))

colnames(dominant_doc_topics)[1] <- "cdacordao"
dominant_doc_topics$cdacordao <- as.integer(dominant_doc_topics$cdacordao)

print(dominant_doc_topics)