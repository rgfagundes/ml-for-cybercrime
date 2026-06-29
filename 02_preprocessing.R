# Load packages
library(dplyr)
library(stringr)
library(tidytext)
library(quanteda)
library(textstem)


# Load dataset: either 'CyberJustice-TJSP-Decisions' or the output from the data extraction step
tjsp <- read.csv("decisions.csv", header = TRUE, sep = ",")

# Remove rows with blank "julgado" column and duplicate rows
tjsp_clean <- tjsp |>
  filter(!is.na(julgado)) |>
  distinct(processo, .keep_all = TRUE)


# Subjects manually identified as non-relevant (see data/removed_subjects.csv)
subjects_to_remove <- read.csv("data/removed_subjects.csv", sep = ";")$assunto

# Replace quotation marks in the "Lavagem" subject entry
tjsp_clean$assunto <- str_replace(
  tjsp_clean$assunto,
  '"Lavagem" ou Ocultação de Bens, Direitos ou Valores Oriundos de Corrupção',
  'Lavagem ou Ocultação de Bens, Direitos ou Valores Oriundos de Corrupção'
)

# Remove non-relevant subjects
tjsp_clean <- tjsp_clean |>
  filter(!assunto %in% subjects_to_remove)


# Build pattern lists for rapporteur names, cities, and adjudicating bodies
rapporteur_names <- paste(str_to_lower(unique(tjsp_clean$relator)), collapse = "|")
city_names       <- paste(str_to_lower(unique(tjsp_clean$comarca)), collapse = "|")
body_names       <- paste(str_to_lower(unique(tjsp_clean$orgao_julgador)), collapse = "|")

# Text preprocessing
tjsp_clean$ruling_processed <- str_to_lower(tjsp_clean$julgado)
tjsp_clean$ruling_processed <- sub(".*?(acórdão)", " ", tjsp_clean$ruling_processed)

tjsp_clean <- tjsp_clean |>
  mutate(ruling_processed = ruling_processed |>
           str_replace_all("poder judiciário\\s+tribunal de justiça (do estado de são paulo|do estado)?", "") |> # Remove terms from page headers
           str_replace_all("tribunal de justiça\\s+poder judiciário", "") |>                                     # Remove terms from page headers
           str_replace_all("assinatura eletrônica", "") |>              # Remove electronic signature terms
           str_replace_all("\\n", " ") |>                               # Remove newline characters
           str_replace_all(rapporteur_names, "") |>                     # Remove rapporteur names
           str_replace_all(city_names, "") |>                           # Remove city names
           str_replace_all(body_names, "") |>                           # Remove adjudicating body names
           str_replace_all("\\b\\d{1,2}[-/.]\\d{1,2}[-/.]\\d{2,4}\\b", " ") |> # Remove dates (dd/mm/yyyy)
           str_replace_all("[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.(com|com\\.br)\\b", " ") |> # Remove email addresses
           str_replace_all("https?://\\S+|www\\.\\S+", " ") |>         # Remove URLs
           str_replace_all("[[:punct:]]", " ") |>                       # Remove punctuation
           str_replace_all("º|ª|°", "") |>                              # Remove ordinal indicators
           str_replace_all("[0-9]", "") |>                              # Remove numbers
           str_replace_all("\\b[A-Za-z]\\b", " ") |>                   # Remove isolated single letters
           str_replace_all("\\s+", " ") |>                              # Remove extra whitespace
           str_squish())                                                 # Strip leading and trailing whitespace


# Tokenize
tokens <- tjsp_clean |>
  unnest_tokens(word, ruling_processed)


# Remove stopwords
# Build stopwords dataframe
stop_w <- tibble(word = stopwords("pt"))

additional_stopwords <- c("auto", "acórdão", "decisão", "voto", "relatados",
                          "comarca", "vistos", "discutidos", "julgamento",
                          "tribunal", "acordam", "participação", "proferir",
                          "desembargadores", "autos", "justiça", "seguinte",
                          "presidente", "integra", "conformidade", "relator",
                          "relatório", "art", "artigo", "disse", "havia",
                          "tendo", "sendo", "dia", "dizer", "ação", "processo",
                          "declaração", "recurso", "processo", "apelação", "fls")

# Terms manually identified as non-relevant (see data/removed_terms.csv)
terms_to_remove <- read.csv("removed_terms.csv", sep = ",")$word

stop_w <- tibble(
  word = unique(c(
    stop_w$word,
    additional_stopwords,
    terms_to_remove
  ))
)

tokens <- tokens |>
  anti_join(stop_w)



# Compute TF-IDF
tf_idf <- tokens |>
  count(cdacordao, word, sort = TRUE) |> # Count word frequencies
  bind_tf_idf(word, cdacordao, n)        # Compute TF-IDF scores