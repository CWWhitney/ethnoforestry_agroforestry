library(RefManageR)
library(tm)
library(SnowballC)

# Load the .bib file
bib_file <- "refs/scopus.bib"
bib_entries <- ReadBib(bib_file)

# excluding all conference papers

# Save bib keys 
bib_keys <- names(bib_entries)
# write.csv(bib_keys, "data/bib_keys.csv", row.names = FALSE) # large file

# Extract relevant fields: titles, abstracts, and keywords
titles <- sapply(bib_entries, function(x) x$title)
abstracts <- sapply(bib_entries, function(x) x$abstract)
keywords <- sapply(bib_entries, function(x) x$keywords)

# Combine all text fields into one corpus per entry
all_text <- mapply(function(t, a, k) paste(t, a, k, sep = " "), titles, abstracts, keywords, SIMPLIFY = FALSE)

# Create a corpus
corpus <- Corpus(VectorSource(all_text))

# Preprocess the text
corpus <- corpus %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace) 

# Preprocessing with logging
# check for dropped papers
source("functions/log_dropped_docs.R")
corpus <- Corpus(VectorSource(all_text))

corpus <- log_dropped_docs(corpus, content_transformer(tolower), "Convert to lowercase")
corpus <- log_dropped_docs(corpus, removePunctuation, "Remove punctuation")
corpus <- log_dropped_docs(corpus, removeNumbers, "Remove numbers")
# corpus <- log_dropped_docs(corpus, removeWords, stopwords("en"), "Remove stopwords")
corpus <- log_dropped_docs(corpus, stemDocument, "Apply stemming")

