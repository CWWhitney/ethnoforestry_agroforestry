library(RefManageR)
library(tm)
library(SnowballC)
library(NLP)

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
  tm_map(stemDocument) %>%  # Add stemming
  tm_map(stripWhitespace)

