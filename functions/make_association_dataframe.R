# Assess associations between forest management practices and outcomes. 
# Start with term frequency data and bibliographic entries.

source("functions/create_corpus.R")

# # Define keywords for practices and outcomes
# hard to make.. balance comprehensive with lengthy
# Group practices and outcomes for aggregation
practices <- list(
  Agroforestry = c("agro(forest|forestry)?", "silvo(pastoral|cultural)?", 
                   "agro-silvo(pastoral|cultural)?", "alley( cropping)?", 
                   "windbreak(s)?", "shelterbelt(s)?", "multistrat(a|as)?"),
  Community_Forests = c("commun(ity|al)?", "village(s|rs)?", "community( forest| forestry| managed)?",
                       "community-based( forest| tree)?", "communal lands"),
  Silviculture = c("silvi(cultural|culture)?", "timber( management| harvesting)?", 
                   "plantat(ion|ions)?", "managed forest(s)?", "reforest(ation)?", 
                   "afforest(ation)?", "forest(ry| stand(s)?)?", "secondary forest(s)?"),
  Urban_Forests = c("urban( forest| forestry)?", "city( greening)?", "green( belt| space)?", 
                   "peri-urban", "crypto(forest|forestry)?"),
  Traditional_Knowledge = c("tradition(al| ecological knowledge)?", "indig(enous|eneity)?", 
          "ethno(forestry|ecology|botany)?", "local( knowledge)?"),
  Managing_Trees = c("tree( planting| management| care)?", "woodlot(s)?", 
                            "degraded( forest| land)?", "forest restor(ation)?", 
                            "natural regeneration", "eco(agriculture|forestry)?")
)

outcomes <- list(
  Economic = c("income(s| generation)?", "profit(s|ability)?", "economic( benefit|s)?", 
               "financial( gain| benefit| revenue| revenues)?", "market access", 
               "trade", "subsist(ence|ance)?", "employment", "job(s)?"),
  Biodiversity = c("biodiv(erse|ersity)?", "species( richness| diversity)?", 
                   "habitat(s| connectivity)?", "ecosystem(s| services| integrity)?", 
                   "wildlife", "conservation", "native( species)?"),
  Carbon = c("carbon( sequestration)?", "climate( mitigation| adaptation)?", 
             "greenhouse( gas| gases)?", "soil( carbon| fertility)?", 
             "emissions reduction", "water( retention| conservation)?"),
  Nutrition = c("nutrit(ion|ional security)?", "food( security| systems)?", 
                "diet( diversity|ary diversity)?", "malnutrit(ion)?", 
                "stunt(ing|ed)?", "healthy( diet| foods)?", "food(s| environment)?"),
  Livelihoods = c("liveli(hood|hoods)?", "poverty( alleviation| reduction)?", 
                  "resilience", "sustainability", "rural( development| livelihoods)?", 
                  "well-being", "gender( equality| equity)?", "autonomy"),
  Ecosystem_Services = c("ecosystem(s| services)?", "provisioning", "cultural(value| heritage)?", 
                        "supporting", "regulating", "biodiversity( hotspot)?")
)


# Create a data frame to track associations
association_df <- expand.grid(
  PracticeGroup = names(practices),
  OutcomeGroup = names(outcomes),
  Count = 0,
  BibKeys = "",
  stringsAsFactors = FALSE
)

# Convert the corpus to a plain text format for analysis
library(tm)
text_data <- sapply(corpus, as.character)

# Analyze co-occurrences by group
for (i in seq_along(text_data)) {
  for (practice_group in names(practices)) {
    for (outcome_group in names(outcomes)) {
      if (any(grepl(paste(practices[[practice_group]], collapse = "|"), 
                    text_data[i], ignore.case = TRUE)) &&
          any(grepl(paste(outcomes[[outcome_group]], collapse = "|"), 
                    text_data[i], ignore.case = TRUE))) {
        
        # Increment count
        association_df$Count[association_df$PracticeGroup == practice_group & 
                              association_df$OutcomeGroup == outcome_group] <- 
          association_df$Count[association_df$PracticeGroup == practice_group & 
                                association_df$OutcomeGroup == outcome_group] + 1
        
        # Add bib key
        relevant_key <- bib_keys[i]
        current_keys <- association_df$BibKeys[association_df$PracticeGroup == practice_group & 
                                                association_df$OutcomeGroup == outcome_group]
        if (!grepl(relevant_key, current_keys)) {
          association_df$BibKeys[association_df$PracticeGroup == practice_group & 
                                  association_df$OutcomeGroup == outcome_group] <- 
            paste(current_keys, relevant_key, sep = "; ")
        }
      }
    }
  }
}

