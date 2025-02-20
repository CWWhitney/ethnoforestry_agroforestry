# # Define keywords for practices and outcomes
# hard to make.. balance comprehensive with lengthy
# Group practices and outcomes for aggregation
practices <- list(
  Agroforestry = c("agroforest|ri|ry)?", "silvo(pastoral|cultural)?", 
                   "eco(agriculture|forestry)?", "agro-silvo(pastoral|cultural)?", 
                   "alley( cropping)?", 
                   "windbreak(s)?", "shelterbelt(s)?", "multistrat(a|as)?"),
  Community_Forests = c("commun(iti|ities|ity|al)?", "village(s|rs)?", "community( forest| forestry| managed)?",
                        "community-based( forest| tree)?", "communal lands", "tradition(al| ecological knowledge)?", "indig(enous|eneity)?", 
                        "ethno(forestry|ecology|botany)?", "local( knowledge)?"),
  Silviculture = c("silvi(cultural|culture)?", "timber( management| harvesting)?", 
                   "woodlot(s)?", 
                   "plantat(ion|ions)?", "managed forest(s)?", "forest(ry| stand(s)?)?", "secondary forest(s)?"),
  Urban_Forests = c("urban( forest| forestry)?", "city( greening)?", "green( belt| space)?", 
                    "peri-urban", "crypto(forest|forestry)?"),
  Conservation = c("forest restor(ation)?", "natural regeneration", 
                   "recreation", "liesure", "habitat restor(ation)?", "national park", 
                   "park", "protected")
)

outcomes <- list(
  Economic = c("income(s| generation)?", "profit(s|ability)?", "economic( benefit|s)?", 
               "financial( gain| benefit| revenue| revenues)?", "market access", 
               "trade", "subsist(ence|ance)?", "employment", "job(s)?"),
  Biodiversity = c("biodiv(erse|ersity)?", "species( richness| diversity)?", 
                   "habitat(s| connectivity)?", "ecosystem(s| services| integrity)?", 
                   "wildlife", "conserv(e|ation)?", "native( species)?", "reforest(ation)?", 
                   "afforest(ation)?"),
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


# Create a data frame for associations
association_df <- expand.grid(
  PracticeGroup = names(practices),
  OutcomeGroup = names(outcomes),
  Count = 0,
  BibKeys = "",
  stringsAsFactors = FALSE
)

# Convert corpus to plain text for analysis
text_data <- sapply(corpus, as.character)

# Analyze associations without double-counting within each paper
for (i in seq_along(text_data)) {
  # Initialize a set to track counted associations for the current paper
  counted_associations <- list()
  
  for (practice_group in names(practices)) {
    for (outcome_group in names(outcomes)) {
      # Check for presence of terms from practice and outcome groups
      practice_found <- any(grepl(paste(practices[[practice_group]], collapse = "|"), 
                                  text_data[i], ignore.case = TRUE))
      outcome_found <- any(grepl(paste(outcomes[[outcome_group]], collapse = "|"), 
                                 text_data[i], ignore.case = TRUE))
      
      # Count association only once per paper
      if (practice_found && outcome_found) {
        association_key <- paste(practice_group, outcome_group, sep = "_")
        
        if (!(association_key %in% counted_associations)) {
          # Increment count only if not already counted in this paper
          association_df$Count[association_df$PracticeGroup == practice_group & 
                                 association_df$OutcomeGroup == outcome_group] <- 
            association_df$Count[association_df$PracticeGroup == practice_group & 
                                   association_df$OutcomeGroup == outcome_group] + 1
          
          # Add BibKey if not already present
          relevant_key <- bib_keys[i]
          current_keys <- association_df$BibKeys[association_df$PracticeGroup == practice_group & 
                                                   association_df$OutcomeGroup == outcome_group]
          if (!grepl(relevant_key, current_keys)) {
            association_df$BibKeys[association_df$PracticeGroup == practice_group & 
                                     association_df$OutcomeGroup == outcome_group] <- 
              paste(current_keys, relevant_key, sep = "; ")
          }
          
          # Mark this association as counted for this paper
          counted_associations <- c(counted_associations, association_key)
        }
      }
    }
  }
}

