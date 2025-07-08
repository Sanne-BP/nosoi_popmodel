#continuing based on the metadata produced, but now within testing of the fragmented habitat situation, so low in between movement of patches

str(all_metadata)
str(summary_stats)

#now filtering for the fragmented scenario
fragmented_metadata <- all_metadata |>
  filter(connectivity == "fragmented")

str(fragmented_metadata)

ggplot(fragmented_metadata, aes(x = gorilla_sociality, y = total_gorilla_infections, fill = gorilla_sociality)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Within Fragmented Habitat: Gorilla Infections by Sociality",
       y = "Total Gorilla Infections",
       x = "Gorilla Sociality")
