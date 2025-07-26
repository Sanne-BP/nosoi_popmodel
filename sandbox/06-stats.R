#doing some statistic on the metadata

str(all_metadata)
str(summary_stats)

library(dplyr)
library(MASS)

ggplot(all_metadata, aes(x = connectivity, y = total_gorilla_infections,
                         fill = gorilla_sociality)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Gorilla Infections in different landscape scenarios",
       y = "Total Gorilla Infections",
       x = "Connectivity Scenario") +
  scale_fill_viridis_d(option = "viridis")


# Fit Poisson model
poisson_mod <- glm(total_gorilla_infections ~ connectivity * gorilla_sociality,
                   family = poisson(link = "log"),
                   data = all_metadata)

# Check for overdispersion
dispersion <- sum(residuals(poisson_mod, type = "pearson")^2) / poisson_mod$df.residual
print(paste("Dispersion:", dispersion))

nb_mod <- glm.nb(total_gorilla_infections ~ connectivity * gorilla_sociality,
                 data = all_metadata)

summary(nb_mod)

anova(nb_mod, test = "Chisq")

