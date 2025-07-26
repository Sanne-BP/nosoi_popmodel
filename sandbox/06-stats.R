#Doing the statistics for script 05-spillover_testing_singleDiscrete.R

#Using the metadata frame for respiratory disease spillover from humans to mountain gorillas with 3 connectivity scenarios: fragmented, connected, restored + 2 sociality behaviours (high and low)
View(all_metadata)

#-------------------------------------------------------------------------------------------------
#Starting off with the stacked bar plot that shows which epidemic simulations worked (spillover versus no spillover)
ggplot(plot_data, aes(x = connectivity, y = percent, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_fill_manual(values = c("fail" = "#f0f0f0", "success" = "#636363"),
                    labels = c("No Spillover", "Spillover")) +
  labs(x = "Connectivity",
       y = "Percentage of Simulations",
       fill = "Simulation Result") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())

#Using dataframe that only includes relevant columns:
View(plot_data)

spillover_table <- plot_data |>
  select(connectivity, status, n) |>
  pivot_wider(names_from = status, values_from = n)

#convert to matrix
spillover_matrix <- as.matrix(spillover_table[, c("fail", "success")])
rownames(spillover_matrix) <- spillover_table$connectivity

#Performing the Chi-squared test;
chisq.test(spillover_matrix)

#Pearson's Chi-squared test
#data:  spillover_matrix
#X-squared = 4.7377, df = 2, p-value = 0.09359
#Warning message: In chisq.test(spillover_matrix) : Chi-squared approximation may be incorrect
#SO: no statistically significant difference in spillover frequency across the three connectivity scenarios. p-value of 0.094 could suggest a possible trend, but we are working with really small values
#-> therefore we can also use the fisher.test:
fisher.test(spillover_matrix)

#Fisher's Exact Test for Count Data
#data:  spillover_matrix
#p-value = 0.1613
#alternative hypothesis: two.sided

#SO: no statistically significant difference in spillover frequency across the three connectivity scenarios. p-value of 0.1613. So any differences observed in spillover rates between scenarios could reasonably due to chance, the stochasticity of the model!





#-------------------------------------------------------------------------------------------------
#now looking at the amount of gorilla infections in different landscape scenarios (after filtering only the succesfull epidemics): test whether total gorilla infections differ across connectivity scenarios (fragmented, connected, restored), separately for each level of gorilla sociality (high and low)
ggplot(successful_runs, aes(x = connectivity, y = total_gorilla_infections,
                            fill = gorilla_sociality)) +
  geom_boxplot(alpha=0.6) +
  theme_minimal(base_size = 11) +
  labs(title = "Gorilla Infections in Different Landscape Scenarios",
       y = "Total Gorilla Infections",
       x = "Connectivity Scenario (n = 200 runs)",
       fill = "Gorilla Sociality") +
  scale_fill_viridis_d(option = "viridis")

str(successful_runs)

#split the data
high_sociality <- successful_runs |>
  filter(gorilla_sociality == "high")

low_sociality <- successful_runs |>
  filter(gorilla_sociality == "low")

#ANOVA model
aov_high <- aov(total_gorilla_infections ~ connectivity, data = high_sociality)

# Check residuals
plot(aov_high, which = 2)  # QQ plot
shapiro.test(resid(aov_high))  # Normality test

#ANOVA assumption of normality is violated, so lets use the Kruskal-Wallis test instead!
kruskal.test(total_gorilla_infections ~ connectivity, data = high_sociality)
#Kruskal-Wallis rank sum test
#Kruskal-Wallis chi-squared = 169.13, df = 2, p-value < 2.2e-16.

kruskal.test(total_gorilla_infections ~ connectivity, data = low_sociality)
#Kruskal-Wallis rank sum test
#Kruskal-Wallis chi-squared = 195.83, df = 2, p-value < 2.2e-16


#Pairwise comparison:
pairwise.wilcox.test(
  x = high_sociality$total_gorilla_infections,
  g = high_sociality$connectivity,
  p.adjust.method = "holm"
)

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction
#data:  high_sociality$total_gorilla_infections and high_sociality$connectivity
#fragmented connected
#connected <2e-16     -
#  restored  <2e-16     <2e-16
#P value adjustment method: holm

pairwise.wilcox.test(
  x = low_sociality$total_gorilla_infections,
  g = low_sociality$connectivity,
  p.adjust.method = "holm"
)

#Pairwise comparisons using Wilcoxon rank sum test with continuity correction
#data:  low_sociality$total_gorilla_infections and low_sociality$connectivity
#fragmented connected
#connected <2e-16     -
#  restored  <2e-16     <2e-16
#P value adjustment method: holm


#Within scenario testing:
wilcox.test(
  total_gorilla_infections ~ gorilla_sociality,
  data = subset(successful_runs, connectivity == "fragmented")
)

#Wilcoxon rank sum test with continuity correction
#data:  total_gorilla_infections by gorilla_sociality
#W = 6985.5, p-value = 2.231e-07
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(
  total_gorilla_infections ~ gorilla_sociality,
  data = subset(successful_runs, connectivity == "connected")
)

#Wilcoxon rank sum test with continuity correction
#data:  total_gorilla_infections by gorilla_sociality
#W = 8142.5, p-value = 3.786e-15
#alternative hypothesis: true location shift is not equal to 0

wilcox.test(
  total_gorilla_infections ~ gorilla_sociality,
  data = subset(successful_runs, connectivity == "restored")
)

#Wilcoxon rank sum test with continuity correction
#data:  total_gorilla_infections by gorilla_sociality
#W = 8052.5, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

#Showing this all in one plot:
library(ggplot2)
library(ggpubr)

#facet by connectivity:
ggplot(successful_runs, aes(x = gorilla_sociality, y = total_gorilla_infections)) +
  geom_boxplot(aes(fill = gorilla_sociality), alpha=0.6) +
  facet_wrap(~ connectivity) +
  theme_bw(base_size = 11) +
  labs(
       y = "Total Gorilla Infections",
       x = "Sociality Behaviour") +
  scale_fill_viridis_d(option = "viridis") +
  theme(legend.position = "none")

sig_labels <- data.frame(
  connectivity = factor(c("fragmented", "connected", "restored"),
                        levels = levels(successful_runs$connectivity)),
  x = 1.5,   # middle between "low" and "high" (assuming 2 levels)
  y = c(500, 500, 500),
  label = "***"
)

ggplot(successful_runs, aes(x = gorilla_sociality, y = total_gorilla_infections, fill = gorilla_sociality)) +
  geom_boxplot(aes(fill = gorilla_sociality), alpha = 0.6) +
  facet_wrap(~ connectivity) +
  theme_bw(base_size = 11) +
  labs(
    y = "Total Gorilla Infections",
    x = "Sociality Behaviour"
  ) +
  scale_fill_viridis_d(option = "viridis") +
  geom_text(data = sig_labels,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            size = 6,
            color = "black")

ggsave("sandbox/plots_report/STATS1.png",  width = 7, height = 4, units = "in", dpi = 300, bg = "white")






#------------------------------------------------------------------------------------------------
#What is the spillover time, so when disease spills over from humans to MG?
ggplot(successful_runs, aes(x = gorilla_sociality, y = first_spillover_time,
                            fill = gorilla_sociality)) +
  geom_boxplot(alpha = 0.6) +
  facet_wrap(~connectivity)+
  theme_bw(base_size = 11) +
  labs(
    y = "Time of First Spillover (days)",
    x = "Sociality Behaviour",
    fill = "Sociality") +
  scale_fill_viridis_d(option = "viridis")

#checking normality:
shapiro.test(resid(aov(first_spillover_time ~ connectivity, data = subset(successful_runs, gorilla_sociality == "high"))))

shapiro.test(resid(aov(first_spillover_time ~ connectivity, data = subset(successful_runs, gorilla_sociality == "low"))))

#Normality is violated, so we use the Kruskal-Wallis test:

#between connectivity:
kruskal.test(first_spillover_time ~ connectivity, data = subset(successful_runs, gorilla_sociality == "high"))
#Kruskal-Wallis rank sum test
#Kruskal-Wallis chi-squared = 26.613, df = 2, p-value = 1.663e-06

kruskal.test(first_spillover_time ~ connectivity, data = subset(successful_runs, gorilla_sociality == "low"))
#Kruskal-Wallis rank sum test
#Kruskal-Wallis chi-squared = 25.332, df = 2, p-value = 3.156e-06

#POST-HOC:
pairwise.wilcox.test(
  x = subset(successful_runs, gorilla_sociality == "high")$first_spillover_time,
  g = subset(successful_runs, gorilla_sociality == "high")$connectivity,
  p.adjust.method = "holm"
)
#Pairwise comparisons using Wilcoxon rank sum test with continuity correction
#fragmented connected
#connected 0.0538     -
#  restored  1.8e-06    0.0015

pairwise.wilcox.test(
  x = subset(successful_runs, gorilla_sociality == "low")$first_spillover_time,
  g = subset(successful_runs, gorilla_sociality == "low")$connectivity,
  p.adjust.method = "holm"
)

#fragmented connected
#connected 0.0015     -
#  restored  2.8e-06    0.1101

#WITHIN connectivity
wilcox.test(first_spillover_time ~ gorilla_sociality, data = subset(successful_runs, connectivity == "fragmented"))
#Wilcoxon rank sum test with continuity correction
#W = 5379, p-value = 0.2308

wilcox.test(first_spillover_time ~ gorilla_sociality, data = subset(successful_runs, connectivity == "connected"))
#W = 5911, p-value = 0.01691

wilcox.test(first_spillover_time ~ gorilla_sociality, data = subset(successful_runs, connectivity == "restored"))
#W = 4806, p-value = 0.7935

ggsave("sandbox/plots_report/STATS2.png",  width = 7, height = 4, units = "in", dpi = 300, bg = "white")





#------------------------------------------------------------------------------------------------
#now: disease spreads from MG 1 to MG 2
ggplot(all_metadata, aes(x = connectivity, y = spillover_MG1_to_MG2_time,
                         fill = gorilla_sociality)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Time of Gorilla1 → Gorilla2 Spillover by Connectivity and Sociality",
    x = "Connectivity Scenario (n = 200 runs)",
    y = "Time of First Spillover",
    fill = "Gorilla Sociality") +
  scale_x_discrete(limits = c("fragmented", "connected", "restored"))+
  scale_fill_viridis_d(option = "viridis")

#WELL... actually spillover only occured in 6 out of 600 simulations, so not sure whether doing statistics on this actually yields some meaningful results...
