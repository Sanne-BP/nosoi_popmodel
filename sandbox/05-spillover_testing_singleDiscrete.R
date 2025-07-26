#now to the actual testing and creating of the final results of spillover and spillback between humans and mountain gorillas! (based on the finalized script 04)

#going to run 100 simulations per scenario, where a scenario is a defined combination of:
# - spillover probability
# - contact rates
# - connectivity
#will store each simulation's result in a meta dataframe to be able to compare across / within scenarios



#------------------------------------------------------------------------------------------------
#SPILLOVER PROBABILITY (HUMAN -> MOUNTAIN GORILLA)

#defining different (sub)populations dynamics:
subpop_names <- c("Humans", "Gorilla1", "Gorilla2")
init_pop <- list(Humans = 500000, Gorilla1 = 604, Gorilla2 = 459)
birth_rates <- list(Humans = 0.0096, Gorilla1 = 0.013, Gorilla2 = 0.013)
death_rates <- list(Humans = 0.0096, Gorilla1 = 0.013, Gorilla2 = 0.013)

#movement matrix
transition.matrix <- matrix(
  c(0.98,  0.015,  0.005,
    0.01, 0.975,  0.015,
    0.005, 0.015, 0.98),
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(subpop_names, subpop_names))

#parameters:
#exit rate per (sub)population
p_Exit_fct <- function(t, current.in) {
  if (current.in == "Humans") return(0.000038)
  if (current.in == "Gorilla1") return(0.2)
  if (current.in == "Gorilla2") return(0.1)}

#movement probability per (sub)population
p_Move_fct <- function(t, current.in) {
  if (current.in == "Humans") return(0.2) # mainly movement rangers, tourists, local forest use
  if (current.in == "Gorilla1") return(0.04) # larger range, overlap with human edges
  if (current.in == "Gorilla2") return(0.02) # min movement, more intact core forest
}

#number of contacts per infected host, but making it dependent on the initial population sizes to prevent very high, unrealistic cases of infections
init_pop # <- list(Humans = 10000, Gorilla1 = 200, Gorilla2 = 450) using this!!

n_contact_fct <- function(t, current.in) {
  pop_size <- init_pop[[current.in]]

  #Define a base rate per subpopulation (can be modified)
  base_rate <- switch(current.in,
                      Humans = 10,    # Rangers, tourists, other locals
                      Gorilla1 = 6,  # VM gorilla group (larger, more habituated)
                      Gorilla2 = 4)  # Bwindi group (smaller, more isolated)

  # Cap contacts at base rate if population size sufficient
  threshold <- 30  # Minimum population for full contact saturation
  if (pop_size < threshold) {
    scaled <- round(base_rate * (pop_size / threshold), digits = 0)
    return(max(1, scaled))  # Ensure at least 1 contact if possible
  } else {
    return(base_rate)
  }
}

# For transmission probability, define with incubation and max prob, per host:
p_max_fct <- function(x) rbeta(x, shape1 = 1.28, shape2 = 2)
t_incub_fct <- function(x) rnorm(x, mean = 2.5, sd = 1.5)

proba <- function(t, p_max, t_incub) {
  if (t <= t_incub) p <- 0 else p <- p_max
  return(p)
}

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)




#Now run the wrapper function:
run_sim_and_popmodel <- function(length.sim = 365,
                                 init.pop,
                                 birth.rate,
                                 death.rate,
                                 transition.matrix,
                                 pExit,
                                 pMove,
                                 nContact,
                                 pTrans,
                                 param.pTrans) {

  #Run epidemic simulation with Nosoi
  sim <- nosoiSim(type = "single", popStructure = "discrete",
                  length.sim = length.sim,
                  max.infected = 1100,
                  init.individuals = 1,
                  init.structure = names(init.pop)[1], # Start in first subpop
                  structure.matrix = transition.matrix,
                  pExit = pExit,
                  param.pExit = NA,
                  timeDep.pExit = FALSE,
                  diff.pExit = TRUE,
                  pMove = pMove,
                  param.pMove = NA,
                  timeDep.pMove = FALSE,
                  diff.pMove = TRUE, #true activates subpop-specific pMove
                  nContact = nContact,
                  param.nContact = list(),
                  timeDep.nContact = FALSE,
                  diff.nContact = FALSE,
                  pTrans = pTrans,
                  param.pTrans = param_pTrans,
                  timeDep.pTrans = FALSE,
                  diff.pTrans = FALSE,
                  prefix.host = "H",
                  print.progress = TRUE,
                  print.step = 10)

  # Initialize population size tracking list for each subpop
  PopModel <- list()
  for (loc in names(init.pop)) {
    PopModel[[loc]] <- numeric(length.sim + 1)
    PopModel[[loc]][1] <- init.pop[[loc]]
  }

  # Extract host info dataframe
  hosts_df <- sim$host.info.A$table.hosts

  # Loop over time steps and subpopulations to update population sizes
  for (t in 1:length.sim) {
    for (loc in names(PopModel)) {
      births <- rpois(1, birth.rate[[loc]] * PopModel[[loc]][t])
      deaths <- rbinom(1, PopModel[[loc]][t], death.rate[[loc]])
      epidemic_deaths <- sum(hosts_df$structure == loc & hosts_df$out.time == t & hosts_df$active == FALSE)

      PopModel[[loc]][t + 1] <- max(0, PopModel[[loc]][t] + births - deaths - epidemic_deaths)
    }
  }

  return(list(simulation = sim, population = PopModel))
}

#results:
result <- run_sim_and_popmodel(
  length.sim = 365,
  init.pop = init_pop,
  birth.rate = birth_rates,
  death.rate = death_rates,
  transition.matrix = transition.matrix,
  pExit = p_Exit_fct,
  pMove = p_Move_fct,
  nContact = n_contact_fct,
  pTrans = proba,
  param.pTrans = param_pTrans
)








#creating a metadata frame for respiratory disease spillover from humans to mountain gorillas
#focusing on generating 3 dataframes: fragmented, connected, restored so we can start testing within these datasets + also between!!

# Fragmented (low gorilla connectivity)
transition_fragmented <- matrix(
  c(0.98,  0.015, 0.005,
    0.01, 0.985, 0.005,
    0.005, 0.015, 0.98),
  nrow = 3, byrow = TRUE,
  dimnames = list(subpop_names, subpop_names))

# Connected (medium connectivity)
transition_connected <- matrix(
  c(0.95,  0.03, 0.02,
    0.02,  0.95, 0.03,
    0.01,  0.02, 0.97),
  nrow = 3, byrow = TRUE,
  dimnames = list(subpop_names, subpop_names))

# Restored (high connectivity, corridor)
transition_restored <- matrix(
  c(0.90,  0.05, 0.05,
    0.04,  0.90, 0.06,
    0.03,  0.03, 0.94),
  nrow = 3, byrow = TRUE,
  dimnames = list(subpop_names, subpop_names))

#define contact scenarios (e.g. low and high)
gorilla_contact_rates <- list(
  low = list(Humans = 10, Gorilla1 = 2, Gorilla2 = 1),
  high = list(Humans = 10, Gorilla1 = 6, Gorilla2 = 4)
)

library(tidyr)
library(dplyr)

# Define scenarios:
scenarios <- expand.grid(
  connectivity = c("fragmented", "connected", "restored"),
  gorilla_sociality = c("low", "high"),
  stringsAsFactors = FALSE
)


#sim wrapper that takes these parameters:
run_scenario_sim <- function(connectivity_scenario, gorilla_sociality, n_reps = 100) {

  # Select transition matrix
  transition_matrix <- switch(connectivity_scenario,
                              fragmented = transition_fragmented,
                              connected = transition_connected,
                              restored = transition_restored)

  # Set contact rates for this sociality scenario
  contact_rates <- gorilla_contact_rates[[gorilla_sociality]]

  # Override n_contact_fct to use these rates
  n_contact_fct_scenario <- function(t, current.in) {
    pop_size <- init_pop[[current.in]]
    base_rate <- contact_rates[[current.in]]

    threshold <- 30
    if (pop_size < threshold) {
      scaled <- round(base_rate * (pop_size / threshold), digits = 0)
      return(max(1, scaled))
    } else {
      return(base_rate)
    }
  }

  # Run n_reps simulations for this scenario
  results_list <- vector("list", n_reps)
  for (i in seq_len(n_reps)) {
    results_list[[i]] <- run_sim_and_popmodel(
      length.sim = 365,
      init.pop = init_pop,
      birth.rate = birth_rates,
      death.rate = death_rates,
      transition.matrix = transition_matrix,
      pExit = p_Exit_fct,
      pMove = p_Move_fct,
      nContact = n_contact_fct_scenario,
      pTrans = proba,
      param.pTrans = param_pTrans
    )
  }

  return(results_list)
}

# Run all scenarios and store results:
all_results <- list()

for (i in 1:nrow(scenarios)) {
  cat("Running scenario", i, "of", nrow(scenarios), "\n")
  sc <- scenarios[i, ]
  all_results[[paste0(sc$connectivity, "_", sc$gorilla_sociality)]] <- run_scenario_sim(
    connectivity_scenario = sc$connectivity,
    gorilla_sociality = sc$gorilla_sociality,
    n_reps = 100
  )
}

#extract and compile metadata from all simulations
extract_metadata <- function(sim_result, scenario_name) {
  sim <- sim_result$simulation
  pop <- sim_result$population

  hosts_df <- sim$host.info.A$table.hosts

  # Identify first infection time in gorilla subpopulations (Gorilla1 or Gorilla2)
  gorilla_infections <- hosts_df[hosts_df$current.in %in% c("Gorilla1", "Gorilla2") & hosts_df$inf.time > 0, ]
  first_spillover_time <- if (nrow(gorilla_infections) > 0) min(gorilla_infections$inf.time, na.rm = TRUE) else NA

  # Total gorilla infections
  total_gorilla_infections <- nrow(gorilla_infections)

  # Outbreak duration: max exit time (last recovery or death) across all hosts
  outbreak_duration <- max(hosts_df$out.time, na.rm = TRUE)

  # Total infections across all subpopulations
  total_infections <- nrow(hosts_df)

  # Final population sizes at end of sim
  final_pop_sizes <- sapply(pop, function(x) tail(x, 1))

  # Create a metadata row
  meta <- data.frame(
    scenario = scenario_name,
    first_spillover_time = first_spillover_time,
    total_gorilla_infections = total_gorilla_infections,
    outbreak_duration = outbreak_duration,
    total_infections = total_infections,
    final_pop_humans = final_pop_sizes["Humans"],
    final_pop_gorilla1 = final_pop_sizes["Gorilla1"],
    final_pop_gorilla2 = final_pop_sizes["Gorilla2"]
  )

  return(meta)
}

all_metadata <- lapply(names(all_results), function(scn_name) {
  runs <- all_results[[scn_name]]

  # Extract metadata for each replicate in this scenario
  scenario_meta <- lapply(runs, function(simres) {
    extract_metadata(simres, scn_name)
  })

  do.call(rbind, scenario_meta)
}) %>% do.call(rbind, .)


unique(simulation$host.info.A$table.hosts$structure)

# Add scenario columns back (optional, split by underscore)
all_metadata <- all_metadata %>%
  mutate(
    connectivity = sub("_.*", "", scenario),
    gorilla_sociality = sub(".*_", "", scenario)
  )




#SUMMARIZE results by scenario factors:
summary_stats <- all_metadata |>
  group_by(connectivity, gorilla_sociality) |>
  summarise(
    mean_first_spillover_time = mean(first_spillover_time, na.rm = TRUE),
    mean_total_gorilla_infections = mean(total_gorilla_infections),
    mean_outbreak_duration = mean(outbreak_duration),
    mean_total_infections = mean(total_infections),
    mean_final_pop_humans = mean(final_pop_humans),
    mean_final_pop_gorilla1 = mean(final_pop_gorilla1),
    mean_final_pop_gorilla2 = mean(final_pop_gorilla2),
    .groups = "drop"
  )
print(summary_stats)


#VISUALIZE!
library(ggplot2)
library(viridis)


#making a stacked bar plot to show which epidemic simulations worked:
all_metadata$status <- ifelse(all_metadata$total_gorilla_infections > 0, "success", "fail")

all_metadata$connectivity <- factor(all_metadata$connectivity, levels = c("fragmented", "connected", "restored"))
all_metadata$status <- factor(all_metadata$status, levels = c("fail", "success"))  # fail first (lightest on right)

# Prepare data: count and percentage
plot_data <- all_metadata %>%
  group_by(connectivity, status) %>%
  summarise(n = n()) %>%
  mutate(percent = 100 * n / sum(n)) %>%
  ungroup()

# Plot
ggplot(plot_data, aes(x = connectivity, y = percent, fill = status)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # horizontal bars
  scale_fill_manual(
    values = c("fail" = "#f0f0f0", "success" = "#636363"),
    labels = c("No Spillover", "Spillover")
  ) +
  labs(
    x = "Connectivity",
    y = "Percentage of Simulations",
    fill = "Simulation Result"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("sandbox/plots_report/succesfulsimulations.png",  width = 7, height = 4, units = "in", dpi = 300, bg = "white")



#now plotting the boxplot with successes only!
successful_runs <- all_metadata %>%
  filter(total_gorilla_infections > 0)

# Optional: set consistent factor order
successful_runs$connectivity <- factor(successful_runs$connectivity, levels = c("fragmented", "connected", "restored"))

# Boxplot
ggplot(successful_runs, aes(x = connectivity, y = total_gorilla_infections,
                            fill = gorilla_sociality)) +
  geom_boxplot(alpha=0.6) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Gorilla Infections in Different Landscape Scenarios",
    y = "Total Infections",
    x = "Connectivity Scenario (n = 200 runs)",
    fill = "Sociality"
  ) +
  scale_fill_viridis_d(option = "viridis")

ggsave("sandbox/plots_report/spillover_gorilla_infections_by_scenario2.png",  width = 7, height = 4, units = "in", dpi = 300, bg = "white")















#What is the spillover time, so when disease spills over from humans to MG?
ggplot(successful_runs, aes(x = connectivity, y = first_spillover_time,
                            fill = gorilla_sociality)) +
  geom_boxplot(alpha = 0.6) +
  theme_minimal(base_size = 11) +
  labs(
    title = "Spillover Timing Across Connectivity Scenarios",
    y = "Time of First Spillover (days)",
    x = "Connectivity Scenario",
    fill = "Gorilla Sociality"
  ) +
  scale_fill_viridis_d(option = "viridis")

ggsave("sandbox/plots_report/spillovertiming.png",  width = 7, height = 4, units = "in", dpi = 300, bg = "white")






















#Also interested whether and if disease spreads from MG 1 to MG 2 and vice versa:
sim_result <- all_results[[1]][[1]]

extract_metadata <- function(sim_result, scenario_name) {
  sim <- sim_result$simulation
  pop <- sim_result$population

  hosts_df <- sim$host.info.A$table.hosts

  # Add infector population info by joining on inf.source.id
  hosts_df <- hosts_df %>%
    left_join(
      hosts_df %>%
        select(hosts.ID, current.in) %>%
        rename(inf.by = hosts.ID, inf.source.pop = current.in),
      by = "inf.by"
    )

  # First spillover to *any* gorilla
  gorilla_infections <- hosts_df[hosts_df$current.in %in% c("Gorilla1", "Gorilla2") & hosts_df$inf.time > 0, ]
  first_spillover_time <- if (nrow(gorilla_infections) > 0) min(gorilla_infections$inf.time, na.rm = TRUE) else NA

  # Spillover from Gorilla1 → Gorilla2
  mg1_to_mg2 <- hosts_df[
    hosts_df$inf.by %in% hosts_df$hosts.ID[hosts_df$inf.in == "Gorilla1"] &
      hosts_df$inf.in == "Gorilla2", ]

  mg2_to_mg1 <- hosts_df[
    hosts_df$inf.by %in% hosts_df$hosts.ID[hosts_df$inf.in == "Gorilla2"] &
      hosts_df$inf.in == "Gorilla1", ]

  spillover_MG1_to_MG2_time <- if (nrow(mg1_to_mg2) > 0) {
    min(mg1_to_mg2$inf.time, na.rm = TRUE)
  } else {
    NA
  }

  spillover_MG2_to_MG1_time <- if (nrow(mg2_to_mg1) > 0) {
    min(mg2_to_mg1$inf.time, na.rm = TRUE)
  } else {
    NA
  }

  # Total gorilla infections
  total_gorilla_infections <- nrow(gorilla_infections)

  # Outbreak duration: max exit time
  outbreak_duration <- max(hosts_df$out.time, na.rm = TRUE)

  # Total infections
  total_infections <- nrow(hosts_df)

  # Final population sizes
  final_pop_sizes <- sapply(pop, function(x) tail(x, 1))

  # Metadata row
  meta <- data.frame(
    scenario = scenario_name,
    first_spillover_time = first_spillover_time,
    total_gorilla_infections = total_gorilla_infections,
    outbreak_duration = outbreak_duration,
    total_infections = total_infections,
    final_pop_humans = final_pop_sizes["Humans"],
    final_pop_gorilla1 = final_pop_sizes["Gorilla1"],
    final_pop_gorilla2 = final_pop_sizes["Gorilla2"],
    spillover_MG1_to_MG2_time = spillover_MG1_to_MG2_time
  )

  return(meta)
}

all_metadata <- lapply(names(all_results), function(scn_name) {
  runs <- all_results[[scn_name]]

  scenario_meta <- lapply(runs, function(simres) {
    extract_metadata(simres, scn_name)
  })

  do.call(rbind, scenario_meta)
}) %>% do.call(rbind, .)

# Add scenario columns
all_metadata <- all_metadata %>%
  mutate(
    connectivity = sub("_.*", "", scenario),
    gorilla_sociality = sub(".*_", "", scenario)
  )


hosts_df <- hosts_df %>%
  left_join(
    hosts_df %>%
      select(hosts.ID, current.in) %>%
      rename(inf.by = hosts.ID, inf.source.pop = current.in),
    by = "inf.by"
  )



p1 <- ggplot(all_metadata, aes(x = connectivity, y = spillover_MG1_to_MG2_time,
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







#show percentage of simulations with spillover from MG1 to MG2:
all_metadata$spillover_MG1_to_MG2_status <- ifelse(
  !is.na(all_metadata$spillover_MG1_to_MG2_time),
  "success",
  "fail"
)

# Make sure factors have the right order
all_metadata$connectivity <- factor(all_metadata$connectivity, levels = c("fragmented", "connected", "restored"))
all_metadata$gorilla_sociality <- factor(all_metadata$gorilla_sociality)  # or specify levels if needed
all_metadata$spillover_MG1_to_MG2_status <- factor(all_metadata$spillover_MG1_to_MG2_status, levels = c("fail", "success"))

spillover_summary <- all_metadata %>%
  group_by(connectivity, gorilla_sociality, spillover_MG1_to_MG2_status) %>%
  summarise(count = n()) %>%
  group_by(connectivity, gorilla_sociality) %>%
  mutate(percent = 100 * count / sum(count)) %>%
  ungroup()

p2 <- ggplot(spillover_summary, aes(x = connectivity, y = percent,
                              fill = spillover_MG1_to_MG2_status)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # horizontal bars
  scale_fill_manual(
    values = c("fail" = "#f0f0f0", "success" = "#636363"),
    labels = c("No Spillover", "Spillover")) +
  labs(x = "Connectivity",
    y = "Percentage of Simulations",
    fill = "Simulation Result",
    title = "Percentage of Simulations with MG1 to MG2 Spillover by Connectivity and Sociality") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank())


ggsave("sandbox/plots_report/succesfulsimulations_part2.png",  width = 7, height = 4, units = "in", dpi = 300, bg = "white")


library(patchwork)
p1 <- p1 + labs(title = NULL)
p2 <- p2 + labs(title = NULL)

p1 + p2 +
  plot_layout(ncol = 1) +
  plot_annotation(tag_levels = 'A')

ggsave("sandbox/plots_report/spillover_MG1_to_MG2.png",  width = 7, height = 6, units = "in", dpi = 300, bg = "white")

