#continuing with script 01-test_singleNone but now making the number of contacts in the simulation population size dependent --> using hostcount() for this!!
#(which is being updated/modified in the single-none.R script!)

#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)

# Set parameters
p_max_fct <- function(ID) {
  return(0.2)  # You can still return a constant, but accept an argument
}

t_incub_fct <- function(ID) {
  return(5)
}

# Transmission probability function
proba <- function(t, p_max, t_incub) {
  # Mimic immunity buildup or susceptible depletion
  decay <- exp(-0.05 * t)
  ifelse(t >= t_incub, p_max * decay, 0)
}


# Exit probability function (constant exit)
p_Exit_fct <- function(t) { 0.1 }

# Contact function (constant number of contacts) making it dependent on initial pop size!!
initial_pop_size <- 10000

n_contact_fct <- function(t) {
  base_rate <- 2           # Max contacts per host
  cap <- 10000             # Cap at which full contact rate is used

  scaled_rate <- base_rate * (initial_pop_size / cap)
  return(max(0, round(scaled_rate)))
}

sim <- singleNone(length.sim = 1000, max.infected = 10000, init.individuals = 1,
                  pExit = p_Exit_fct,
                  param.pExit = NA,
                  timeDep.pExit = FALSE,
                  nContact = n_contact_fct,
                  param.nContact = list(),
                  timeDep.nContact = FALSE,
                  pTrans = proba,
                  param.pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct),
                  timeDep.pTrans = FALSE,
                  print.progress = TRUE,
                  initial.population = 10000,
                  birth.rate = 0.5,
                  death.rate = 0.5)

#Starting the simulation
#Initializing ... running ...
#Time: 10 (20% of maximum length). Hosts count: 10 (2% of maximum infected hosts).
#Time: 20 (40% of maximum length). Hosts count: 85 (17% of maximum infected hosts).
#Time: 30 (60% of maximum length). Hosts count: 566 (113% of maximum infected hosts).
#done.
#The simulation has run for 30 units of time and a total of 566 hosts have been infected.


str(sim$host.info.A)

infected_over_time <- sapply(0:sim$total.time, function(t) {
  sum(sim$host.info.A$table.hosts$inf.time <= t)})

#lets also add the dynamics (active infected hosts at a given time step) to the same table!
active_infections <- sapply(0:sim$total.time, function(t) {
  sum(sim$host.info.A$table.hosts$inf.time <= t &
        sim$host.info.A$table.hosts$active)})

pop_df <- data.frame(time = 0:sim$total.time,
                     population_size = sim$pop_model[1:(sim$total.time + 1)],
                     infected = infected_over_time,
                     active = active_infections)

head(pop_df)

#in the same plot:
p4 <- ggplot(pop_df, aes(x = time)) +
  geom_line(aes(y = population_size,
                color = factor("Population Size",
                               levels = c("Population Size",
                                          "Cumulative Infected Individuals",
                                          "Active Infected Individuals"))), size = 1) +
  geom_line(aes(y = infected,
                color = factor("Cumulative Infected Individuals",
                               levels = c("Population Size",
                                          "Cumulative Infected Individuals",
                                          "Active Infected Individuals"))), size = 1) +
  geom_line(aes(y = active,
                color = factor("Active Infected Individuals",
                               levels = c("Population Size",
                                          "Cumulative Infected Individuals",
                                          "Active Infected Individuals"))), size = 1) +
  scale_colour_viridis_d()+
  labs(
    x = "Time",
    y = "Number of Individuals",
    color = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 11))
p4





library(patchwork)
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Epidemic Exits influence Population size",
                  theme = theme(plot.title = element_text(size = 11, face = "bold")),
                  tag_levels = 'A') & theme(legend.position = "bottom")

ggsave("sandbox/plots/report/epidemicexits_popmodel2.png", width = 7, height = 5, units = "in", dpi = 300, bg = "white")





#---------test with stochastic replicates:
n_replicates <- 100

results <- replicate(n_replicates, {
  sim <- singleNone(length.sim = 100,
                    max.infected = 10000,
                    init.individuals = 1,
                    pExit = p_Exit_fct,
                    param.pExit = NA,
                    timeDep.pExit = FALSE,
                    nContact = n_contact_fct,
                    param.nContact = NA,
                    timeDep.nContact = FALSE,
                    pTrans = p_Trans_fct,
                    param.pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct),
                    timeDep.pTrans = FALSE,
                    print.progress = FALSE,
                    initial.population = 10000,
                    birth.rate = 0.5,
                    death.rate = 0.5)
  max(sapply(0:sim$total.time, function(t) {
    sum(sim$host.info.A$table.hosts$inf.time <= t)
  }))})

summary(results)
sd(results)
table(results == 1)  # Number of extinctions at first case

results_df <- data.frame(max_infected = results)

pB <- ggplot(results_df, aes(x = max_infected)) +
  geom_histogram(bins = 20, color = "white") +
  scale_color_viridis_d() +
  labs(title = "Distribution of Maximum Infected Individuals across 100 Simulations",
       x = "Maximum Number of Infected Individuals",
       y = "Frequency") +
  theme_minimal()+
  theme(plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11))
pB

pA <- pA + labs(title = NULL)
pB <- pB + labs(title = NULL, subtitle = NULL)

pB + pA +
  plot_layout(ncol = 2) +
  plot_annotation(title = "Distribution of Maximum Infected Individuals across 100 Simulations",
                  theme = theme(plot.title = element_text(size = 11, face = "bold")),
                  tag_levels = 'A')


ggsave("sandbox/plots/report/histogram_repeatsAB4.png", width = 7, height = 5, units = "in", dpi = 300, bg = "white")
