#this script is for testing the updated/modified singleNone function
#(which is being updated/modified in the single-none.R script!)

#clear environment
rm(list = ls())


#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)


#-------------------------------testing whether we now have the working singleNone file!!!

# Set parameters (if not already)
p_max_fct <- function(ID) {
  return(0.3)  # You can still return a constant, but accept an argument
}

t_incub_fct <- function(ID) {
  return(3)
}

# Transmission probability function
proba <- function(t, p_max, t_incub) {
  ifelse(t >= t_incub, p_max, 0)
}

# Exit probability function (constant exit)
p_Exit_fct <- function(t) { 0.1 }

# Contact function (constant number of contacts)
time_contact <- function(t) { 2 }

# Run the simulation
test.popdyn <- singleNone(length.sim = 100, max.infected = 1000, init.individuals = 2,
                          nContact = time_contact,
                          param.nContact = NA,
                          timeDep.nContact = FALSE,
                          pTrans = proba,
                          param.pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct),
                          timeDep.pTrans = FALSE,
                          pExit = p_Exit_fct,
                          param.pExit = NA,
                          timeDep.pExit = FALSE,
                          print.progress = TRUE)

#THIS WORKSSS YHAS
plot(test.popdyn$pop_model, type="l", xlab="Time", ylab="Population Size")
test.popdyn$pop_model  # Vector of population size per time step
test.popdyn$host.info.A$N.infected

plot(test.popdyn$pop_model, type="l", col="blue", ylab="Count", xlab="Time")
lines(test.popdyn$host.info.A$summary$N.infected, col="red")
legend("topright", legend=c("Population Size", "Infected"), col=c("blue", "red"), lty=1)










#Is it possible to determine the initital population size + birth and death rates ourselves? (the answer is, yes we can!!)
sim <- singleNone(length.sim = 100, max.infected = 10000, init.individuals = 1,
                  pExit = p_Exit_fct,
                  param.pExit = NA,
                  timeDep.pExit = FALSE,
                  nContact = time_contact,
                  param.nContact = NA,
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

plot(sim$pop_model, type = "l", ylab = "Population size", xlab = "Time", main = "Population Dynamics")

sim$pop_model





#So, next step is fine tuning this!!------------------------------------------------------------
# --> lets focus first on the interpretation and visualization

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

#only population size:
ggplot(pop_df, aes(x = time, y = population_size)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Population Size Over Time",
       x = "Time",
       y = "Population Size") +
  theme_minimal()

#only infected:
ggplot(pop_df, aes(x = time, y = infected)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Number of Infected Over Time",
       x = "Time",
       y = "Infected Count") +
  theme_minimal()

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

#ggsave("sandbox/plots/population_dynamics2.pdf", width = 10, height = 6)
#ggsave("sandbox/plots/presentation/PopModel_infected5.png", width = 10, height = 6, dpi = 300, bg = "white")

#observation from plot: infected rises while population drops, which indicates that the outbreak is actually spreading fast enough that infection-related exits are reducing population size!!






library(patchwork)
p1 + p2 + p3 + p4 +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(title = "Epidemic Exits influence Population size",
                  theme = theme(plot.title = element_text(size = 11, face = "bold")),
                  tag_levels = 'A') & theme(legend.position = "bottom")

ggsave("sandbox/plots/report/epidemicexits_popmodel.png", width = 7, height = 5, units = "in", dpi = 300, bg = "white")










#Can we alter the parameters?------------------------------------------------------------
#yes we can, these parameters are now more representing how the "old default" parameters are set
p_Exit_fct <- function(t) {return(0.3)}

n_contact_fct <- function(t) {abs(round(rnorm(1, mean = 5, sd = 1), 0))}

p_Trans_fct <- function(t, p_max, t_incub) {
  if (t < t_incub) return(0)
  else return(p_max)}

t_incub_fct <- function(x) {rnorm(x, mean = 1, sd = 1)}
p_max_fct <- function(x) {rbeta(x, shape1 = 2, shape2 = 2)}

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)


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
                  param.pTrans = param_pTrans,
                  timeDep.pTrans = FALSE,
                  print.progress = TRUE,
                  initial.population = 10000,
                  birth.rate = 0.5,
                  death.rate = 0.5)

#Starting the simulation
#Initializing ... running ...
#Time: 10 (10% of maximum length). Hosts count: 4 (0% of maximum infected hosts).
#Time: 20 (20% of maximum length). Hosts count: 9 (0% of maximum infected hosts).
#Time: 30 (30% of maximum length). Hosts count: 24 (0% of maximum infected hosts).
#Time: 40 (40% of maximum length). Hosts count: 83 (1% of maximum infected hosts).
#Time: 50 (50% of maximum length). Hosts count: 266 (3% of maximum infected hosts).
#Time: 60 (60% of maximum length). Hosts count: 678 (7% of maximum infected hosts).
#Time: 70 (70% of maximum length). Hosts count: 1799 (18% of maximum infected hosts).
#Time: 80 (80% of maximum length). Hosts count: 4571 (46% of maximum infected hosts).
#done.
#The simulation has run for 89 units of time and a total of 10821 hosts have been infected.

infected_over_time <- sapply(0:sim$total.time, function(t) {
  sum(sim$host.info.A$table.hosts$inf.time <= t)})

pop_df <- data.frame(time = 0:sim$total.time,
                     population_size = sim$pop_model[1:(sim$total.time + 1)],
                     infected = infected_over_time)

p3 <- ggplot(pop_df, aes(x = time)) +
  geom_line(aes(y = population_size, color = "Population Size"), size = 1) +
  geom_line(aes(y = infected, color = "Infected Individuals"), size = 1) +
  scale_color_manual(values = c("Population Size" = "blue",
                                "Infected Individuals" = "red")) +
  labs(title = "Population Size and Infected Individuals Over Time",
       x = "Time",
       y = "Number of Individuals",
       color = "Legend") +
  theme_minimal()+
  theme(plot.title = element_text(size = 22, face = "bold"),
        axis.title = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 18))
p3

#ggsave("sandbox/plots/population_dynamics2.pdf", width = 10, height = 6)
ggsave("sandbox/plots/presentation/PopModel_infected2.png", width = 10, height = 6, dpi = 300, bg = "white")







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

pA <- ggplot(results_df, aes(x = max_infected)) +
  geom_histogram(bins = 20, color = "white") +
  scale_color_viridis_d() +
  labs(title = "Distribution of Maximum Infected Individuals across 100 Simulations",
    x = "Maximum Number of Infected Individuals",
    y = "Frequency") +
  theme_minimal()+
  theme(plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 11))
pA

pA <- pA + labs(title = NULL)
pB <- pB + labs(title = NULL, subtitle = NULL)

pA + pB +
  plot_layout(ncol = 1) +
  plot_annotation(title = "Distribution of Maximum Infected Individuals across 100 Simulations",
                  theme = theme(plot.title = element_text(size = 11, face = "bold")),
                  tag_levels = 'A')


ggsave("sandbox/plots/report/histogram_repeatsAB2.png", width = 7, height = 5, units = "in", dpi = 300, bg = "white")




#so for these parameters: 28 out of 50 runs ended with a max infected of 1 -> these outbreaks never really took off. A few runs had small outbreaks. The rest (~21 runs) had large outbreaks with max infected ranging from 4000 to 11000.
#this is typical for these models as often many simulations die out early due to chance, while others explode into full outbreaks.




















#--------------------------lets make nContact dependent on popsize again!!
n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]

  base_rate <- 10  # baseline contacts per 1000 individuals
  scaled_mean <- base_rate * (current_pop / 1000)

  n_contacts_i <- abs(round(rnorm(1, mean = scaled_mean, sd = 1)))

  return(n_contacts_i)
}

p_Exit_fct <- function(t) {return(0.08)}

p_Trans_fct <- function(t, p_max, t_incub) {
  if (t < t_incub) return(0)
  else return(p_max)}

t_incub_fct <- function(x) {rnorm(x, mean = 7, sd = 1)}
p_max_fct <- function(x) {rbeta(x, shape1 = 5, shape2 = 2)}

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)

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
                  param.pTrans = param_pTrans,
                  timeDep.pTrans = FALSE,
                  print.progress = TRUE,
                  initial.population = 10000,
                  birth.rate = 0.5,
                  death.rate = 0.5)






