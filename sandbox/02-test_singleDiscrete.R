#this script is for testing the updated/modified singleDiscrete function
#(which is being updated/modified in the single-Discrete.R script!)

#clear environment
rm(list = ls())


#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)






# Initial population sizes for locations A, B, C
initial.population.structure <- list(A = 1000, B = 800, C = 600)

# Birth rates per time step (can be simple constants or functions)
birth.rate.structure <- list(A = 0.02, B = 0.01, C = 0.03)

# Death rates per time step
death.rate.structure <- list(A = 0.01, B = 0.015, C = 0.02)

transition.matrix <- matrix(
  c(0, 0.2, 0.4,
    0.5, 0, 0.6,
    0.5, 0.8, 0),
  nrow = 3, byrow = TRUE,
  dimnames = list(c("A", "B", "C"), c("A", "B", "C")))

p_Exit_fct  <- function(t, current.in) {
  if(current.in == "A") return(0.02)
  if(current.in == "B") return(0.05)
  if(current.in == "C") return(0.1)}

p_Move_fct <- function(t) 0.1

n_contact_fct <- function(t) abs(round(rnorm(1, 0.5, 1), 0))

proba <- function(t, p_max, t_incub) {
  if(t <= t_incub) p <- 0 else p <- p_max
  return(p)}

t_incub_fct <- function(x) rnorm(x, mean = 5, sd = 1)
p_max_fct <- function(x) rbeta(x, shape1 = 5, shape2 = 2)

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)

SimulationSingle <- nosoiSim(type = "single", popStructure = "discrete",
                             length.sim = 300, max.infected = 300,
                             init.individuals = 1, init.structure = "A",
                             structure.matrix = transition.matrix,
                             pExit = p_Exit_fct, param.pExit = NA,
                             timeDep.pExit = FALSE, diff.pExit = TRUE,
                             pMove = p_Move_fct, param.pMove = NA,
                             timeDep.pMove = FALSE, diff.pMove = FALSE,
                             nContact = n_contact_fct, param.nContact = NA,
                             timeDep.nContact = FALSE, diff.nContact = FALSE,
                             pTrans = proba, param.pTrans = param_pTrans,
                             timeDep.pTrans = FALSE, diff.pTrans = FALSE,
                             prefix.host = "H", print.progress = FALSE, print.step = 10)

length.sim <- 300  # match simulation length

initial.population.structure <- list(A = 1000, B = 800, C = 600)  # your initial pop sizes
birth.rate.structure <- list(A = 0.05, B = 0.03, C = 0.04)       # per location birth rates
death.rate.structure <- list(A = 0.01, B = 0.015, C = 0.02)     # per location death rates

# Initialize population size vectors per location
PopModel <- list()
for (loc in names(initial.population.structure)) {
  PopModel[[loc]] <- numeric(length.sim + 1)
  PopModel[[loc]][1] <- initial.population.structure[[loc]]
}

# For each time step, update births and deaths
for (t in 1:length.sim) {
  for (loc in names(PopModel)) {
    births <- rpois(1, birth.rate.structure[[loc]] * PopModel[[loc]][t])
    deaths <- rbinom(1, PopModel[[loc]][t], death.rate.structure[[loc]])

    # Optional: count epidemic deaths at time t and loc from your simulation:
    epidemic_deaths <- sum(
      SimulationSingle$host.info.A$table.hosts$structure == loc &
        SimulationSingle$host.info.A$table.hosts$out.time == t &
        SimulationSingle$host.info.A$table.hosts$active == FALSE
    )

    # Update population size with births, deaths, and epidemic deaths
    PopModel[[loc]][t + 1] <- max(0, PopModel[[loc]][t] + births - deaths - epidemic_deaths)
  }
}
























###Made a wrapper function!!
run_sim_and_popmodel <- function(length.sim = 300,
                                 init.pop = list(A = 1000, B = 800, C = 600),
                                 birth.rate = list(A = 0.05, B = 0.03, C = 0.04),
                                 death.rate = list(A = 0.01, B = 0.015, C = 0.02),
                                 transition.matrix,
                                 pExit,
                                 pMove,
                                 nContact,
                                 pTrans,
                                 param.pTrans) {

  # Run epidemic simulation
  sim <- nosoiSim(type = "single", popStructure = "discrete",
                  length.sim = length.sim,
                  max.infected = 300,
                  init.individuals = 1,
                  init.structure = "A",
                  structure.matrix = transition.matrix,
                  pExit = pExit,
                  param.pExit = NA,
                  timeDep.pExit = FALSE,
                  diff.pExit = TRUE,
                  pMove = pMove,
                  param.pMove = NA,
                  timeDep.pMove = FALSE,
                  diff.pMove = FALSE,
                  nContact = nContact,
                  param.nContact = NA,
                  timeDep.nContact = FALSE,
                  diff.nContact = FALSE,
                  pTrans = pTrans,
                  param.pTrans = param.pTrans,
                  timeDep.pTrans = FALSE,
                  diff.pTrans = FALSE,
                  prefix.host = "H",
                  print.progress = FALSE,
                  print.step = 10)

  # Get actual epidemic duration
  sim_length <- sim$total.time

  # Initialize population tracking
  PopModel <- list()
  for (loc in names(init.pop)) {
    PopModel[[loc]] <- numeric(sim_length + 1)
    PopModel[[loc]][1] <- init.pop[[loc]]
  }

  # Extract hosts data frame
  hosts_df <- sim$host.info.A$table.hosts

  # Update population size based on births, deaths, and epidemic exits
  for (t in 1:sim_length) {
    for (loc in names(PopModel)) {
      births <- rpois(1, birth.rate[[loc]] * PopModel[[loc]][t])
      deaths <- rbinom(1, PopModel[[loc]][t], death.rate[[loc]])
      epidemic_deaths <- sum(hosts_df$structure == loc & hosts_df$out.time == t & hosts_df$active == FALSE)

      PopModel[[loc]][t + 1] <- max(0, PopModel[[loc]][t] + births - deaths - epidemic_deaths)
    }
  }

  return(list(simulation = sim, population = PopModel))
}


# Define parameters and functions (as you already have them):
transition.matrix <- matrix(c(0,0.2,0.4,
                              0.5,0,0.6,
                              0.5,0.8,0), nrow=3, ncol=3,
                            dimnames=list(c("A","B","C"),
                                          c("A","B","C")))

p_Exit_fct <- function(t, current.in){
  if(current.in == "A") return(0.02)
  if(current.in == "B") return(0.05)
  if(current.in == "C") return(0.1)
}

p_Move_fct <- function(t) 0.1
n_contact_fct <- function(t) abs(round(rnorm(1, 0.5, 1), 0))

proba <- function(t, p_max, t_incub){
  if(t <= t_incub) p <- 0 else p <- p_max
  return(p)
}
t_incub_fct <- function(x) rnorm(x, mean=5, sd=1)
p_max_fct <- function(x) rbeta(x, shape1=5, shape2=2)

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)

# Run the combined simulation + population dynamics:
result <- run_sim_and_popmodel(length.sim = 300,
                               init.pop = list(A=1000, B=800, C=600),
                               birth.rate = list(A=0.3, B=0.5, C=0.1),
                               death.rate = list(A=0.3, B=0.5, C=0.1),
                               transition.matrix = transition.matrix,
                               pExit = p_Exit_fct,
                               pMove = p_Move_fct,
                               nContact = n_contact_fct,
                               pTrans = proba,
                               param.pTrans = param_pTrans)



# Combine all infected hosts from each location
host_data <- do.call(rbind, result$simulation[grep("host.info", names(result$simulation))])
hosts_dt <- host_data[[2]]
summary(hosts_dt$out.time)
table(is.na(hosts_dt$out.time)) # TRUE means still infected, FALSE means exited
sum(!is.na(hosts_dt$out.time))
table(hosts_dt$current.in, is.na(hosts_dt$out.time))



#now lets extract pop size and epidemic exits per location
library(data.table)
population_dynamics <- function(hosts_dt) {
  # Ensure data.table
  dt <- as.data.table(hosts_dt)

  # Create a vector of all time points from 0 to max observed time
  max_time <- max(c(dt$inf.time, dt$out.time), na.rm = TRUE)
  times <- 0:max_time

  # Initialize result list
  result_list <- list()

  # Unique locations
  locations <- unique(dt$current.in)

  # For each location, build time series of active hosts and exits
  for(loc in locations) {
    # Subset hosts in this location (current.in)
    dt_loc <- dt[current.in == loc]

    # For each time, count active hosts: those infected before or at t and not exited before t
    active_counts <- sapply(times, function(t) {
      sum(dt_loc$inf.time <= t & (is.na(dt_loc$out.time) | dt_loc$out.time > t))
    })

    # For each time, count new epidemic exits at t
    exits_counts <- sapply(times, function(t) {
      sum(dt_loc$out.time == t, na.rm = TRUE)
    })

    result_list[[loc]] <- data.table(
      time = times,
      location = loc,
      active_hosts = active_counts,
      epidemic_exits = exits_counts
    )
  }

  # Combine all locations
  population_dynamics_dt <- rbindlist(result_list)

  return(population_dynamics_dt)
}

pop_dynamics <- population_dynamics(hosts_dt)
head(pop_dynamics)

p1 <- ggplot(pop_dynamics, aes(x = time, y = active_hosts, color = location)) +
  geom_line(size = 1) +
  labs(title = "Active Hosts Over Time by Location",
       x = "Time",
       y = "Number of Active Hosts")+
  theme_minimal()




max_len <- max(sapply(result$population, length)) - 1  # since time starts at 0

pop_dt <- rbindlist(
  lapply(names(result$population), function(loc) {
    pop_vec <- result$population[[loc]]
    # Pad with last known value if needed
    if (length(pop_vec) < max_len + 1) {
      pad_len <- max_len + 1 - length(pop_vec)
      pop_vec <- c(pop_vec, rep(tail(pop_vec, 1), pad_len))
    }
    data.table(
      time = 0:max_len,
      population = pop_vec,
      location = loc
    )
  })
)

# Plot
p2 <- ggplot(pop_dt, aes(x = time, y = population, color = location)) +
  geom_line(size = 1) +
  labs(title = "Population Size Over Time by Location",
       x = "Time", y = "Population Size") +
  theme_minimal()


library(patchwork)
# Combine the two plots
p2 + p1 + plot_layout(ncol = 1)
ggsave("sandbox/plots_report/singleDiscrete_test.png", width = 10, height = 6, dpi = 300, bg = "white")


#SOOO: what does this even tell us as we are now running it external from nosoi again
#so we can tell whether infection levels rise and fall relative to the population pool in each subpopulation.
#whether epidemic waves are impacting pop dynamics
#how host availability might be limiting or amplifying transmission


