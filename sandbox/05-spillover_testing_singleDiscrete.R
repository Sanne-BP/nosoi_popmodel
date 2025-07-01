#now to the actual testing and creating of the final results of spillover and spillback between humans and mountain gorillas! (based on the finalized script 04)

#going to run 100 simulations per scenario, where a scenario is a defined combination of:
# - spillover probability
# - contact rates
# - connectivity
#will store each simulation's result in a meta dataframe to be able to compare across / within scenarios



#------------------------------------------------------------------------------------------------
#1ST SCENARIO: SPILLOVER PROBABILITY (HUMAN -> MOUNTAIN GORILLA)

#defining different (sub)populations dynamics:
subpop_names <- c("Humans", "Gorilla1", "Gorilla2")
init_pop <- list(Humans = 300000, Gorilla1 = 607, Gorilla2 = 456)
birth_rates <- list(Humans = 0.00001, Gorilla1 = 0.00006, Gorilla2 = 0.00006)
death_rates <- list(Humans = 0.00001, Gorilla1 = 0.00006, Gorilla2 = 0.00006)

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
  if (current.in == "Humans") return(0.14)
  if (current.in == "Gorilla1") return(0.10)
  if (current.in == "Gorilla2") return(0.12)}

#movement probability per (sub)population
p_Move_fct <- function(t, current.in) {
  if (current.in == "Humans") return(0.12) # mainly movement rangers, tourists, local forest use
  if (current.in == "Gorilla1") return(0.03) # larger range, overlap with human edges
  if (current.in == "Gorilla2") return(0.01) # min movement, more intact core forest
}

#number of contacts per infected host, but making it dependent on the initial population sizes to prevent very high, unrealistic cases of infections
init_pop # <- list(Humans = 10000, Gorilla1 = 200, Gorilla2 = 450) using this!!

n_contact_fct <- function(t, current.in) {
  pop_size <- init_pop[[current.in]]

  #Define a base rate per subpopulation (can be modified)
  base_rate <- switch(current.in,
                      Humans = 6,    # Rangers, tourists, other locals
                      Gorilla1 = 4,  # VM gorilla group (larger, more habituated)
                      Gorilla2 = 2)  # Bwindi group (smaller, more isolated)

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
# For transmission probability, define with incubation and max prob, per host:
p_max_fct <- function(x) rbeta(x, shape1 = 3, shape2 = 4)
t_incub_fct <- function(x) rnorm(x, mean = 3, sd = 1)

proba <- function(t, p_max, t_incub) {
  if (t <= t_incub) {
    p <- 0  # no transmission during incubation
  } else {
    p <- p_max  # after incubation, constant transmission probability
  }
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
                  max.infected = 10000,
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
                  print.progress = FALSE,
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

create_spillover_metadata <- function(simulation, population, scenario_name) {
  # Extract simulation metadata
  sim_meta <- list(
    scenario = scenario_name,
    length_sim = length(population$Humans) - 1,
    init_pop = sapply(population, function(x) x[1]),
    final_pop = sapply(population, function(x) tail(x, 1)),
    total_infections = sum(simulation$host.info.A$table.hosts$active == FALSE),
    total_deaths = sum(simulation$host.info.A$table.hosts$out.time > 0)
  )

  return(sim_meta)
}





