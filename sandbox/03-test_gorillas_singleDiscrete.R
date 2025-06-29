#lets continue with from the 02-test_singleDiscrete but now implement it into the case study example with humans and isolated mountain gorillas!

#use hostcount()!! make nContact dependent on hostcount(), similar in ebola continuous, make it dependent on pop size!!! its a bandaid!! to avoid stupid cases of 100 million infections
#gonna to this in script 04-mountaingorillas_singleDiscrete

subpop_names <- c("Humans", "Gorilla1", "Gorilla2")
init_pop <- list(Humans = 10000, Gorilla1 = 200, Gorilla2 = 450)
birth_rates <- list(Humans = 0.2, Gorilla1 = 0.09, Gorilla2 = 0.15)
death_rates <- list(Humans = 0.2, Gorilla1 = 0.09, Gorilla2 = 0.15)

transition.matrix <- matrix(
  c(0.85,  0.1,  0.05,  # Humans mostly stay among humans, small chance to gorillas
    0.05, 0.9,  0.05,  # Gorilla1 mostly stays, small human contact, small gorilla2 contact
    0.05, 0.05, 0.9),    # Gorilla2 mostly stays, small human contact, small gorilla1 contact
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(subpop_names, subpop_names))

p_Exit_fct <- function(t, current.in) {
  if (current.in == "Humans") return(0.02)
  if (current.in == "Gorilla1") return(0.05)
  if (current.in == "Gorilla2") return(0.06)
}

p_Move_fct <- function(t) 0.1  # simple constant move probability, or customize

n_contact_fct <- function(t) {
  abs(round(rnorm(1, mean = 0.5, sd = 1), 0))
}

# For transmission probability, define with incubation and max prob, per host:

p_max_fct <- function(x) rbeta(x, shape1 = 5, shape2 = 2)
t_incub_fct <- function(x) rnorm(x, mean = 5, sd = 1)

proba <- function(t, p_max, t_incub) {
  if (t <= t_incub) p <- 0 else p <- p_max
  return(p)
}

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)





#Now run the wrapper function:
run_sim_and_popmodel <- function(length.sim = 300,
                                 init.pop,
                                 birth.rate,
                                 death.rate,
                                 transition.matrix,
                                 pExit,
                                 pMove,
                                 nContact,
                                 pTrans,
                                 param.pTrans) {

  # Run epidemic simulation with Nosoi
  sim <- nosoiSim(type = "single", popStructure = "discrete",
                  length.sim = length.sim,
                  max.infected = 300,
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
                  diff.pMove = FALSE,
                  nContact = nContact,
                  param.nContact = NA,
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


result <- run_sim_and_popmodel(
  length.sim = 300,
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







#NOW exploring these results:
pop_sizes_df <- do.call(rbind, lapply(names(result$population), function(loc) {
  pop_vec <- result$population[[loc]]
  data.frame(
    time = seq_along(pop_vec) - 1,  # same length as pop_vec, starting at 0
    location = loc,
    population = pop_vec
  )
}))

head(pop_sizes_df)

ggplot(pop_sizes_df, aes(x = time, y = population, color = location)) +
  geom_line(size = 1) +
  labs(title = "Population Sizes Over Time by Subpopulation",
       x = "Time",
       y = "Population Size") +
  theme_minimal()




#now lets have a look at how we can visualize spillover/spillback etc.!
host_info_list <- result$simulation[grep("host.info", names(result$simulation))]
hosts_tables <- lapply(host_info_list, function(x) x$table.hosts)
hosts_dt <- rbindlist(hosts_tables, use.names = TRUE, fill = TRUE)

summary(hosts_dt$out.time)
table(is.na(hosts_dt$out.time))
table(hosts_dt$current.in, is.na(hosts_dt$out.time))

#so spillover: infection originating from humans to gorillas (either 1 or 2)
#spillback: infection from gorillas back to humans (not sure if we even want that tho)
#we can also consider within-group transmission

hosts_dt <- as.data.table(hosts_dt)
hosts_dt[inf.in == "NA", inf.in := NA_character_]

# Create a transmission matrix counting inf.in -> current.in pairs
transmission_matrix <- hosts_dt[!is.na(inf.in), .N, by = .(inf.in, current.in)]

print(transmission_matrix)

# Spillover from Humans to Gorillas
spillover <- transmission_matrix[inf.in == "Humans" & current.in %in% c("Gorilla1", "Gorilla2")]

# Spillback from Gorillas to Humans
spillback <- transmission_matrix[inf.in %in% c("Gorilla1", "Gorilla2") & current.in == "Humans"]

spillover
#inf.in current.in      N
#1: Humans Gorilla2     5
#2: Humans Gorilla1    15

spillback #no spillback from gorillas to humans

ggplot(transmission_matrix, aes(x = inf.in, y = current.in, fill = N)) +
  geom_tile() +
  geom_text(aes(label = N), color = "white") +
  labs(title = "Transmission Flows Between Subpopulations",
       x = "Source (infected by)",
       y = "Destination (current location)") +
  theme_minimal()





#how to visualize this nice?
#Example combined data (spillover + spillback)
spillover <- data.table(inf.in = c("Humans", "Humans"),
                        current.in = c("Gorilla2", "Gorilla1"),
                        N = c(5, 15))

spillback <- data.table(inf.in = character(0), current.in = character(0), N = integer(0))

# Add spillover type
spillover[, type := "Spillover"]
spillback[, type := "Spillback"]

# Combine
combined <- rbind(spillover, spillback, fill=TRUE)

# If spillback is empty, add zero rows for plotting
if(nrow(spillback) == 0){
  combined <- rbind(combined,
                    data.table(inf.in = c("Gorilla1", "Gorilla2"),
                               current.in = rep("Humans", 2),
                               N = 0,
                               type = "Spillback"))
}

# Plot
ggplot(spillover, aes(x = inf.in, y = N, fill = current.in)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~type) +
  labs(title = "Spillover and Spillback Transmission Counts",
       x = "Source Population (infector)",
       y = "Number of Transmission Events",
       fill = "Recipient Population") +
  theme_minimal()









#sankey diagram (never heard that name before)
library(networkD3)
library(data.table)

# Sample data - replace with your own if needed
spillover <- data.table(inf.in = c("Humans", "Humans"),
                        current.in = c("Gorilla2", "Gorilla1"),
                        N = c(5, 15))

spillback <- data.table(inf.in = character(0), current.in = character(0), N = integer(0))

# Combine data (spillback empty here)
all_flows <- rbind(spillover, spillback)

# Create nodes data.table
nodes <- data.table(name = unique(c(all_flows$inf.in, all_flows$current.in)))

# Map source/target to zero-based indices
all_flows[, source := match(inf.in, nodes$name) - 1]
all_flows[, target := match(current.in, nodes$name) - 1]

links <- all_flows[, .(source, target, value = N)]

# Check your data:
print(nodes)
print(links)

# Plot
sankeyNetwork(Links = links, Nodes = nodes,
              Source = "source", Target = "target",
              Value = "value", NodeID = "name",
              fontSize = 14, nodeWidth = 30)  # increase width

#save this plot
saveNetwork(sankeyNetwork(Links = links, Nodes = nodes,
                          Source = "source", Target = "target",
                          Value = "value", NodeID = "name",
                          fontSize = 14, nodeWidth = 30),
            file = "sankey_diagram.html")






