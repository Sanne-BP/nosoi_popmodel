#this script is for testing the updated/modified dualDiscrete function
#(which is being updated/modified in the dual-Discrete.R script!)

#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)

# Dummy structure matrices (2 states, simple equal transition)
structure.matrix.A <- matrix(c(0.5, 0.5, 0.5, 0.5), nrow=2, byrow=TRUE)
rownames(structure.matrix.A) <- colnames(structure.matrix.A) <- c("state1", "state2")
structure.matrix.B <- structure.matrix.A

# Dummy functions that do almost nothing
pExit.A <- function(t) { return(0) }
pMove.A <- function(t) { return(0) }
nContact.A <- function(t) { return(0) }
pTrans.A <- function(t) { return(0) }

pExit.B <- function(t) { return(0) }
pMove.B <- function(t) { return(0) }
nContact.B <- function(t) { return(0) }
pTrans.B <- function(t) { return(0) }

# Run the simulation with 1 initially infected in state1 for both A and B
result <- dualDiscrete(
  length.sim = 3,
  max.infected.A = 10,
  max.infected.B = 10,
  init.individuals.A = 1,
  init.individuals.B = 1,
  init.structure.A = "state1",
  init.structure.B = "state1",
  structure.matrix.A = structure.matrix.A,
  structure.matrix.B = structure.matrix.B,
  pExit.A = pExit.A,
  pMove.A = pMove.A,
  nContact.A = nContact.A,
  pTrans.A = pTrans.A,
  pExit.B = pExit.B,
  pMove.B = pMove.B,
  nContact.B = nContact.B,
  pTrans.B = pTrans.B,
  print.progress = TRUE,
  print.step = 5
)

print(result)





result <- dualDiscrete(
  length.sim = 5,
  max.infected.A = 10,
  max.infected.B = 10,
  init.individuals.A = 1,
  init.individuals.B = 1,
  init.structure.A = "state1",
  init.structure.B = "state1",
  structure.matrix.A = structure.matrix.A,
  structure.matrix.B = structure.matrix.B,
  pExit.A = pExit.A,
  pMove.A = pMove.A,
  nContact.A = nContact.A,
  pTrans.A = pTrans.A,
  pExit.B = pExit.B,
  pMove.B = pMove.B,
  nContact.B = nContact.B,
  pTrans.B = pTrans.B,
  print.progress = TRUE,
  print.step = 1
)


#Running simulation step: 1
#Running simulation step: 2
#Running simulation step: 3
#Running simulation step: 4
#Running simulation step: 5

#whyyyyyy where does this even come from?!

# Example structure matrices with 2 states
yourStructureMatrixA <- matrix(c(0.8, 0.2,
                                 0.1, 0.9),
                               nrow = 2, byrow = TRUE)
rownames(yourStructureMatrixA) <- colnames(yourStructureMatrixA) <- c("state1", "state2")

yourStructureMatrixB <- yourStructureMatrixA  # For simplicity, same as A here

# Example functions (dummy placeholders you will replace)

your_pExit_A <- function(...) { 0.1 }  # 10% exit prob for demo
your_pMove_A <- function(...) { "state1" } # Always move to state1 (dummy)
your_nContact_A <- function(...) { 5 }  # 5 contacts per infected
your_pTrans_A <- function(...) { 0.2 }  # 20% transmission prob

your_pExit_B <- function(...) { 0.1 }
your_pMove_B <- function(...) { "state1" }
your_nContact_B <- function(...) { 5 }
your_pTrans_B <- function(...) { 0.2 }


result <- dualDiscrete(
  length.sim = 36,
  max.infected.A = 500,
  max.infected.B = 500,
  init.individuals.A = 1,
  init.individuals.B = 1,
  init.structure.A = "state1",
  init.structure.B = "state1",
  structure.matrix.A = yourStructureMatrixA,
  structure.matrix.B = yourStructureMatrixB,
  pExit.A = your_pExit_A,
  pMove.A = your_pMove_A,
  nContact.A = your_nContact_A,
  pTrans.A = your_pTrans_A,
  pExit.B = your_pExit_B,
  pMove.B = your_pMove_B,
  nContact.B = your_nContact_B,
  pTrans.B = your_pTrans_B,
  print.progress = TRUE,
  print.step = 5
)














#-------------
# pExit: probability an individual exits at each time step (random)
your_pExit_A <- function(time, infected, ...) {
  runif(1) < 0.05  # 5% chance to exit per time step
}
your_pExit_B <- function(time, infected, ...) {
  runif(1) < 0.07  # 7% chance to exit per time step
}

# pMove: randomly move between two states with given probabilities
your_pMove_A <- function(current_state, ...) {
  sample(c("state1", "state2"), size = 1, prob = c(0.7, 0.3))
}
your_pMove_B <- function(current_state, ...) {
  sample(c("state1", "state2"), size = 1, prob = c(0.5, 0.5))
}

# nContact: number of contacts per time step (Poisson random variable)
your_nContact_A <- function(...) {
  rpois(1, lambda = 5)  # mean 5 contacts
}
your_nContact_B <- function(...) {
  rpois(1, lambda = 3)  # mean 3 contacts
}

# pTrans: fixed transmission probability per contact
your_pTrans_A <- function(...) {
  0.2  # 20% chance transmission
}
your_pTrans_B <- function(...) {
  0.15  # 15% chance transmission
}

# 2 states: state1 and state2
yourStructureMatrixA <- matrix(c(0.7, 0.3,
                                 0.4, 0.6), nrow=2, byrow=TRUE,
                               dimnames = list(c("state1", "state2"), c("state1", "state2")))

yourStructureMatrixB <- matrix(c(0.5, 0.5,
                                 0.2, 0.8), nrow=2, byrow=TRUE,
                               dimnames = list(c("state1", "state2"), c("state1", "state2")))



result <- dualDiscrete(
  length.sim = 36,
  max.infected.A = 500,
  max.infected.B = 500,
  init.individuals.A = 1,
  init.individuals.B = 1,
  init.structure.A = "state1",
  init.structure.B = "state1",
  structure.matrix.A = yourStructureMatrixA,
  structure.matrix.B = yourStructureMatrixB,
  pExit.A = your_pExit_A,
  pMove.A = your_pMove_A,
  nContact.A = your_nContact_A,
  pTrans.A = your_pTrans_A,
  pExit.B = your_pExit_B,
  pMove.B = your_pMove_B,
  nContact.B = your_nContact_B,
  pTrans.B = your_pTrans_B,
  print.progress = TRUE,
  print.step = 5
)















#---- new implementation so lets test:
# Simple constant function returning a fixed probability
constant_prob <- function(time, param) {
  return(param)
}

# Parsing function stub (assuming same as your environment)
parseFunction <- function(fun, param, name, timeDep=FALSE) {
  return(function(time) fun(time, param))
}

# Example getExitingMoving - returns indices of hosts exiting due to epidemic (simulate random exits)
getExitingMoving <- function(host.info, time, pExitFun) {
  active_hosts <- which(host.info$table.hosts$active)
  if (length(active_hosts) == 0) return(logical(0))
  prob_exit <- pExitFun(time)
  exiting <- active_hosts[runif(length(active_hosts)) < prob_exit]
  return(exiting)
}

# Dummy meetTransmit function - returns empty dataframe for testing
meetTransmit <- function(host.info, time, positions, nContactFun, pTransFun) {
  # For testing, return empty data.frame (no new infections)
  return(data.frame())
}

# Dummy writeInfected function - no changes for testing
writeInfected <- function(df, host.info, time, ParamHost) {
  # Just return host.info unchanged
  return(host.info)
}

# Dummy progressMessage and endMessage
progressMessage <- function(...) { }
endMessage <- function(...) { }

# Dummy paramConstructor
paramConstructor <- function(...) { list() }

# Dummy iniTable function to create initial table of hosts
iniTable <- function(n, extra, prefix, ParamHost) {
  # Create data.table with n hosts all active
  library(data.table)
  dt <- data.table(id = paste0(prefix, seq_len(n)),
                   active = TRUE,
                   out.time = NA_integer_)
  return(dt)
}

# Dummy nosoiSimOneConstructor
nosoiSimOneConstructor <- function(N.infected, table.hosts, table.state, prefix.host, popStructure) {
  list(N.infected = N.infected,
       table.hosts = table.hosts,
       table.state = table.state,
       prefix.host = prefix.host,
       popStructure = popStructure)
}

# Dummy nosoiSimConstructor
nosoiSimConstructor <- function(total.time, type, pop.A, pop.B) {
  list(total.time = total.time,
       type = type,
       host.info.A = pop.A,
       host.info.B = pop.B)
}

result <- dualDiscrete(length.sim = 100,
                       max.infected.A = 10000,
                       max.infected.B = 10000,
                       init.individuals.A = 5,
                       init.individuals.B = 5,
                       pExit.A = constant_prob, param.pExit.A = 0.1,
                       pExit.B = constant_prob, param.pExit.B = 0.05,
                       nContact.A = constant_prob, param.nContact.A = 10,
                       nContact.B = constant_prob, param.nContact.B = 5,
                       pTrans.A = constant_prob, param.pTrans.A = 0.2,
                       pTrans.B = constant_prob, param.pTrans.B = 0.3,
                       prefix.host.A = "A",
                       prefix.host.B = "B",
                       print.progress = FALSE,
                       initial.population.A = 10000,
                       initial.population.B = 5000,
                       birth.rate.A = 0.5,
                       birth.rate.B = 0.3,
                       death.rate.A = 0.5,
                       death.rate.B = 0.3)

#Starting the simulation
#Initializing ...Starting the simulation
#Initializing ... running ...
#done.
#The simulation has run for 20 units of time and a total of 10 hosts have been infected.


#Not completely satisfied with this message, it should be more like this, but how???:
##> Starting the simulation
#> Initializing ... running ...
#> done.
#> The simulation has run for 34 units of time and a total of 106 (A) and 129 (B) hosts have been infected.





















#this was apparently a very dummy test version thanks chatgpt, you are wonderful
#so lets fix this:
# Dummy versions of internal nosoi helpers ---------------------

# ParamHost constructor (light wrapper)
paramConstructor <- function(param.pExit, param.pMove, param.nContact, param.pTrans, param.sdMove) {
  list(pExit = param.pExit, pMove = param.pMove,
       nContact = param.nContact, pTrans = param.pTrans,
       sdMove = param.sdMove)
}

# Fake initial host table constructor
iniTable <- function(n.infected, state, prefix.host, params) {
  data.table::data.table(
    host.ID = paste0(prefix.host, "_", seq_len(n.infected)),
    inf.time = 0,
    out.time = NA,
    active = TRUE
  )
}

# Minimal simulation constructor
nosoiSimOneConstructor <- function(N.infected, table.hosts, table.state, prefix.host, popStructure) {
  list(
    N.infected = N.infected,
    table.hosts = table.hosts,
    table.state = table.state,
    prefix.host = prefix.host,
    popStructure = popStructure
  )
}

nosoiSimConstructor <- function(total.time, type, pop.A, pop.B) {
  list(
    total.time = total.time,
    type = type,
    host.info.A = pop.A,
    host.info.B = pop.B
  )
}

# Parse function wrapper
parseFunction <- function(fct, param, name, timeDep=FALSE) {
  return(function(time) fct(time, param))  # wrap in closure
}

# Epidemic exit determination
getExitingMoving <- function(host.info, time, pExit.fct) {
  active.idx <- which(host.info$table.hosts$active)
  prob.exit <- pExit.fct(time)
  exit <- stats::rbinom(length(active.idx), 1, prob.exit) == 1
  active.idx[exit]
}

# Contact and transmission
meetTransmit <- function(host.info, time, positions, nContact.fct, pTrans.fct) {
  active.idx <- which(host.info$table.hosts$active)
  n.contacts <- nContact.fct(time)
  new.infections <- stats::rbinom(length(active.idx), n.contacts, pTrans.fct(time))
  data.frame(
    host = host.info$table.hosts$host.ID[active.idx],
    new.infections = new.infections
  )
}

# Infect hosts
writeInfected <- function(df.meet, host.info, time, paramHost) {
  total.new <- sum(df.meet$new.infections)
  if (total.new > 0) {
    new.IDs <- paste0(host.info$prefix.host, "_", nrow(host.info$table.hosts) + seq_len(total.new))
    new.entries <- data.table::data.table(
      host.ID = new.IDs,
      inf.time = time,
      out.time = NA,
      active = TRUE
    )
    host.info$table.hosts <- data.table::rbindlist(list(host.info$table.hosts, new.entries), use.names = TRUE)
    host.info$N.infected <- host.info$N.infected + total.new
  }
  return(host.info)
}

# Progress bar
progressMessage <- function(Host.count.A, Host.count.B, pres.time, print.step, length.sim, max.infected.A, max.infected.B) {
  cat(sprintf("[Time %d] Infected A: %d | Infected B: %d\n", pres.time, Host.count.A, Host.count.B))
}

# Simple constant-probability function
constant_prob <- function(time, param) param

# Now run your simulation
result <- dualDiscrete(
  length.sim = 100,
  max.infected.A = 100000,
  max.infected.B = 100000,
  init.individuals.A = 10,
  init.individuals.B = 5,
  pExit.A = constant_prob, param.pExit.A = 0.1,
  pExit.B = constant_prob, param.pExit.B = 0.05,
  nContact.A = constant_prob, param.nContact.A = 5,
  nContact.B = constant_prob, param.nContact.B = 3,
  pTrans.A = constant_prob, param.pTrans.A = 0.2,
  pTrans.B = constant_prob, param.pTrans.B = 0.3,
  prefix.host.A = "H",
  prefix.host.B = "W",
  initial.population.A = 100000,
  initial.population.B = 50000,
  birth.rate.A = 0.3,
  birth.rate.B = 0.05,
  death.rate.A = 0.3,
  death.rate.B = 0.05,
  print.progress = TRUE,
  print.step = 10
)
#The simulation has run for 6 units of time and a total of 5861 (A) and 795 (B) hosts have been infected.

#lets create some visualizations:
result$total.time
result$type
result$host.info.A
result$host.info.A$N.infected
result$host.info.B$N.infected
result$host.info.A$table.hosts

summary(results)

getCumulative_dual <- function(host.info) {
  tab <- host.info$table.hosts
  tab[, t := inf.time]
  cumulative <- tab[, .N, by = t][order(t)]
  cumulative[, Count := cumsum(N)]
  return(data.frame(t = cumulative$t, Count = cumulative$Count))}

cumulative.A <- getCumulative_dual(result$host.info.A)
cumulative.B <- getCumulative_dual(result$host.info.B)

cumulative.A$host <- "A"
cumulative.B$host <- "B"

cumulative.table <- rbind(cumulative.A, cumulative.B)

ggplot(data = cumulative.table, aes(x = t, y = Count, color = host)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(x = "Time (t)", y = "Cumulative count of infected hosts",
       title = "Cumulative Infections Over Time")

getDynamic_dual <- function(host.info) {
  tab <- host.info$table.hosts
  times <- 0:max(tab$inf.time, na.rm = TRUE)

  dyn <- sapply(times, function(t) {
    sum(tab$inf.time <= t & (is.na(tab$out.time) | tab$out.time > t))
  })

  return(data.frame(t = times, Active = dyn))
}

dynamic.A <- getDynamic_dual(result$host.info.A)
dynamic.B <- getDynamic_dual(result$host.info.B)

dynamic.A$host <- "A"
dynamic.B$host <- "B"

dynamics.table <- rbind(dynamic.A, dynamic.B)

ggplot(data = dynamics.table, aes(x = t, y = Active, color = host)) +
  geom_line(size = 1.2) +
  theme_minimal() +
  labs(x = "Time (t)", y = "Active infections",
       title = "Active Infections Over Time")

## Plot 1: Cumulative
p3 <- ggplot(cumulative.table, aes(x = t, y = Count, color = host)) +
  geom_line(size = 1) +
  theme_minimal() +
  scale_colour_manual(values = c("A" = "blue", "B" = "red"),
                      labels = c("A" = "Population A", "B" = "Population B"))+
  labs(title = "Cumulative Infections", y = "Total Infected", x = "Time",
       colour = "Infected Individuals")+
  theme_minimal(base_size = 18)+
  theme(plot.title = element_text(face = "bold"))
p3

#Plot 2: Pop Model
pop_df <- data.table(
  time = 0:(length(result$pop_model.A) - 1),   # time 0 … last step
  PopA = result$pop_model.A,
  PopB = result$pop_model.B
)

pop_df_long <- melt(
  pop_df,
  id.vars   = "time",
  measure.vars = c("PopA", "PopB"),
  variable.name = "host",
  value.name   = "population"
)

## ---- 2. Quick visualisation ----
p4 <- ggplot(pop_df_long, aes(x = time, y = population, colour = host)) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("PopA" = "blue", "PopB" = "red"),
                      labels = c("PopA" = "A", "PopB" = "B"))+
  labs(title = "Population trajectories",
       x     = "Time",
       y     = "Population size",
       colour = "Population") +
  theme_minimal(base_size = 18)+
  theme(plot.title = element_text(face = "bold"))
p4

library(patchwork)
p4 + p3 + plot_layout(ncol = 1)
ggsave("sandbox/plots/presentation/dualdiscrete2.png", width = 10, height = 6, dpi = 300, bg = "white")



