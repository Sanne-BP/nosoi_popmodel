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

