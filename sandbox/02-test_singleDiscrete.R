#this script is for testing the updated/modified singleDiscrete function
#(which is being updated/modified in the single-Discrete.R script!)

#clear environment
rm(list = ls())

#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)


#defining simulation:

init.individuals <- 2
init.structure <- c("A", "B")

# Movement matrix: individuals can move between A and B
structure.matrix <- matrix(c(0.9, 0.1,
                             0.1, 0.9),
                           nrow = 2, byrow = TRUE)
colnames(structure.matrix) <- rownames(structure.matrix) <- c("A", "B")

# --- Population dynamics ---
initial.population.structure <- list(A = 100, B = 50)
birth.rate.structure <- list(A = 0.05, B = 0.1)  # Higher birth rate in B
death.rate.structure <- list(A = 0.01, B = 0.03) # Higher death rate in B

# Empty param lists for now
param.birth.rate.structure <- list()
param.death.rate.structure <- list()

# --- Epidemic functions (simple, constant values) ---
pExit <- function(t, ...) return(0.05)
pMove <- function(t, ...) return(0.1)
nContact <- function(t, ...) return(3)
pTrans <- function(t, ...) return(0.2)

# --- Wrapper call to your function ---
sim <- singleDiscrete(length.sim = 100,
                      max.infected = 1000,
                      init.individuals = init.individuals,
                      init.structure = init.structure,
                      structure.matrix = structure.matrix,

                      initial.population.structure = initial.population.structure,
                      birth.rate.structure = birth.rate.structure,
                      param.birth.rate.structure = param.birth.rate.structure,
                      death.rate.structure = death.rate.structure,
                      param.death.rate.structure = param.death.rate.structure,

                      pExit = pExit,
                      param.pExit = list(),
                      pMove = pMove,
                      param.pMove = list(),
                      nContact = nContact,
                      param.nContact = list(),
                      pTrans = pTrans,
                      param.pTrans = list(),

                      diff.pExit = FALSE,
                      timeDep.pExit = FALSE,
                      hostCount.pExit = FALSE,
                      diff.pMove = FALSE,
                      timeDep.pMove = FALSE,
                      hostCount.pMove = FALSE,
                      diff.nContact = FALSE,
                      timeDep.nContact = FALSE,
                      hostCount.nContact = FALSE,
                      diff.pTrans = FALSE,
                      timeDep.pTrans = FALSE,
                      hostCount.pTrans = FALSE,

                      print.progress = TRUE)









#---------------------
# Suppose your structure matrix has locations A, B, C
structure.matrix <- matrix(c(0.8, 0.1, 0.1,
                             0.2, 0.7, 0.1,
                             0.1, 0.2, 0.7),
                           nrow=3, byrow=TRUE)
rownames(structure.matrix) <- colnames(structure.matrix) <- c("A", "B", "C")

initial.population.structure <- list(A=1000, B=500, C=300)
birth.rate.structure <- list(A=0.05, B=0.02, C=0.03)
death.rate.structure <- list(A=0.01, B=0.01, C=0.01)

res <- singleDiscrete(length.sim = 100,
                      max.infected = 10000,
                      init.individuals = 10,
                      init.structure = rep("A", 10),  # all start in A
                      structure.matrix = structure.matrix,
                      initial.population.structure = initial.population.structure,
                      birth.rate.structure = birth.rate.structure,
                      param.birth.rate.structure = NULL,  # or appropriate parameters if needed
                      death.rate.structure = death.rate.structure,
                      param.death.rate.structure = NULL,
                      # other required params like pExit, pMove, pTrans, nContact, etc.
                      pExit = pExit,
                      param.pExit = list(),
                      pMove = pMove,
                      param.pMove = list(),
                      nContact = nContact,
                      param.nContact = list(),
                      pTrans = pTrans,
                      param.pTrans = list())






#apparently there is some error with MatrixSanityChecks, but I really dont feel like fixing/modifying another core script. so lets see if we can override it by running it here:
MatrixSanityChecks <- function(structure.matrix, init.structure, none.at.start=NULL) {
  if (!is.matrix(structure.matrix)) stop("structure.matrix should be a matrix.")
  if (ncol(structure.matrix)!=nrow(structure.matrix)) stop("structure.matrix should have the same number of rows and columns.")
  if (!identical(colnames(structure.matrix),rownames(structure.matrix))) stop("structure.matrix rows and columns should have the same names.")
  if (!isTRUE(all.equal(rep(1, ncol(structure.matrix)), rowSums(structure.matrix), check.attributes = FALSE))) stop("structure.matrix rows should sum up to 1.")

  if (is.null(none.at.start) && any(!(init.structure %in% rownames(structure.matrix)))) stop("init.structure should be a state present in structure.matrix.")

  if(!is.null(none.at.start) && none.at.start==FALSE && any(!(init.structure %in% rownames(structure.matrix)))) stop("init.structure should be a state present in structure.matrix.")
}

environment(MatrixSanityChecks) <- asNamespace("nosoi")
assignInNamespace("MatrixSanityChecks", MatrixSanityChecks, ns = "nosoi")






#now there is an error with initializing the table:
#Starting the simulation
#Initializing ...Error in setkeyv(x, cols, verbose = verbose, physical = physical) :
#  some columns are not in the data.table: [hosts.ID]

#instead of fixing it in the core, we are going to override this again:
newLine <- function(hosts.ID,
                    infected.by,
                    infected.in,
                    time.is,
                    ParamHost,
                    current.environmental.value = NULL,
                    current.cell.number.raster = NULL,
                    current.count.A = 0,
                    current.count.B = 0) {

  # force scalar
  scalarInt <- function(x) if (length(x) == 0) 0L else as.integer(x)

  list(
    hosts.ID     = as.character(hosts.ID),
    inf.by       = as.character(infected.by),
    inf.in       = as.character(infected.in),
    current.in   = as.character(infected.in),
    host.count.A = as.integer(current.count.A),
    host.count.B = as.integer(current.count.B),
    inf.time     = as.integer(time.is),
    out.time     = NA_integer_,
    active       = TRUE
  )
}


iniTable <- function(init.individuals, init.structure, prefix.host, ParamHost,
                     current.environmental.value = NULL, current.cell.number.raster = NULL,
                     current.count.A = 0L,
                     current.count.B = 0L) {

  if (init.individuals >= 1){
    list.init <- vector("list", init.individuals)

    for (indiv in 1:init.individuals) {
      list.init[[indiv]] <- newLine(
        hosts.ID = paste(prefix.host, indiv, sep = "-"),
        infected.by = paste(NA, indiv, sep = "-"),
        infected.in = init.structure[indiv],
        time.is = 0L,
        ParamHost = ParamHost,
        current.environmental.value = current.environmental.value,
        current.cell.number.raster = current.cell.number.raster,
        current.count.A = current.count.A,
        current.count.B = current.count.B
      )
    }
    table.hosts <- data.table::rbindlist(list.init, fill = TRUE)
  }

  if (init.individuals == 0){
    str(list.init[[1]])
    table.hosts <- data.table::rbindlist(list(), fill = TRUE)
    table.hosts <- table.hosts[0]
  }

  data.table::setkey(table.hosts, "hosts.ID")

  return(table.hosts)
}


res <- singleDiscrete(length.sim = 100,
                      max.infected = 10000,
                      init.individuals = 10,
                      init.structure = rep("A", 10),  # all start in A
                      structure.matrix = structure.matrix,
                      initial.population.structure = initial.population.structure,
                      birth.rate.structure = birth.rate.structure,
                      param.birth.rate.structure = NULL,  # or appropriate parameters if needed
                      death.rate.structure = death.rate.structure,
                      param.death.rate.structure = NULL,
                      # other required params like pExit, pMove, pTrans, nContact, etc.
                      pExit = pExit,
                      param.pExit = list(),
                      pMove = pMove,
                      param.pMove = list(),
                      nContact = nContact,
                      param.nContact = list(),
                      pTrans = pTrans,
                      param.pTrans = list())






