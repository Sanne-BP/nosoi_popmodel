#' @title Single-host pathogen in a structured (discrete) host population
#'
#' @description This function, that can be wrapped within \code{\link{nosoiSim}}, runs a single-host transmission chain simulation, with a discrete host population structure (e.g. spatial, socio-economic, etc.). The simulation stops either at
#' the end of given time (specified by \code{length.sim}) or when the number of hosts infected threshold (\code{max.infected}) is crossed.
#'
#' @section Structure Matrix:
#' The structure matrix provided provided should of class \code{matrix}, with the same number of rows and columns, rows representing departure state and column the arrival state. All rows should add to 1.
#' @section Structure Parameters:
#' The \code{pMove} function should return a single probability (a number between 0 and 1).
#' @section Structure Parameters:
#' The use of \code{diff} (switch to \code{TRUE}) makes the corresponding function use the argument \code{current.in} (for "currently in"). Your function should in that case give a result for every possible discrete state.
#' @section Structure Parameters:
#' The use of \code{hostCount} (switch to \code{TRUE}) makes the corresponding function use the argument \code{host.count}.
#' @section Order of Arguments:
#' The user specified function's arguments should follow this order: \code{t} (mandatory), \code{prestime} (optional, only if timeDep is TRUE),
#' \code{current.in} (optional, only if diff is TRUE), \code{host.count} (optional, only if hostCount is TRUE) and \code{parameters} specified in the list.
#'
#' @inheritParams singleNone
#' @param init.structure in which state (e.g. location) the initially infected individuals are located.
#' @param structure.matrix transition matrix (probabilities) to go from location A (row) to B (column)
#' @param diff.pMove is pMove different between states of the structured population (TRUE/FALSE)
#' @param timeDep.pMove is pMove dependent on the absolute time of the simulation (TRUE/FALSE)
#' @param hostCount.pMove does pMove varies with the host count in the state? (TRUE/FALSE); diff.pMove should be TRUE.
#' @param pMove function that gives the probability of a host moving as a function of time.
#' @param param.pMove parameter names (list of functions) for the pMove.
#' @param diff.nContact is nContact different between states of the structured population (TRUE/FALSE)
#' @param hostCount.nContact does nContact varies with the host count in the state? (TRUE/FALSE); diff.nContact should be TRUE.
#' @param diff.pTrans is pTrans different between states of the structured population (TRUE/FALSE)
#' @param hostCount.pTrans does pTrans varies with the host count in the state? (TRUE/FALSE); diff.pTrans should be TRUE.
#' @param diff.pExit is pExit different between states of the structured population (TRUE/FALSE)
#' @param hostCount.pExit does pExit varies with the host count in the state? (TRUE/FALSE); diff.pExit should be TRUE.
#' @param initial.population initial size of the host population.
#' @param birth.rate birth rate per individual per time step.
#' @param death.rate death rate per individual per time step.
#'
#' @inherit singleNone return details
#'
#' @seealso For simulations with a structure in continuous space, see \code{\link{singleContinuous}}. For simulations without any structures, see \code{\link{singleNone}}.
#'
#' @examples
#' \donttest{
#' t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#' p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#' p_Exit_fct  <- function(t){return(0.08)}
#' p_Move_fct  <- function(t){return(0.1)}
#'
#' proba <- function(t,p_max,t_incub){
#'  if(t <= t_incub){p=0}
#'  if(t >= t_incub){p=p_max}
#'  return(p)
#' }
#'
#' time_contact = function(t){round(rnorm(1, 3, 1), 0)}
#'
#' transition.matrix = matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),
#'                            nrow = 3, ncol = 3,
#'                            dimnames=list(c("A","B","C"),c("A","B","C")))
#'
#' set.seed(805)
#' test.nosoiA <- nosoiSim(type="single", popStructure="discrete",
#'                        length=20,
#'                        max.infected=100,
#'                        init.individuals=1,
#'                        init.structure="A",
#'                        structure.matrix=transition.matrix,
#'                        pMove=p_Move_fct,
#'                        param.pMove=NA,
#'                        nContact=time_contact,
#'                        param.nContact=NA,
#'                        pTrans = proba,
#'                        param.pTrans = list(p_max=p_max_fct,
#'                                            t_incub=t_incub_fct),
#'                       pExit=p_Exit_fct,
#'                       param.pExit=NA)
#'}
#' @export singleDiscrete
singleDiscrete <- function(length.sim,
                           max.infected,
                           init.individuals.A,
                           init.individuals.B,
                           pExit.A, param.pExit.A, timeDep.pExit.A=FALSE,
                           pExit.B, param.pExit.B, timeDep.pExit.B=FALSE,
                           nContact.A, param.nContact.A, timeDep.nContact.A=FALSE,
                           nContact.B, param.nContact.B, timeDep.nContact.B=FALSE,
                           pTrans.A, param.pTrans.A, timeDep.pTrans.A=FALSE,
                           pTrans.B, param.pTrans.B, timeDep.pTrans.B=FALSE,
                           prefix.host.A="A",
                           prefix.host.B="B",
                           print.progress=TRUE,
                           print.step=10,
                           initial.population.A=10000,
                           birth.rate.A=0.5,
                           death.rate.A=0.5,
                           initial.population.B=1000,
                           birth.rate.B=0.8,
                           death.rate.B=0.6) {

  # Sanity check
  CoreSanityChecksDiscrete(length.sim, max.infected, init.individuals.A, init.individuals.B)

  # Parse functions
  nContactParsed.A <- parseFunction(nContact.A, param.nContact.A, as.character(quote(nContact.A)), timeDep.nContact.A)
  nContactParsed.B <- parseFunction(nContact.B, param.nContact.B, as.character(quote(nContact.B)), timeDep.nContact.B)

  pTransParsed.A <- parseFunction(pTrans.A, param.pTrans.A, as.character(quote(pTrans.A)), timeDep.pTrans.A)
  pTransParsed.B <- parseFunction(pTrans.B, param.pTrans.B, as.character(quote(pTrans.B)), timeDep.pTrans.B)

  pExitParsed.A <- parseFunction(pExit.A, param.pExit.A, as.character(quote(pExit.A)), timeDep.pExit.A)
  pExitParsed.B <- parseFunction(pExit.B, param.pExit.B, as.character(quote(pExit.B)), timeDep.pExit.B)

  # Parameter parsing
  ParamHost.A <- paramConstructor(param.pExit.A, NA, param.nContact.A, param.pTrans.A, NA)
  ParamHost.B <- paramConstructor(param.pExit.B, NA, param.nContact.B, param.pTrans.B, NA)

  # Initialization
  message("Starting the simulation\nInitializing ...", appendLF = FALSE)

  res <- nosoiSimConstructor(
    total.time = 1,
    type = "dual",
    pop.A = nosoiSimOneConstructor(N.infected = init.individuals.A,
                                   table.hosts = iniTable(init.individuals.A, NA, prefix.host.A, ParamHost.A),
                                   table.state = NA,
                                   prefix.host = prefix.host.A,
                                   popStructure = "discrete"),
    pop.B = nosoiSimOneConstructor(N.infected = init.individuals.B,
                                   table.hosts = iniTable(init.individuals.B, NA, prefix.host.B, ParamHost.B),
                                   table.state = NA,
                                   prefix.host = prefix.host.B,
                                   popStructure = "discrete"))

  res[["table.state"]] <- data.table::data.table(
    time = integer(),
    population.A = numeric(),
    infected.A = integer(),
    population.B = numeric(),
    infected.B = integer())

  print(str(res$table.state))

  # Initialize population size vectors
  PopModel.A <- numeric(length.sim + 1)
  PopModel.B <- numeric(length.sim + 1)
  PopModel.A[1] <- initial.population.A
  PopModel.B[1] <- initial.population.B

  message(" running ...")

  for (pres.time in 1:length.sim) {
    ### -- SUBPOPULATION A --
    births.A <- rpois(1, birth.rate.A * PopModel.A[pres.time])
    deaths.A <- rbinom(1, PopModel.A[pres.time], death.rate.A)

    exiting.full.A <- getExitingMoving(res$host.info.A, pres.time, pExitParsed.A)
    if (!is.logical(exiting.full.A)) {
      exiting.full.A <- seq_len(nrow(res$host.info.A$table.hosts)) %in% exiting.full.A
    }
    res$host.info.A$table.hosts[exiting.full.A, `:=` (out.time = pres.time, active = FALSE)]
    epidemic_deaths.A <- sum(exiting.full.A)

    PopModel.A[pres.time + 1] <- max(0, PopModel.A[pres.time] + births.A - deaths.A - epidemic_deaths.A)

    ### -- SUBPOPULATION B --
    births.B <- rpois(1, birth.rate.B * PopModel.B[pres.time])
    deaths.B <- rbinom(1, PopModel.B[pres.time], death.rate.B)

    exiting.full.B <- getExitingMoving(res$host.info.B, pres.time, pExitParsed.B)
    if (!is.logical(exiting.full.B)) {
      exiting.full.B <- seq_len(nrow(res$host.info.B$table.hosts)) %in% exiting.full.B
    }
    res$host.info.B$table.hosts[exiting.full.B, `:=` (out.time = pres.time, active = FALSE)]
    epidemic_deaths.B <- sum(exiting.full.B)

    PopModel.B[pres.time + 1] <- max(0, PopModel.B[pres.time] + births.B - deaths.B - epidemic_deaths.B)

    # Break if no active hosts
    if (!any(res$host.info.A$table.hosts$active) && !any(res$host.info.B$table.hosts$active)) {
      break
    }

    # Step: Contact and transmission
    df.meetTransmit.A <- meetTransmit(res$host.info.A, pres.time, positions = NULL, nContactParsed.A, pTransParsed.A)
    df.meetTransmit.B <- meetTransmit(res$host.info.B, pres.time, positions = NULL, nContactParsed.B, pTransParsed.B)

    res$host.info.A <- writeInfected(df.meetTransmit.A, res$host.info.A, pres.time, ParamHost.A)
    res$host.info.B <- writeInfected(df.meetTransmit.B, res$host.info.B, pres.time, ParamHost.B)

    # Construct temporary state table for this time step (new)
    table.state.temp <- data.table::data.table(
      time = pres.time,
      population.A = PopModel.A[pres.time + 1],
      infected.A = sum(res$host.info.A$table.hosts$active),
      population.B = PopModel.B[pres.time + 1],
      infected.B = sum(res$host.info.B$table.hosts$active))
    print(class(table.state.temp))

    if (!data.table::is.data.table(res$table.state)) {
      res$table.state <- data.table::data.table(
        time = integer(),
        population.A = numeric(),
        infected.A = integer(),
        population.B = numeric(),
        infected.B = integer()
      )}

    # Append to full state table (new)
    res$table.state <- data.table::rbindlist(
      list(res$table.state, table.state.temp),
      use.names = TRUE, fill = TRUE)
    print(class(res$table.state))
    print(class(table.state.temp))
    print(str(res$table.state))

    # Progress update
    if (print.progress && (pres.time %% print.step == 0)) {
      progressMessage(
        Host.count.A = res$host.info.A$N.infected,
        Host.count.B = res$host.info.B$N.infected,
        pres.time = pres.time,
        print.step = print.step,
        length.sim = length.sim,
        max.infected.A = max.infected,
        max.infected.B = max.infected
      )
    }

    if ((res$host.info.A$N.infected + res$host.info.B$N.infected) > max.infected) {
      break
    }
  }

  # Finalize
  endMessage(res$host.info.A$N.infected + res$host.info.B$N.infected, pres.time)

  res$total.time <- pres.time
  res$pop_model <- list(
    A = PopModel.A[1:(pres.time + 1)],
    B = PopModel.B[1:(pres.time + 1)]
  )

  return(res)
}
