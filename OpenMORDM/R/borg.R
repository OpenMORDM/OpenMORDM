# Borg Multiobjective Evolutionary Algorithm - R Wrapper
# Copyright 2013 David Hadka
#
# The Borg Multiobjective Evolutionary Algorithm (Borg MOEA) is a general-
# purpose optimization algorithm.  It's notable features include multiple
# search operators, auto-adaptive controls, and epsilon-dominance archiving.
#
# Hadka, D. and Reed, P. (2013).  "Borg: An Auto-Adaptive Many-Objective
# Evolutionary Computing Framework."  Evolutionary Computation, 21(2):231-259.
#
# The Borg MOEA is free and open for non-commercial users.  Source code can
# be obtained from \url{http://borgmoea.org}.

DOUBLE_PRECISION_WIDTH <- 8

setValue <- function(array, index, value) {
	.pack(array, DOUBLE_PRECISION_WIDTH*index, "d", value)
}

getValue <- function(array, index) {
	.unpack(array, DOUBLE_PRECISION_WIDTH*index, "d")
}

setValues <- function(array, values) {
	for (i in 1:length(values)) {
		setValue(array, i-1, values[i])
	}
}

getValues <- function(array, size) {
	result <- vector()
	
	for (i in 1:size) {
		result <- append(result, getValue(array, i-1))
	}
	
	return(result)
}

lookup <- function(parameters, key, defaultValue) {
	result <- parameters[key][[1]]
	
	if (is.null(result)) {
		result <- defaultValue
	} else {
		cat(paste("Setting parameter", key, "to", defaultValue))
		cat("\n")
	}
	
	return(result)
}

magicWrapper <- function(objectiveFcn, nvars, nobjs, nconstrs) {
	function(vars, objs, constrs) {
		exVars <- getValues(vars, nvars)
		result <- objectiveFcn(exVars)
		
		if (is.vector(result)) {
			if (length(result) == nobjs+nconstrs) {
				setValues(objs, result[1:nobjs])
				
				if (nconstrs > 0) {
					setValues(constrs, result[(nobjs+1):(nobjs+nconstrs)])
				}
			} else {
				stop("Objective function return value does not contain the correct number of elements")
			}
		} else if (is.list(result)) {
			if (length(result) == 1) {
				setValues(objs, result[1])
			} else if (length(result) == 2) {
				setValues(objs, result[1])
				setValues(constrs, result[2])
			} else {
				stop("Objective function return value does not contain the correct number of elements")
			}
		} else {
			stop("Objective function return value is not the correct class")
		}
		
	}
}

DOMINATES <- -2
DOMINATES_SAME_BOX <- -1
NONDOMINATED <- 0
DOMINATED_SAME_BOX <- 1
DOMINATED <- 2

RESTART_DEFAULT <- 0
RESTART_RANDOM <- 1
RESTART_RAMPED <- 2
RESTART_ADAPTIVE <- 3
RESTART_INVERTED <- 4

PROBABILITIES_DEFAULT <- 0
PROBABILITIES_RECENCY <- 1
PROBABILITIES_BOTH <- 2
PROBABILITIES_ADAPTIVE <- 3

borg <- function(nvars, nobjs, nconstrs, objectiveFcn, maxEvaluations, epsilons, lowerBounds=NA, upperBounds=NA, ...) {
	# validate arguments
	if (nvars < 1) {
		stop("Requires at least one decision variable")
	}
	
	if (nobjs < 1) {
		stop("Requires at least one objective")
	}
	
	if (nconstrs < 0) {
		stop("Number of constraints can not be negative")
	}
	
	if (maxEvaluations < 0) {
		stop("Requires a positive number of objective function evaluations (NFE)")
	}
	
	if (length(epsilons) != nobjs) {
		stop("Length of epsilon array must match the number of objectives")
	}
	
	if (any(is.na(lowerBounds) == TRUE)) {
		lowerBounds = rep(0, nvars)
	} else if (length(lowerBounds) != nvars) {
		stop("Length of lower bounds must match the number of decision variables")
	}
	
	if (any(is.na(upperBounds) == TRUE)) {
		upperBounds = rep(1, nvars)
	} else if (length(upperBounds) != nvars) {
		stop("Length of upper bounds must match the number of decision variables")
	}
	
	parameters <- list(...)
	
	# setup the callback function
	mw <- magicWrapper(objectiveFcn, nvars, nobjs, nconstrs)
	cb <- new.callback("ppp)v", mw);
	
	# load the DLL
	borglib <- dynbind(c("borg.dll", "libborg.so", "borg"),"
						   BORG_Debug_on()v;
						   BORG_Debug_off()v;
						   BORG_Debug_set_name(Z)v;
						   BORG_Problem_create(iiip)p;
						   BORG_Problem_destroy(p)v;
						   BORG_Problem_set_epsilon(pid)v;
						   BORG_Problem_set_name(piZ)v;
						   BORG_Problem_set_epsilons(p*)v;
						   BORG_Problem_set_bounds(pidd)v;
						   BORG_Problem_number_of_variables(p)i;
						   BORG_Problem_number_of_objectives(p)i;
						   BORG_Problem_number_of_constraints(p)i;
						   BORG_Solution_create(p)p;
						   BORG_Solution_destroy(p)v;
						   BORG_Solution_clone(p)p;
						   BORG_Solution_get_variable(pi)d;
						   BORG_Solution_get_objective(pi)d;
						   BORG_Solution_get_constraint(pi)d;
						   BORG_Solution_set_variables(pp)v;
						   BORG_Solution_get_problem(p)p;
						   BORG_Solution_evaluate(p)v;
						   BORG_Solution_print(pp)v;
						   BORG_Solution_initialize(p)v;
						   BORG_Solution_violates_constraints(p)i;
						   BORG_Dominance_pareto(pp)i;
						   BORG_Dominance_epsilon(pp)i;
						   BORG_Dominance_constraints(pp)i;
						   BORG_Dominance_compound(pppp)i;
						   BORG_Random_seed(J)v;
						   BORG_Random_uniform(dd)d;
						   BORG_Random_int(i)i;
						   BORG_Random_gaussian(dd)d;
						   BORG_Operator_create(Ziiip)p;
						   BORG_Operator_destroy(p)v;
						   BORG_Operator_get_probability(p)d;
						   BORG_Operator_set_parameter(pid)v;
						   BORG_Operator_set_parameters(pp)v;
						   BORG_Operator_get_number_of_offspring(p)i;
						   BORG_Operator_get_number_of_parents(p)i;
						   BORG_Operator_set_mutation(pp)v;
						   BORG_Operator_apply(ppp)v;
						   BORG_Operator_UM(ppp)v;
						   BORG_Operator_SBX(ppp)v;
						   BORG_Operator_PM(ppp)v;
						   BORG_Operator_DE(ppp)v;
						   BORG_Operator_SPX(ppp)v;
						   BORG_Operator_PCX(ppp)v;
						   BORG_Operator_UNDX(ppp)v;
						   BORG_Population_create(i)p;
						   BORG_Population_destroy(p)v;
						   BORG_Population_add(pp)v;
						   BORG_Population_reset(pi)v;
						   BORG_Population_tournament(pi)v;
						   BORG_Population_select(pipi)v;
						   BORG_Archive_create()p;
						   BORG_Archive_clone(p)p;
						   BORG_Archive_destroy(p)v;
						   BORG_Archive_add(pp)v;
						   BORG_Archive_get_size(p)i;
						   BORG_Archive_get(pi)p;
						   BORG_Archive_get_improvements(p)i;
						   BORG_Archive_select(p)p;
						   BORG_Archive_print(pp)v;
						   BORG_Archive_append(pp)v;
						   BORG_Algorithm_create(pi)p;
						   BORG_Algorithm_destroy(p)v;
						   BORG_Algorithm_validate(p)v;
						   BORG_Algorithm_get_number_restarts(p)i;
						   BORG_Algorithm_get_number_improvements(p)i;
						   BORG_Algorithm_set_window_size(pi)v;
						   BORG_Algorithm_set_initial_population_size(pi)v;
						   BORG_Algorithm_set_minimum_population_size(pi)v;
						   BORG_Algorithm_set_maximum_population_size(pi)v;
						   BORG_Algorithm_set_population_ratio(pd)v;
						   BORG_Algorithm_set_selection_ratio(pd)v;
						   BORG_Algorithm_set_restart_mode(pi)v;
						   BORG_Algorithm_set_probability_mode(pi)v;
						   BORG_Algorithm_get_mutation_index(p)i;
						   BORG_Algorithm_set_max_mutation_index(pi)v;
						   BORG_Algorithm_set_operator(pip)v;
						   BORG_Algorithm_update(p)v;
						   BORG_Algorithm_select(p)i;
						   BORG_Algorithm_shuffle(ip)v;
						   BORG_Algorithm_restart(p)v;
						   BORG_Algorithm_check(p)i;
						   BORG_Algorithm_step(p)v;
						   BORG_Algorithm_get_nfe(p)i;
						   BORG_Algorithm_get_population_size(p)i;
						   BORG_Algorithm_get_archive_size(p)i;
						   BORG_Algorithm_get_result(p)p;
						   BORG_Algorithm_run(pi)p;
						   ")
	
	BORG_Operator_PM_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_PM")
	BORG_Operator_SBX_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_SBX")
	BORG_Operator_DE_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_DE")
	BORG_Operator_UM_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_UM")
	BORG_Operator_SPX_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_SPX")
	BORG_Operator_PCX_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_PCX")
	BORG_Operator_UNDX_ptr <- .dynsym(borglib$libhandle, "BORG_Operator_UNDX")
	
	# setup the random number generator
	BORG_Random_seed(lookup(parameters, "rngstate", 4294967295))
	
	# setup the problem
	problem <- BORG_Problem_create(nvars, nobjs, nconstrs, cb)
	
	for (i in 1:nvars) {
		BORG_Problem_set_bounds(problem, i-1, lowerBounds[i], upperBounds[i])
	}
	
	for (i in 1:nobjs) {
		BORG_Problem_set_epsilon(problem, i-1, epsilons[i])
	}

	# setup the algorithm
	pm <- BORG_Operator_create("PM", 1, 1, 2, BORG_Operator_PM_ptr)
	BORG_Operator_set_parameter(pm, 0, lookup(parameters, "pm.rate", 1.0 / nvars))
	BORG_Operator_set_parameter(pm, 1, lookup(parameters, "pm.distributionIndex", 20.0))
	
	sbx <- BORG_Operator_create("SBX", 2, 2, 2, BORG_Operator_SBX_ptr)
	BORG_Operator_set_parameter(sbx, 0, lookup(parameters, "sbx.rate", 1.0))
	BORG_Operator_set_parameter(sbx, 1, lookup(parameters, "sbx.distributionIndex", 15.0))
	BORG_Operator_set_mutation(sbx, pm)
	
	de <- BORG_Operator_create("DE", 4, 1, 2, BORG_Operator_DE_ptr)
	BORG_Operator_set_parameter(de, 0, lookup(parameters, "de.crossoverRate", 0.1))
	BORG_Operator_set_parameter(de, 1, lookup(parameters, "de.stepSize", 0.5))
	BORG_Operator_set_mutation(de, pm)
	
	um <- BORG_Operator_create("UM", 1, 1, 1, BORG_Operator_UM_ptr)
	BORG_Operator_set_parameter(um, 0, lookup(parameters, "um.rate", 1.0 / nvars))
	
	spx <- BORG_Operator_create("SPX", lookup(parameters, "spx.parents", 10),
								lookup(parameters, "spx.offspring", 2), 1, BORG_Operator_SPX_ptr)
	BORG_Operator_set_parameter(spx, 0, lookup(parameters, "spx.epsilon", 3.0))
	
	pcx <- BORG_Operator_create("PCX", lookup(parameters, "pcx.parents", 10),
								lookup(parameters, "pcx.offspring", 2), 2, BORG_Operator_PCX_ptr)
	BORG_Operator_set_parameter(pcx, 0, lookup(parameters, "pcx.eta", 0.1))
	BORG_Operator_set_parameter(pcx, 1, lookup(parameters, "pcx.zeta", 0.1))
	
	undx <- BORG_Operator_create("UNDX", lookup(parameters, "undx.parents", 10),
								 lookup(parameters, "undx.offspring", 2), 2, BORG_Operator_UNDX_ptr)
	BORG_Operator_set_parameter(undx, 0, lookup(parameters, "undx.zeta", 0.5))
	BORG_Operator_set_parameter(undx, 1, lookup(parameters, "undx.eta", 0.35))
	
	algorithm <- BORG_Algorithm_create(problem, 6)
	BORG_Algorithm_set_operator(algorithm, 0, sbx)
	BORG_Algorithm_set_operator(algorithm, 1, de)
	BORG_Algorithm_set_operator(algorithm, 2, pcx)
	BORG_Algorithm_set_operator(algorithm, 3, spx)
	BORG_Algorithm_set_operator(algorithm, 4, undx)
	BORG_Algorithm_set_operator(algorithm, 5, um)
	
	BORG_Algorithm_set_initial_population_size(algorithm, lookup(parameters, "initialPopulationSize", 100))
	BORG_Algorithm_set_minimum_population_size(algorithm, lookup(parameters, "minimumPopulationSize", 100))
	BORG_Algorithm_set_maximum_population_size(algorithm, lookup(parameters, "maximumPopulationSize", 10000))
	BORG_Algorithm_set_population_ratio(algorithm, 1.0 / lookup(parameters, "injectionRate", 0.25))
	BORG_Algorithm_set_selection_ratio(algorithm, lookup(parameters, "selectionRatio", 0.02))
	BORG_Algorithm_set_restart_mode(algorithm, lookup(parameters, "restartMode", RESTART_DEFAULT))
	BORG_Algorithm_set_max_mutation_index(algorithm, lookup(parameters, "maxMutationIndex", 10))
	BORG_Algorithm_set_probability_mode(algorithm, lookup(parameters, "probabilityMode", PROBABILITIES_DEFAULT))
	
	# run the Borg MOEA
	currentEvaluations <- 0
	
	while (BORG_Algorithm_get_nfe(algorithm) < maxEvaluations) {
		BORG_Algorithm_step(algorithm)
	}
	
	result <- BORG_Algorithm_get_result(algorithm)
	size <- BORG_Archive_get_size(result)
	
	# check if result contains feasible solutions
	if (size > 0) {
		if (BORG_Solution_violates_constraints(BORG_Archive_get(result, 0))) {
			size = 0
		}
	}
	
	# generate results
	variables <- matrix(0, size, nvars)
	objectives <- matrix(0, size, nobjs)
	
	for (i in 1:size) {
		solution <- BORG_Archive_get(result, i-1)
		
		for (j in 1:nvars) {
			variables[i,j] = BORG_Solution_get_variable(solution, j-1)
		}
		
		for (j in 1:nobjs) {
			objectives[i,j] = BORG_Solution_get_objective(solution, j-1)
		}
	}
	
	# free resources
	BORG_Operator_destroy(sbx)
	BORG_Operator_destroy(de)
	BORG_Operator_destroy(pm)
	BORG_Operator_destroy(um)
	BORG_Operator_destroy(spx)
	BORG_Operator_destroy(pcx)
	BORG_Operator_destroy(undx)
	BORG_Algorithm_destroy(algorithm)
	BORG_Archive_destroy(result)
	BORG_Problem_destroy(problem)
	
	# return results as an R data frame
	data.frame(var=variables, obj=objectives)
}
