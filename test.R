source("borg.R")
source("DTLZ2.R")

output <- borg(nvars, nobjs, 0, DTLZ2, 10000)

print(output)
