source("OpenMORDM.R")

data <- mordm.read("lakeoutput.txt", 20, 5, 1,
				   bounds=matrix(rep(range(0.0, 0.1), 20), nrow=2),
				   maximize=c("Obj2", "Obj3", "Obj4", "Obj5"))

# Animate the data, saving to a GIF
#mordm.animate(data)

# Plot Operators
#mordm.plotops(data, time=FALSE, improvements=TRUE)

# Marking Demo
mark1 <- mordm.mark.rule(function(x) x[21] < 0.1)
#mark2 <- mordm.mark.rule(function(x) x[21] > 0.125)
#mark3 <- mordm.mark.not(mordm.mark.union(mark1, mark2))
#mordm.plot(data, mark=list(mark1, mark2, mark3))

# Select Solutions to Mark
#mark4 <- mordm.mark.selection()

# Prim Analysis - Bump Hunting
#boxes <- mordm.prim(data, mark1)
#mordm.plotprim(data, boxes)

# Identify Differences in Datasets
#boxes.union <- mordm.mark.union(boxes)
#prim <- mordm.select(data, boxes.union)
#non.prim <- mordm.select(data, mordm.mark.not(boxes.union))
#mordm.differences(prim, non.prim, decreasing=TRUE)

#mordm.recommend(data)

# Parallel Plot
#mordm.plotpar()
#mordm.identify()

# Box (Candlestick) Plot
#mordm.plotbox()
#mordm.identify()

# Find Scenarios with High Utility
#mark.high <- mordm.mark.rule(function(x) x["Obj2"] > 0.25)
#boxes.high <- mordm.prim(data, mark.high, paste.alpha=1)
#boxes.low <- mordm.prim(data, mordm.mark.not(mark.high))
#mordm.plotbox(data, boxes.high[[1]])
#mordm.printbox(data, boxes.high[[1]])

#mordm.plotprim(data, list(boxes.high, boxes.low), names=c("High Bentham Utility", "Low Bentham Utility"))

#mordm.sensitivity(data, function(x) x["Obj2"], all=TRUE)


# Non-dominated ranking
#set <- mordm.join(data, index=1:length(data))
#print(nrow(set))
#ranks <- mordm.pareto.rank(set)
#mordm.plot(set, color=-ranks)


# Compute robustness at each point and color the plot
#lake.problem <- setup("lake5obj.exe", 20, 5, 1,
#					  bounds=matrix(rep(range(0, 0.1), 20), nrow=2))
#r <- mordm.robustness(data, 0.01, 100, lake.problem, method=c("default", "variance", "constraints", "infogap"))
#mordm.plot(data, color=r[,"default"])



# Sensitivity analysis
lake.problem <- setup("lake5obj.exe", 20, 5, 1,
					  bounds=matrix(rep(range(0, 0.1), 20), nrow=2))
r.fun <- function(x) mordm.robustness(x, 0.01, 100, lake.problem)
sen <- sensitivity(lake.problem, r.fun, 1000, method="plischke")
print(sen)