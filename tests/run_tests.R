library(RUnit)
library(devtools)

print(installed.packages()[,"Package"])

require(visualTest)

#if (!"visualTest" %in% installed.packages()[,"Package"]) {
#	devtools::install_github("MangoTheCat/visualTest")
#}

test.suite <- defineTestSuite("OpenMORDM Tests",
							  dir = file.path("."),
							  testFileRegexp = "^test_.*\\.R$")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)