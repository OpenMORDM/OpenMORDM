library(RUnit)
library(devtools)

if (!"visualTest" %in% installed.packages()[,"Package"]) {
	devtools::install_github("MangoTheCat/visualTest")
}

library(visualTest)

test.suite <- defineTestSuite("OpenMORDM Tests",
							  dir = file.path("."),
							  testFileRegexp = "^test_.*\\.R$")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)