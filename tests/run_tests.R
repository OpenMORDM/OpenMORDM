library(RUnit)

test.suite <- defineTestSuite("OpenMORDM Tests",
							  dirs = file.path("."),
							  testFileRegexp = "^test_.*\\.R$")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)