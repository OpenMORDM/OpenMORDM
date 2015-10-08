library(RUnit)

test.suite <- defineTestSuite("OpenMORDM Tests",
							  dirs = file.path("tests"),
							  testFileRegexp = "^test_.*\\.R$")

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)