library(qcTAF)

tests <- qc("../src/simple")

cat(capture.output(t(t(tests))), sep = "\n", file = "simple.txt")
