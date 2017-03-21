load("@io_bazel_rules_scala//scala:scala.bzl", "scala_library", "scala_binary", "scala_test")

scala_test(
	name = "testDFA",
	size = "small",
	srcs = [
		"DFATest.scala",
		"TestRegexes.scala",
	],
	deps = [
		"//src/main/scala/lucassonnabend/finiteautomata:finiteAutomata",
	],
)

scala_test(
	name = "testNFA",
	size = "small",
	srcs = [
		"NFATest.scala",
		"TestRegexes.scala",
	],
	deps = [
		"//src/main/scala/lucassonnabend/finiteautomata:finiteAutomata",
	],
)
