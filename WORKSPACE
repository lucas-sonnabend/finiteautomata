workspace(name = "FiniteAutomata")

bind (
	name = "main",
	actual = "//src.main",
)

git_repository(
	name = "io_bazel_rules_scala",
	remote = "git://github.com/bazelbuild/rules_scala",
	commit = "690cf39eba8eccf3c6e21ca77d8a4e1710aa3629"
)

load("@io_bazel_rules_scala//scala:scala.bzl", "scala_repositories")
scala_repositories()
