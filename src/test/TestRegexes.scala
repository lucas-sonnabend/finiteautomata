package test

/**
  * This singleton contains test regexes.
  */
object TestRegexes {

  val TEST_REGEX_A_OR_B = "a|b"
  val TEST_INPUTS_A_OR_B = Map("" -> false, "a" -> true, "b" -> true, "c" -> false, "ab" -> false)

  val TEST_REGEX_A_OR_BS_AND_C = "(a|b*)c"
  val TEST_INPUTS_A_OR_BS_AND_C = Map("" -> false, "a" -> false, "ac" -> true, "bbbc" -> true,
    "abac" -> false, "ab" -> false)

  val TEST_REGEX_AA_OR_AA = "aa|aa"
  val TEST_INPUTS_AA_OR_AA = Map("" -> false, "a" -> false, "aa" -> true, "aaa" -> false)

  val TEST_REGEX_AB_OR_AA = "ab|aa"
  val TEST_INPUT_AB_OR_AA = Map("" -> false, "ab" -> true, "aa" -> true, "aba" -> false, "bb" -> false)

  val TEST_REGEX_A_OR_B_AND_C_STAR = "((a|b)c)*"
  val TEST_INPUTS_A_OR_B_AND_C_STAR = Map("ac" -> true, "acacac" -> true, "acbcac" -> true, "bc" -> true, "bcac" -> true,
    "bac" -> false
  )

  val TEST_INPUTS = Map(
    TEST_REGEX_A_OR_B -> TEST_INPUTS_A_OR_B,
    TEST_REGEX_A_OR_BS_AND_C -> TEST_INPUTS_A_OR_BS_AND_C,
    TEST_REGEX_AA_OR_AA -> TEST_INPUTS_AA_OR_AA
  )
}
