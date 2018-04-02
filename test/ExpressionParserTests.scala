class ExpressionParserTests extends ParsleyTest
{
    "chainPost" must "require an initial value" in pending
    it must "parse all operators that follow" in pending
    it must "apply the functions" in pending
    it must "fail if an operator fails after consuming input" in pending

    "chainPre" must "parse an operatorless value" in pending
    it must "parse all operators that precede a value" in pending
    it must "fail if the final value is absent" in pending
    it must "apply the functions" in pending

    "chainr1" must "require an initial value" in pending
    it must "parse all operators and values that follow" in pending
    it must "apply the functions with correct associativity" in pending
    it must "fail if an operator or p fails after consuming input" in pending

    "chainl1" must "require an initial value" in pending
    it must "parse all operators and values that follow" in pending
    it must "apply the functions with correct associativity" in pending
    it must "fail if an operator fails after consuming input" in pending

    "expression parsers" should "result in correct precedence" in pending
    they should "work for multiple operators at the same level" in pending
    they should "work for mixed associativity operators at same level" in pending
    they should "parse mathematical expressions" in pending
}
