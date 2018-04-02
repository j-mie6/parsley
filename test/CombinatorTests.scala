class CombinatorTests extends ParsleyTest
{
    "choice" should "fail if given the empty list" in pending
    it should "behave like p for List(p)" in pending
    it should "parse in order" in pending
    it should "fail if none of the parsers succeed" in pending

    "repeat" should "be pure(Nil) for n <= 0" in pending
    it should "parse n times for n > 0" in pending

    "option" should "succeed with Some if p succeeds" in pending
    it should "succeed with None if p fails without consumption" in pending
    it should "fail if p fails with consumption" in pending

    "decide" must "succeed for Some" in pending
    it must "fail for None" in pending
    it must "compose with option to become identity" in pending

    "optional" must "succeed if p succeeds" in pending
    it must "also succeed if p fails without consumption" in pending
    it must "fail if p failed with consumption" in pending

    "manyN" must "ensure that n are parsed" in pending
    it should "not care if more are present" in pending

    "skipManyN" must "ensure that n are parsed" in pending
    it should "not care if more are present" in pending

    "sepBy" must "accept empty input" in pending
    it should "not require sep at the end of chain" in pending
    it should "be able to parse 2 or more p" in pending

    "sepBy1" must "require a p" in pending

    "sepEndBy" must "accept empty input" in pending
    it should "not require sep at the end of chain" in pending
    it should "be able to parse 2 or more p" in pending
    it should "be able to parse a final sep" in pending

    "sepEndBy1" must "require a p" in pending

    "endBy" must "accept empty input" in pending
    it must "require sep at end of chain" in pending
    it should "be able to parse 2 or more p" in pending

    "endBy1" must "require a p" in pending

    "eof" must "succeed at the end of input" in pending
    it must "fail if input remains" in pending

    "notFollowedBy" must "succeed if p fails" in pending
    it must "succeed even if p consumed input" in pending
    it must "fail if p succeeds" in pending

    "manyTill" must "require an end" in pending
    it should "parse the end without result" in pending
    it should "parse p until that end is found" in pending

    "many1Till" must "parse at least 1 p" in pending
}
