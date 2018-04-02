class CharTests extends ParsleyTest
{
    "string" should "consume succeed if it is found at head" in pending
    it should "not consume input if it fails on first character" in pending
    it should "consume input if it fails mid-string" in pending
    it should "not consume input if it fails mid-string when combined with attempt" in pending

    "anyChar" should "accept any character" in pending
    it should "fail if the input has run out, expecting any character" in pending

    "space" should "consume ' ' or '\t'" in pending
    it should "expect space/tab otherwise" in pending

    "spaces" should "consume lots of spaces" in pending
    it should "never fail" in pending

    "whitespace" should "consume any whitespace chars" in pending
    it should "fail otherwise" in pending

    "endOfLine" should "consume windows or unix line endings" in pending
    it should "fail otherwise" in pending

    "upper" should "only accept uppercase characters" in pending
    it should "fail otherwise" in pending

    "lower" should "only accept lowercase characters" in pending
    it should "fail otherwise" in pending

    "digit parsers" should "accept the appropriate characters" in pending
    they should "fail otherwise" in pending
}
