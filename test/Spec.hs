import Control.Exception (evaluate)
import Parser (parseRule)
import Syntax
import Test.Hspec

main :: IO ()
main = hspec $ do
  parserSpec

parserSpec =
  describe "Parser.parseRule" $ do
    it "parses a rule" $ do
      parseRule "foo(a, bar(B, _), 1) :- baz(2, c), buzz(D, D, D)." `shouldBe` Right (Rule foo [baz, buzz])
  where
    foo = Predicate "foo" [TermLiteral $ LiteralAtom "a", TermPredicate bar, TermLiteral $ LiteralInt 1]
    bar = Predicate "bar" [TermVariable $ Variable "B", TermVariable $ Variable "_"]
    baz = Predicate "baz" [(TermLiteral $ LiteralInt 2), (TermLiteral $ LiteralAtom "c")]
    buzz = Predicate "buzz" [(TermVariable $ Variable "D"), (TermVariable $ Variable "D"), (TermVariable $ Variable "D")]