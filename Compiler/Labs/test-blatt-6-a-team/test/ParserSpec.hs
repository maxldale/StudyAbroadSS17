module ParserSpec (spec) where

import           Test.Hspec

import           Data.Maybe (fromJust)
import           Parser
import           Types

spec :: Spec
spec =
    describe "The Parser" $ do

        it "parses the empty stream" $
            fromJust ( parse []) `shouldBe` Sequence []

        it "parses a newline" $
            fromJust ( parse [TokenNewline])
                `shouldBe` Sequence [P [Text "\n"]]

        it "parses 2 Newlines" $
            fromJust ( parse [TokenNewline, TokenNewline])
                `shouldBe` Sequence [Emptyline]

        it "parses 2 Texts" $
            fromJust ( parse [TokenText "Hallo", TokenNewline])
                `shouldBe` Sequence [P [Text "Hallo", Text "\n"]]

        it "does not parse a header without a following blank" $
            fromJust ( parse [TokenH 6, TokenText "Hallo"])
                `shouldBe` Sequence [P [Text "######", Text "Hallo"]]

        it "parses a header" $
            fromJust ( parse [TokenH 6, TokenBlanks 1, TokenText "Hallo"
                              , TokenBlanks 1, TokenText "Welt!", TokenNewline])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"
                                                    ]
                                          )
                                    , P [Text "\n" ]
                                    ]

        it "parses a header without a newline" $
            fromJust ( parse [TokenH 6, TokenBlanks 1, TokenText "Hallo"
                              , TokenBlanks 1, TokenText "Welt!"])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"
                                                    ]
                                          )
                                    ]

        it "parses a header with two newlines" $
            fromJust ( parse [TokenH 6, TokenBlanks 1, TokenText "Hallo"
                              , TokenBlanks 1, TokenText "Welt!"
                              ,TokenNewline, TokenNewline])
                `shouldBe` Sequence [ H 6 (Sequence [Text "Hallo"
                                                    , Text " "
                                                    , Text "Welt!"]
                                          )
                                    , Emptyline
                                    ]

        it "parses two Paragraphs" $
            fromJust ( parse [ TokenText "Hallo", TokenNewline, TokenText "Welt"
                              , TokenBlanks 3, TokenNewline, TokenNewline
                              , TokenText "Huhu", TokenBlanks 1, TokenText "World"
                              , TokenNewline])
                `shouldBe` Sequence [P [Text "Hallo",Text "\n",Text "Welt",Text "<br />\n",
                Text "\n", Text "Huhu",Text " ",Text "World",Text "\n"]]

        it "parse emphasis with one star" $
            fromJust (parse [TokenStars 1, TokenText "Hallo", TokenStars 1])
                `shouldBe` Sequence [P [Emphasis (Sequence [Text "Hallo"])]]

        it "parse text with blank after one star" $
            fromJust (parse [TokenStars 1,TokenBlanks 1,TokenText "Hallo",TokenStars 1])
                `shouldBe` Sequence [P [Text "*",Text " ",Text "Hallo",Text "*"]]

        it "parse example 328 (Emphasis)" $
            fromJust (parse [TokenStars 1,TokenText "foo",TokenBlanks 1,TokenText "bar",TokenStars 1])
                `shouldBe` Sequence [P [Emphasis (Sequence [Text "foo",Text " ",Text "bar"])]]

        it "parse example 329 (Emphasis)" $
            fromJust (parse [TokenText "a",TokenBlanks 1,TokenStars 1,TokenBlanks 1,TokenText "foo",TokenBlanks 1,TokenText "bar",TokenStars 1])
                `shouldBe` Sequence [P [Text "a",Text " ",Text "*",Text " ",Text "foo",Text " ",Text "bar",Text "*"]]

        it "parse example 332 (Emphasis)" $
            fromJust (parse [TokenText "foo",TokenStars 1,TokenText "bar",TokenStars 1])
                `shouldBe` Sequence [P [Text "foo",Emphasis (Sequence [Text "bar"])]]

        it "parse example 333 (Emphasis)" $
            fromJust (parse [TokenText "5",TokenStars 1,TokenText "6",TokenStars 1,TokenText "78"])
                `shouldBe` Sequence [P [Text "5",Emphasis (Sequence [Text "6"]),Text "78"]]

        it "parse example 355 (Strong)" $
            fromJust (parse [TokenStars 2,TokenText "foo",TokenBlanks 1,TokenText "bar",TokenStars 2])
                `shouldBe` Sequence [P [Strong (Sequence [Text "foo",Text " ",Text "bar"])]]

        it "parse example 356 (Strong)" $
            fromJust (parse [TokenStars 2,TokenBlanks 1,TokenText "foo",TokenBlanks 1,TokenText "bar",TokenStars 2])
                `shouldBe` Sequence [P [Text "**",Text " ",Text "foo",Text " ",Text "bar",Text "**"]]

        it "parse example 358 (Strong)" $
            fromJust (parse [TokenText "foo",TokenStars 2,TokenText "bar",TokenStars 2])
                `shouldBe` Sequence [P [Text "foo",Strong (Sequence [Text "bar"])]]

        it "parse example 370 (Emphasis & Strong)" $
            fromJust (parse [TokenStars 1,TokenText "(",TokenStars 2,TokenText "foo",TokenStars 2,TokenText ")",TokenStars 1])
                `shouldBe` Sequence [P [Emphasis (Sequence [Text "(",Strong (Sequence [Text "foo"]),Text ")"])]]

        it "parse example 371 (Emphasis & Strong" $
            fromJust (parse [TokenStars 2,TokenText "Gomphocarpus",TokenBlanks 1,TokenText "(",TokenStars 1,TokenText "Gomphocarpus",TokenBlanks 1,TokenText "physocarpus",TokenStars 1,TokenText ",",TokenBlanks 1,TokenText "syn.",TokenNewline,TokenStars 1,TokenText "Asclepias",TokenBlanks 1,TokenText "physocarpa",TokenStars 1,TokenText ")",TokenStars 2])
                `shouldBe` Sequence [P [Strong (Sequence [Text "Gomphocarpus",Text " ",Text "(",Emphasis (Sequence [Text "Gomphocarpus",Text " ",Text "physocarpus"]),Text ",",Text " ",Text "syn.",Text "\n",Emphasis (Sequence [Text "Asclepias",Text " ",Text "physocarpa"]),Text ")"])]]

        it "parse example 373 (Emphasis & Strong)" $
            fromJust (parse [TokenStars 2,TokenText "foo",TokenStars 2,TokenText "bar"])
                `shouldBe` Sequence [P [Strong (Sequence [Text "foo"]),Text "bar"]]

        it "parse example 180 (paragraph)" $
            fromJust (parse [TokenNewline, TokenText "aaa", TokenNewline, TokenNewline, TokenText "bbb", TokenNewline])
                `shouldBe` Sequence [P [Text "\n",Text "aaa"],Emptyline,P [Text "bbb",Text "\n"]]

        it "parse example 181 (paragraph)" $
              fromJust (parse [TokenNewline,TokenText "aaa",TokenNewline,TokenText "bbb",TokenNewline,TokenNewline,TokenText "ccc",TokenNewline,TokenText "ddd",TokenNewline])
                `shouldBe` Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb"],Emptyline,P [Text "ccc",Text "\n",Text "ddd",Text "\n"]]

        it "parse example 182 (paragraph)" $
            fromJust (parse [TokenNewline, TokenText "aaa", TokenNewline, TokenNewline, TokenText "bbb", TokenNewline])
                `shouldBe` Sequence [P [Text "\n",Text "aaa"],Emptyline,P [Text "bbb",Text "\n"]]

        it "parse example 183 (paragraph)" $
            fromJust (parse [TokenNewline, TokenText "aaa", TokenNewline, TokenText "bbb", TokenNewline])
                `shouldBe` Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb",Text "\n"]]

        it "parse example 184 (paragraph)" $
            fromJust (parse [TokenNewline, TokenText "aaa", TokenNewline, TokenText "bbb", TokenNewline, TokenText "ccc", TokenNewline])
                `shouldBe` Sequence [P [Text "\n", Text "aaa",Text "\n",Text "bbb",Text "\n",Text "ccc",Text "\n"]]

        it "parse example 185 (paragraph)" $
            fromJust (parse [TokenNewline, TokenText "aaa", TokenNewline, TokenText "bbb", TokenNewline])
                `shouldBe` Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb",Text "\n"]]

        --it "parse example 186 (paragraph)" $

        it "parse example 187 (paragraph)" $
            fromJust (parse [TokenNewline, TokenText "aaa", TokenBlanks 5, TokenNewline, TokenText "bbb", TokenBlanks 5, TokenNewline])
                `shouldBe` Sequence [P [Text "\n",Text "aaa",Text "<br />\n",Text "bbb",Text "<br />\n"]]
