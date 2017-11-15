module HTMLGenSpec (spec) where

import           Test.Hspec

import           HTMLGen
import           Types

spec :: Spec
spec =
    describe "The code generator" $ do
        it "generates HTML for the empty AST" $
            generateHTML' (Sequence []) `shouldBe` ""

        it "generates HTML for a text" $
            generateHTML' (Text "der Text") `shouldBe` "der Text"

        it "generates HTML for a paragraph consisting of one element" $
            generateHTML' (P [Text "der Text"])
                `shouldBe` "<p>der Text</p>\n"

        it "generates HTML for a paragraph consisting of three elements" $
            generateHTML' (P [Text "der", Text " ", Text "Text"])
                `shouldBe` "<p>der Text</p>\n"

        it "generates HTML for a header" $
            generateHTML' (H 1 $ Sequence [Text "der", Text " "
                                         , Text "Header"])
                `shouldBe` "<h1>der Header</h1>\n"

        it "generate Code for a sequence" $
            generateHTML' (Sequence [ H 1 (Sequence [Text "der"
                                                    , Text " "
                                                    , Text "Header"
                                                    ]
                                          )
                                    , P [Text "Hallo"]
                                    , P [Text "Welt!"]
                                    ])
                `shouldBe`
                    "<h1>der Header</h1>\n<p>Hallo</p>\n<p>Welt!</p>\n"

        it "generate HTML for emphasis" $
            generateHTML' (Sequence [P [Emphasis (Sequence [Text "Hallo"])]])
                `shouldBe` "<p><em>Hallo</em>\n</p>\n"



        it "generate HTML-Text with blank after one star" $
            generateHTML' (Sequence [P [Text "*",Text " ",Text "Hallo",Text "*"]])
                `shouldBe` "<p>* Hallo*</p>\n"

        it "generate HTML for example 328 (Emphasis)" $
            generateHTML' (Sequence [P [Emphasis (Sequence [Text "foo",Text " ",Text "bar"])]])
                `shouldBe` "<p><em>foo bar</em>\n</p>\n"

        it "generate HTML for example 329 (Emphasis)" $
            generateHTML' (Sequence [P [Text "a",Text " ",Text "*",Text " ",Text "foo",Text " ",Text "bar",Text "*"]])
                `shouldBe` "<p>a * foo bar*</p>\n"

        it "generate HTML for example 332 (Emphasis)" $
            generateHTML'  (Sequence [P [Text "foo",Emphasis (Sequence [Text "bar"])]])
                `shouldBe` "<p>foo<em>bar</em>\n</p>\n"

        it "generate HTML for exmaple 333 (Emphasis)" $
            generateHTML' (Sequence [P [Text "5",Emphasis (Sequence [Text "6"]),Text "78"]])
                `shouldBe` "<p>5<em>6</em>\n78</p>\n"

        it "generate HTML for example 355 (Strong)" $
            generateHTML' (Sequence [P [Strong (Sequence [Text "foo",Text " ",Text "bar"])]])
                `shouldBe` "<p><strong>foo bar</strong>\n</p>\n"

        it "generate HTML for example 356 (Strong)" $
            generateHTML' (Sequence [P [Text "**",Text " ",Text "foo",Text " ",Text "bar",Text "**"]])
                `shouldBe` "<p>** foo bar**</p>\n"

        it "generate HTML for example 358 (Strong)" $
            generateHTML' (Sequence [P [Text "foo",Strong (Sequence [Text "bar"])]])
                `shouldBe` "<p>foo<strong>bar</strong>\n</p>\n"

        it "generate HTML for example 370 (Emphasis & Strong)" $
            generateHTML' (Sequence [P [Emphasis (Sequence [Text "(",Strong (Sequence [Text "foo"]),Text ")"])]])
                `shouldBe` "<p><em>(<strong>foo</strong>\n)</em>\n</p>\n"

        it "generate HTML for example 371 (Emphasis & Strong)" $
            generateHTML' (Sequence [P [Strong (Sequence [Text "Gomphocarpus",Text " ",Text "(",Emphasis (Sequence [Text "Gomphocarpus",Text " ",Text "physocarpus"]),Text ",",Text " ",Text "syn.",Text "\n",Emphasis (Sequence [Text "Asclepias",Text " ",Text "physocarpa"]),Text ")"])]])
                `shouldBe` "<p><strong>Gomphocarpus (<em>Gomphocarpus physocarpus</em>\n, syn.\n<em>Asclepias physocarpa</em>\n)</strong>\n</p>\n"

        it "generate HTML for example 373 (Emphasis & Strong)" $
            generateHTML' (Sequence [P [Strong (Sequence [Text "foo"]),Text "bar"]])
                `shouldBe` "<p><strong>foo</strong>\nbar</p>\n"

        it "generate HTML for example 180 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "aaa"],Emptyline,P [Text "bbb"]])
                `shouldBe` "<p>aaa</p>\n<p>bbb</p>\n"

        it "generate HTML for example 181 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "aaa",Text "\n",Text "bbb"],Emptyline,P [Text "ccc",Text "\n",Text "ddd"]])
                `shouldBe` "<p>aaa\nbbb</p>\n<p>ccc\nddd</p>\n"

        it "generate HTML for example 182 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "aaa"],Emptyline,P [Text "\n",Text "bbb"]])
                `shouldBe` "<p>aaa</p>\n<p>\nbbb</p>\n"

        it "generate HTML for example 183 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb"]])
                `shouldBe` "<p>\naaa\nbbb</p>\n"

        it "generate HTML for example 184 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb",Text "\n",Text "ccc"]])
                `shouldBe` "<p>\naaa\nbbb\nccc</p>\n"

        it "generate HTML for example 185 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb"]])
                `shouldBe` "<p>\naaa\nbbb</p>\n"

        -- it "generate HTML for example 186 (Paragraphs)" $
        --    generateHTML' (Sequence [P [Text "\n",Text "aaa",Text "\n",Text "bbb"]])
        --        `shouldBe` "<pre><code>aaa</code></pre><p>bbb</p>\n"

        it "generate HTML for example 187 (Paragraphs)" $
            generateHTML' (Sequence [P [Text "\n",Text "aaa",Text "<br />\n",Text "bbb",Text "<br />\n"]])
                `shouldBe`"<p>\naaa<br />\nbbb<br />\n</p>\n"
