module ParserTests (hunitTests) where

import Test.HUnit
import Parser
import Ast

tests = [ ("TheSource",
           unlines [
            "program TheSource ;",
            "begin",
            "   a := b3;",
            "   xyz := a + b mod c + c - p/q;",
            "   a := xyz * (p + q);",
            "   p := a - xyz - p",
            "end."
          ],
          [ Program "TheSource" [] []
              [ Assign "a" (Var "b3")
              , Assign "xyz" (Op Minus (Op Plus (Op Plus (Var "a")
                                                         (Op Mod (Var "b")
                                                                 (Var "c")))
                                                (Var "c"))
                                       (Op Div (Var "p")
                                               (Var "q")))
              , Assign "a" (Op Times (Var "xyz") (Op Plus (Var "p") (Var "q")))
              , Assign "p" (Op Minus (Op Minus (Var "a") (Var "xyz")) (Var "p"))
              ]
          ])
        , ("decisions",
           unlines [
            "program decisions (input, output);",
            "var i, j : integer;",
            "    variable : boolean;",
            "begin",
            "   if i > j then",
            "    i := i + j",
            "   else if variable then",
            "    if i <= j then",
            "     i := 0",
            "    else",
            "     j := 0",
            "end."
          ],
          [ Program "decisions" ["input", "output"] [ (["i", "j"], TyInt)
                                                    , (["variable"], TyBool)
                                                    ]
            [ If (Op Gt (Var "i") (Var "j"))
                 (Assign "i" (Op Plus (Var "i") (Var "j")))
                 (Just $ If (Var "variable")
                         (If (Op LtEq (Var "i") (Var "j"))
                             (Assign "i" (Num 0))
                             (Just $ Assign "j" (Num 0)))
                         Nothing)
            ]
          ])
        , ("HasLoops",
           unlines [
            "program HasLoops (input, output);",
            "var i, j : integer;",
            "    NotDone : boolean ;",
            "begin",
            "   while (i < j) and NotDone do begin",
            "    k := k  + 1;",
            "    while  i = j  do  i := i + 2",
            "   end",
            "end."
          ],
          [ Program "HasLoops" ["input", "output"] [ (["i", "j"], TyInt)
                                                   , (["NotDone"], TyBool)
                                                   ]
            [ While (Op And (Op Lt (Var "i") (Var "j")) (Var "NotDone"))
               (CompoundStatement [ Assign "k" (Op Plus (Var "k") (Num 1))
                                  , While (Op Eq (Var "i") (Var "j"))
                                          (Assign "i" (Op Plus (Var "i") (Num 2)))
                                  ])
            ]
          ])
        , ("signsAndBools",
           unlines [
            "program signsAndBools; begin",
            "  if not true then",
            "    x := 4 + -2",
            "  else",
            "    y := false",
            "end."
          ],
          [ Program "signsAndBools" [] []
            [ If (Not T)
                 (Assign "x" (Op Plus (Num 4) (Neg (Num 2))))
                 (Just (Assign "y" F))
            ]
          ])
        ]

assertAst :: String -> [Program] -> Assertion
assertAst text ast = case (parse "test-input" text) of
    Left e -> putStrLn ("parse error: " ++ (show e))
    Right ast' -> assertEqual "Ast was not as expected" ast' ast

hunitTests = TestList $ map (\(name,text,ast) ->
    TestLabel name (TestCase (assertAst text ast))) tests
