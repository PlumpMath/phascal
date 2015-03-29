import Ast
import Parser
import Text.ParserCombinators.Parsec (ParseError)

tests = [ ("ex1",
          [ Program "TheSource" [] []
              [ Assign "a" (Var "b3")
              , Assign "xyz" (Op Plus (Var "a")
                                 (Op Plus (Op Mod (Var "b") (Var "c"))
                                          (Op Minus (Var "c") (Op Div (Var "p")
                                                                      (Var "q")))))
              , Assign "a" (Op Times (Var "xyz") (Op Plus (Var "p") (Var "q")))
              , Assign "p" (Op Minus (Var "a") (Op Minus (Var "xyz") (Var "p")))
              ]
          ])
        , ("ex2",
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
        , ("ex3",
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
        , ("ex4",
          [ Program "signsAndBools" [] []
            [ If (Not T)
                 (Assign "x" (Op Plus (Num 4) (Neg (Num 2))))
                 (Just (Assign "y" F))
            ]
          ])
        ]

type TestFailure = Either ParseError ([Program], [Program])
type TestResult = Either TestFailure [Program]

runTest :: String ->  String -> [Program] -> TestResult
runTest name input ast = case parse name input of
    Left e -> Left (Left e)
    Right ast' ->
        if ast' == ast
        then Right ast'
        else Left (Right (ast, ast))

runTests :: IO [TestFailure]
runTests = do
    let filenames = ["ex/" ++ name ++ ".pscl" | name <- map fst tests]
    files <- mapM readFile filenames
    let testResults = zipWith3 runTest filenames files (map snd tests)
    (return :: a -> IO a) $ map (\(Left e) -> e) $ flip filter testResults (\res ->
        case res of
            Left _ -> True
            Right _ -> False)

main :: IO ()
main = do
    failures <- runTests
    case failures of
        [] -> interact (show . parse "<stdin>")
        _ -> do
            putStrLn "Test Failures:"
            print failures
