module DotGen
  ( generateDot
  ) where

import           Types

generateDot :: AST -> String
generateDot ast =
     "digraph ast {\n"
  ++ snd (generateDot' "" ast)
  ++ "\n}"

generateDot' :: String -> AST -> (String, String)
generateDot' suffix Emptyline  =
  let topnode = "text" ++ suffix
  in ( topnode
     , topnode ++ " [label=emptyline];\n"
     )
generateDot' suffix (Text str) =
  let topnode = "text" ++ suffix
  in ( topnode
     , topnode ++ " [label=" ++ show str ++ "];\n"
     )
generateDot' suffix (H i ast) =
  let topnode = "h" ++ suffix
      (childnode, childstr) = generateDot' (suffix++"h") ast
  in  ( topnode
      , topnode ++ " [label=\"H" ++ show i ++ "\"];\n"
        ++ topnode ++ " -> " ++ childnode ++ ";\n"
        ++ childstr
      )
generateDot' suffix (P asts) =
  let topnode = "p" ++ suffix
      childDots = map (\(i,ast) -> generateDot' (suffix++"p"++show i) ast)
                      $ zip [1..] asts
  in  ( topnode
      , topnode ++ " [label=\"P\"];\n"
        ++ concatMap (\(childnode,childstr) ->
                      topnode ++ " -> " ++ childnode ++ ";\n"
                      ++ childstr) childDots
      )
generateDot' suffix (Sequence asts) =
  let topnode = "s" ++ suffix
      childDots =  map (\(i,ast) -> generateDot' (suffix++"s"++show i) ast)
                      $ zip [1..] asts
  in  ( topnode
      , topnode ++ " [label=\"Sequence\"];\n"
        ++ concatMap (\(childnode,childstr) ->
                      topnode ++ " -> " ++ childnode ++ ";\n"
                      ++ childstr) childDots
      )
