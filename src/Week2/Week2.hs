module Week2 where

import Log

-- Week 2

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

data Result = Err
            | Ok String

-- Homework 2

split'' :: String -> (Char -> Bool) -> [String] -> [String]
split'' cs delim [] = split'' cs delim [""]
split'' (c:cs) delim (o:os)
  | delim c = split'' cs delim ("":o:os)
  | otherwise = split'' cs delim ((o ++ [c]):os)
split'' [] delim os = os

split' :: String -> (Char -> Bool) -> [String]
split' cs delim = reverse $ split'' cs delim []

split :: String -> [String]
split cs = split' cs (== ' ')

join :: [String] -> String
join [arr] = arr
join (x:y:rest) = join ((x ++ (' ':y)):rest)

parseMessage :: String -> LogMessage
parseMessage input = case split input of
                       "I":t:s -> LogMessage Info (read t) $ join s
                       "W":t:s -> LogMessage Warning (read t) $ join s
                       "E":p:t:s -> LogMessage (Error (read p)) (read t) $ join s
                       s -> Unknown $ join s

parse :: String -> [LogMessage]
parse x = map parseMessage (split' x (== '\n'))

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg @ (LogMessage _ time _) tree = case tree of
  Leaf -> Node Leaf msg Leaf
  Node left (prevMsg @ (LogMessage _ prevTime _)) right -> if time > prevTime
    then Node left prevMsg (insert msg right)
    else Node (insert msg left) prevMsg right

build' :: [LogMessage] -> MessageTree -> MessageTree
build' (x:xs) tree = build' xs $ insert x tree
build' [] tree = tree

build :: [LogMessage] -> MessageTree
build xs = build' xs Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = (inOrder left) ++ (msg : (inOrder right))

whatWentWrongMessages :: [LogMessage] -> [LogMessage]
whatWentWrongMessages x = inOrder (
    build (
      filter (
        \y -> case y of
          (LogMessage (Error _) p _) -> p >= 50
          otherwise -> False
      ) x
    )
  )

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong x = map (\y -> case y of
    (LogMessage _ _ s) -> s
    otherwise -> ""
  ) $ whatWentWrongMessages x

isError :: LogMessage -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False
