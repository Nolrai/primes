module Main (main) where

import Reified
import System.Console.Argument
import System.Console.Command
import System.Console.Program

main :: IO ()
main = single commands

commands :: Tree (Command IO)
commands = Node
  (command "usage" "" . io
    $ putStrLn "No command given." >> (showUsage commands))
  (flip Node [] . toCommand <$> reifiedFunctions)
  where
  toCommand :: Reified IO -> Command IO
  toCommand (Reified name description function) =
    command name description
      (toAction function)
