module Main where

import Prelude

import Control.Monad.Except (runExcept, runExceptT)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.RWS (RWSResult(..), runRWS)
import Control.Monad.RWS.Trans (runRWST)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.GameEnvironment (GameEnvironment, gameEnvironment)
import Data.GameEnvironment2 (GameEnvironment', gameEnvironment')
import Data.GameState (GameState, initialGameState)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (split)
import Effect (Effect)
import Effect.Console (log)
import Game (game)
import Game2 (game')
import Math (e)
import Node.ReadLine as RL
import Node.Yargs.Applicative (Y, runY, flag, yarg)
import Node.Yargs.Setup (usage)

runGame :: GameEnvironment -> Effect Unit
runGame env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface

  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler currentState input = do
      case runRWS (game (split (wrap " ") input)) env currentState of
        RWSResult state _ written -> do
          for_ written log
          RL.setLineHandler (lineHandler state) $ interface
      RL.prompt interface
      pure unit

  RL.setLineHandler (lineHandler initialGameState) interface
  RL.prompt interface

  pure unit


runGame' :: GameEnvironment' -> Effect Unit
runGame' env = do
  interface <- RL.createConsoleInterface RL.noCompletion
  RL.setPrompt "> " interface

  let
    lineHandler :: GameState -> String -> Effect Unit
    lineHandler currentState input = do
      case runExcept $ runRWST (game' (split (wrap " ") input)) env currentState of
        Right (RWSResult state _ written) -> do
          for_ written log
          RL.setLineHandler (lineHandler state) $ interface
        Left e -> do
          log e
          RL.setLineHandler (lineHandler currentState) $ interface
      RL.prompt interface
      pure unit

  RL.setLineHandler (lineHandler initialGameState) interface
  RL.prompt interface

  pure unit


main' :: Effect Unit
main' = runY (usage "$0 -p <player name>") $ map runGame env
  where
  env :: Y GameEnvironment
  env = gameEnvironment
          <$> yarg "p" ["player"]
                       (Just "Player name")
                       (Right "The player name is required")
                       false
          <*> flag "d" ["debug"]
                       (Just "Use debug mode")


main :: Effect Unit
main = runY (usage "$0 -p <player name>") $ map runGame' env
  where
  env :: Y GameEnvironment'
  env = gameEnvironment'
          <$> yarg "p" ["player"]
                       (Just "Player name")
                       (Right "The player name is required")
                       false
          <*> flag "d" ["debug"]
                       (Just "Use debug mode")
          <*> flag "c" []
                       (Just "Use cheat mode")