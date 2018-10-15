{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Data.Char (intToDigit)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import System.Exit (exitSuccess)
import System.Random (randomRIO)

data Player =
    Human Text
  | Computer
  deriving (Eq, Ord)

instance Show Player where
  show Computer = "Computer"
  show (Human t) = TL.unpack t

type Winner = Maybe Player

type PlayerInput = Int

type Score = Int

type GameState = M.Map Player (Score, EarnedPointF)

type EarnedPointF = PlayerInput -> Bool

minInput :: PlayerInput
minInput = 0

maxInput :: PlayerInput
maxInput = 5 -- if this goes above 9, danger!

charToInput :: M.Map Char PlayerInput
charToInput = M.fromList $ 
  zipWith (,) inputChars inputs
    where inputs = [minInput .. maxInput]
          inputChars = intToDigit <$> inputs

pointsEarned :: Score
pointsEarned = 1

scoreToWin :: Score
scoreToWin = 3

main :: IO ()
main = do
  let initialS :: GameState
      initialS = M.fromList [ (Human "Player One", (0, odd))
                            , (Computer, (0, even)) ]
  _ <- runStateT runGame $ initialS
  return ()

getPlayerInput :: Player -> IO (Player, PlayerInput)
getPlayerInput p = do
  case p of
    (Human _) -> getHumanPlayerInput p
    Computer  -> getComputerPlayerInput p

getHumanPlayerInput :: Player -> IO (Player, PlayerInput)
getHumanPlayerInput p = do
  putStr $ show p ++ ", enter input: "
  c <- getChar
  putStrLn ("" :: String)
  let input = M.lookup c charToInput
  case input of
    Nothing -> (putStrLn $
      "Please enter a number from 0 to 5.")
      >> (getPlayerInput p)
      <* putStrLn ("" :: String)
    Just i -> return (p, i)

getComputerPlayerInput :: Player -> IO (Player, PlayerInput)
getComputerPlayerInput p = do
  i <- randomRIO (minInput, maxInput)
  putStrLn $ show p ++ " chose " ++ show i
  return (p, i)

findWinner :: GameState -> Winner
findWinner gameState = 
  case null winningScores of
    True  -> Nothing
    False -> Just $ fst $ M.elemAt 0 winningScores
  where winningScores = 
          M.filter ((>= scoreToWin) . fst) gameState

findPointEarners :: GameState -> PlayerInput -> [Player]
findPointEarners gameState summed =
  M.keys $ M.filter (($ summed) . snd) gameState

assignPoints :: [Player] -> GameState -> GameState
assignPoints ps gameState =
  M.mapWithKey (addPoint ps) gameState
    where
      addPoint :: [Player] 
               -> Player
               -> (Score, EarnedPointF)
               -> (Score, EarnedPointF)
      addPoint players k (s, f) = 
        if k `elem` players
        then (s + pointsEarned, f)
        else (s, f)

runGame :: StateT GameState IO Winner
runGame = forever $ do
  gameState <- get
  let players :: [Player]
      players = M.keys gameState
  -- playersWithInputs :: [(Player, PlayerInput)]
  playersWithInputs <- liftIO $ sequence $ 
                         getPlayerInput <$> players
  let inputs = snd <$> playersWithInputs
      summed = foldr (+) 0 inputs
      pointEarners = findPointEarners gameState summed
  liftIO $ putStr "Point earned by: "
  liftIO $ print pointEarners
  liftIO $ putStrLn ""
  _ <- modify (assignPoints pointEarners)
  winner <- gets findWinner
  _ <- case winner of
        Nothing -> liftIO $ return ()
        Just w  -> liftIO $ 
                     (putStrLn $ show w ++ " wins!")
                     >> exitSuccess
  gets findWinner
