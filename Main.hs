import System.Random
import Words ( wordList )
import Data.Ord (Ord)
import Data.List (words)
import Data.Char (Char)
import Data.Bool (Bool(False))

-- New data type to represent the game state
data GameState = GameState
  {
    targetWord :: String -- Думата която трябва да познаем
  , expertMode :: Bool -- Дали играе на ниво експерт
  , lieUsed :: Bool --Ако сме в ниво експерт дали е използвана лъжата досега
  , wordsLeft :: [String] -- Останалите думи в списъка които НЕ противоречат на отговорите до сега 
  , turnsLeft :: Int -- Колко хода имаме останали (започваме всяка игра с 6 хода)
  }

--
--constants 
wordLength :: Int
wordLength = 5

startingTurns :: Int
startingTurns = 6

-- validation
isValidGuess :: String -> Bool
isValidGuess guess = length guess == wordLength && elem guess wordList

-- RNG
selectRandomWord :: IO String
selectRandomWord = do
  index <- randomRIO (0, length wordList - 1)
  return (wordList !! index)

initializeGameState :: String -> Bool -> GameState
initializeGameState word expert = GameState word expert False wordList startingTurns


playEasyGameMode :: IO ()
playEasyGameMode= do
  targetWord <- selectRandomWord
  putStrLn "Easy mode wordle game! Try to guess a 6 letter word!"
  playRoundEasy (initializeGameState targetWord False)

playStandardGameMode :: IO ()
playStandardGameMode= do
  targetWord <- selectRandomWord
  putStrLn "Wordle game! Try to guess a 6 letter word!"
  playRoundStandard (initializeGameState targetWord False)

playExpertGameMode :: IO ()
playExpertGameMode= do
  targetWord <- selectRandomWord
  putStrLn "Expert mode wordle game! Try to guess a 6 letter word! Remember, 1 missleading answer can be given!"
  playRoundExpert (initializeGameState targetWord False)


evaluateGuess :: String -> String -> String-> [Char]
evaluateGuess [] _ _ = []
evaluateGuess (x:xs) (y:ys) target = checkChar x y target : evaluateGuess xs ys target
  where
    checkChar g t target
      | g == t = 'G'
      | g `elem` target = 'Y'
      | otherwise = 'X'


playRoundEasy :: GameState -> IO ()
playRoundEasy gameState = do
  if turnsLeft gameState == 0
    then do
      putStrLn "Game over! You are out of guesses!"
    else do
        putStrLn "Enter your guess:"
        guess <- getLine
        if isValidGuess guess
            then do
            if guess `elem` wordsLeft gameState
                then do
                let feedback = evaluateGuess guess (targetWord gameState) (targetWord gameState)
                putStrLn $ "Feedback: " ++ feedback
                if feedback == replicate wordLength 'G'
                    then putStrLn "Congratulations! You guessed the word!"
                    else do
                    let newState = updateGameState gameState guess 0 feedback
                    playRoundEasy newState
                else do
                    putStrLn "Your guess doesn't match the feedback from prevois guesses. Try again."
                    playRoundEasy gameState
            else do
            putStrLn "Invalid guess. Try again."
            playRoundEasy gameState


playRoundStandard :: GameState -> IO ()
playRoundStandard gameState = do
  if turnsLeft gameState == 0
    then do
      putStrLn "Game over! You are out of guesses!"
    else do
        putStrLn "Enter your guess:"
        guess <- getLine
        if isValidGuess guess
            then do
            let feedback = evaluateGuess guess (targetWord gameState) (targetWord gameState)
            putStrLn $ "Feedback: " ++ feedback
            if feedback == replicate wordLength 'G'
                then putStrLn "Congratulations! You guessed the word!"
                else do
                let newState = updateGameState gameState guess 0 feedback
                playRoundStandard newState
            else do
            putStrLn "Invalid guess. Try again."
            playRoundStandard gameState




lyingFeedback :: GameState -> IO String
lyingFeedback gameState = do
  index <- randomRIO (0, length (wordsLeft gameState) - 1)
  let newWord = wordsLeft gameState !! index
  if newWord == targetWord gameState
    then lyingFeedback gameState
    else return (wordsLeft gameState !! index)



playRoundExpert :: GameState -> IO ()
playRoundExpert gameState = do
  if turnsLeft gameState == 0
    then do
      putStrLn "Game over! You are out of guesses!"
    else do
        putStrLn "Enter your guess:"
        guess <- getLine
        if isValidGuess guess
            then do
            if targetWord gameState == guess
              then do
              putStrLn "Congratulations! You guessed the word!"
              else do
              randomNumber <- randomRIO (1 :: Int, 3 :: Int)
              if not (lieUsed gameState) && (randomNumber == 1)
                then do
                fakeWord <- lyingFeedback gameState
                let feedback = evaluateGuess guess fakeWord fakeWord
                putStrLn $ "Feedback: " ++ feedback
                let newState = GameState (targetWord gameState)
                                             (expertMode gameState)
                                             True
                                             (wordsLeft gameState)
                                             (turnsLeft gameState - 1)
                playRoundStandard newState
                else do
                let feedback = evaluateGuess guess (targetWord gameState) (targetWord gameState)
                putStrLn $ "Feedback: " ++ feedback
                let newState = updateGameState gameState guess 0 feedback
                playRoundStandard newState
            else do
            putStrLn "Invalid guess. Try again."
            playRoundStandard gameState

greenHelper :: Char -> Int -> Int -> String -> Bool
greenHelper letter index currIndex (x:xs)
    | index == currIndex = letter == x
    | otherwise          = greenHelper letter index (currIndex + 1) xs

yellowHelper :: Char -> Int -> String -> Bool
yellowHelper letter index word = elem letter word  && not (greenHelper letter index 0 word)


updateGameState :: GameState -> String -> Int -> [Char] -> GameState
updateGameState gameState _ _ [] = GameState (targetWord gameState)
                                             (expertMode gameState)
                                             (lieUsed gameState)
                                             (wordsLeft gameState)
                                             (turnsLeft gameState - 1)
updateGameState gameState (x:xs) index (y:ys)
    | y == 'G'  = updateGameState (GameState (targetWord gameState)
                                             (expertMode gameState)
                                             (lieUsed gameState)
                                             (filter (greenHelper x index 0)  (wordsLeft gameState))
                                             (turnsLeft gameState))
                                 xs
                                 (index + 1)
                                 ys
    | y == 'Y'  = updateGameState (GameState (targetWord gameState)
                                             (expertMode gameState)
                                             (lieUsed gameState)
                                             (filter (yellowHelper x index)  (wordsLeft gameState))
                                             (turnsLeft gameState))
                                 xs
                                 (index + 1)
                                 ys
    | y == 'X'  = updateGameState (GameState (targetWord gameState)
                                             (expertMode gameState)
                                             (lieUsed gameState)
                                             (filter (notElem x)  (wordsLeft gameState))
                                             (turnsLeft gameState))
                                 xs
                                 (index + 1)
                                 ys


onlyValidChar :: String -> Bool
onlyValidChar [] = True
onlyValidChar (x:xs)
  | x == 'X' || x == 'Y' || x == 'G' = onlyValidChar xs
  | otherwise                        = False

isValidFeedback :: String -> Bool
isValidFeedback word
  | length word /= 5  = False
  | otherwise         = onlyValidChar word

playStandardHelperMode :: IO ()
playStandardHelperMode = do
  putStrLn "Welcome to Wordle Helper Mode!"
  putStrLn "Enter the secret word:"
  targetWord <- getLine
  if length targetWord /= wordLength
    then do
      putStrLn "Invalid secret word length. Try again"
      playStandardHelperMode
    else do
      if not (isValidGuess targetWord)
      then do
        putStrLn "Secret word is not in the dictionary. Try again"
        playStandardHelperMode
      else do
        putStrLn "Let the game begin!"
        helperRoundStandard (initializeGameState targetWord False)



eliminatedWords :: String -> [String] -> Int
eliminatedWords word words = sum [length words - length (wordsLeft gameState) | gameState <- [updateGameState (GameState otherWord False False words 6)
                                                                      word
                                                                      0
                                                                      (evaluateGuess word otherWord otherWord) |  otherWord <- words]]


calculateBestGuess :: [String] -> String
calculateBestGuess words = head (take 1 [word | word <- words, eliminatedWords word words == maxEliminated])
  where
    maxEliminated = maximum [eliminatedWords w words | w <- words]

helperRoundStandard :: GameState -> IO ()
helperRoundStandard  gameState = do
    let guessedWord = calculateBestGuess (wordsLeft gameState)
    putStrLn $ "My guess is: " ++ guessedWord
    putStrLn "Enter your feedback:"
    feedback <- getLine
    if isValidFeedback feedback
    then do
      if feedback == replicate wordLength 'G'
      then do
         putStrLn "Seems that I guessed the word collectly. Good game!"
      else do
        let newState = updateGameState gameState guessedWord 0 feedback
        if null (wordsLeft newState)
        then do
          putStrLn "There is no such words that match this feedback. Try again."
          helperRoundStandard gameState
        else do
          helperRoundStandard newState
    else do
      putStrLn "Invalid feedback. Try again."
      helperRoundStandard gameState


-- TODO:
playExpertHelperMode:: IO ()
playExpertHelperMode = do
  putStrLn "Welcome to Wordle Expert Helper Mode! Remember you can use only 1 lie"
  putStrLn "Enter the secret word:"
  targetWord <- getLine
  if length targetWord /= wordLength
    then do
      putStrLn "Invalid secret word length. Try again"
      playExpertHelperMode
    else do
      if not (isValidGuess targetWord)
      then do
        putStrLn "Secret word is not in the dictionary. Try again"
        playExpertHelperMode
      else do
        putStrLn "Let the game begin!"
        helperRoundExpert (initializeGameState targetWord False)


helperRoundExpert :: GameState -> IO ()
helperRoundExpert gameState = do
    let guessedWord = calculateBestGuess (wordsLeft gameState)
    putStrLn $ "My guess is: " ++ guessedWord
    putStrLn "Enter your feedback:"
    feedback <- getLine
    if isValidFeedback feedback
    then do
      if feedback == replicate wordLength 'G'
      then do
         putStrLn "Seems that I guessed the word collectly. Good game!"
      else do
        let newState = updateGameState gameState guessedWord 0 feedback
        if null (wordsLeft newState)
        then do
          helperRoundStandard (initializeGameState (targetWord gameState) False)
        else do
          helperRoundExpert newState
    else do
      putStrLn "Invalid feedback. Try again."
      helperRoundExpert gameState


main :: IO ()
main = do
  putStrLn "Choose a mode (game/helper):"
  mode <- getLine
  case mode of
    "game" -> do
      putStrLn "Choose a difficulty level (easy/standard/expert):"
      difficulty <- getLine
      case difficulty of
        "easy" -> playEasyGameMode
        "standard" -> playStandardGameMode
        "expert" -> playExpertGameMode
        _ -> putStrLn "Invalid difficulty level."
    "helper" -> do
      putStrLn "Choose a difficulty level (standard/expert):"
      difficulty <- getLine
      case difficulty of
        "standard" -> playStandardHelperMode
        "expert" -> playExpertHelperMode  --TODO
    _ -> putStrLn "Invalid mode."
