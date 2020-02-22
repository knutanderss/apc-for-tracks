module ApcDevice where

import Control.Concurrent
import Control.Monad
import Lib
import qualified Safe
import qualified System.MIDI as MIDI

updateLeds :: MIDI.Connection -> MIDI.MidiEvent -> IO ()
updateLeds dest (MIDI.MidiEvent _ midiMsg) = case midiMsg of
  MIDI.MidiMessage _ (MIDI.NoteOn key vel) ->
    case convertKeyToButton key of
      Nothing -> return ()
      Just (Clip row col) -> do
        reset dest
        forM_ (rowToKeys row) (changeToColor dest YELLOW)
        MIDI.send dest $ MIDI.MidiMessage 1 $ MIDI.NoteOn key 1
  _ -> return ()

data Color = OFF | GREEN | YELLOW | RED

colorToCode :: Color -> Int
colorToCode OFF = 0
colorToCode GREEN = 1
colorToCode RED = 3
colorToCode YELLOW = 5

reset :: MIDI.Connection -> IO ()
reset dest = forM_ [0 .. 63] (changeToColor dest OFF)

changeToColor :: MIDI.Connection -> Color -> Int -> IO ()
changeToColor dest color key =
  MIDI.send dest
    $ MIDI.MidiMessage 1
    $ MIDI.NoteOn key
    $ colorToCode color

convertKeyToButton :: Int -> Maybe Button
convertKeyToButton k
  | k >= 0 && k <= 63 = Just $ Clip (getRow k) (getCol k)  -- Clip buttons
  | k >= 64 && k <= 71 = Just $ Below (k - 64) -- Buttons below clips
  | k >= 82 && k <= 71 = Just $ Beside (k - 82) -- Buttons on the right side 
  | otherwise = Nothing
  where
    getRow n = 8 - (n `div` 8)
    getCol n = n `mod` 8

convertButtonToKey :: Button -> Int
-- (8 - row) because apc starts counting rows from bottom
convertButtonToKey (Clip row col) = (8 - row) * 8 + col 
convertButtonToKey (Below k) = k + 64
convertButtonToKey (Beside k) = k + 82

rowToKeys :: Int -> [Int]
rowToKeys r = map (+ (8 - r) * 8) [0 .. 7]

data Button
  = Clip Int Int -- Row Column
  | Below Int
  | Beside Int
  deriving (Show, Eq)

getApcSourceAndDestionation :: IO (Maybe (MIDI.Source, MIDI.Destination))
getApcSourceAndDestionation = do
  maybeSource <- MIDI.enumerateSources >>= getApc
  maybeDest <- MIDI.enumerateDestinations >>= getApc
  return $ (,) <$> maybeSource <*> maybeDest

getApc :: MIDI.MIDIHasName a => [a] -> IO (Maybe a)
getApc xs = Safe.headMay <$> filterM filterApc xs
  where
    filterApc :: MIDI.MIDIHasName a => a -> IO Bool
    filterApc source = (== "APC MINI") <$> MIDI.getName source
