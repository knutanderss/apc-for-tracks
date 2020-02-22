{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Concurrent
import Control.Monad
import Lib
import qualified System.MIDI as MIDI
import qualified GI.Gtk as Gtk
import Data.GI.Base
import ApcDevice

main :: IO ()
main = do
  maybeDevice <- getApcSourceAndDestionation
  case maybeDevice of
    Nothing -> print "Couldn't find APC mini"
    Just (source, destination) -> do
      destConn <- MIDI.openDestination destination
      sourceConn <- MIDI.openSource source (Just $ updateLeds destConn)
      reset destConn
      startGUI sourceConn destConn

startGUI :: MIDI.Connection -> MIDI.Connection -> IO ()
startGUI sourceConnection destConnection = do
  MIDI.start sourceConnection
  Gtk.init Nothing
  win <- new Gtk.Window [ #title := "APC Tracks Led" ]

  on win #destroy $ do
    reset destConnection
    MIDI.stop sourceConnection
    Gtk.mainQuit

  #showAll win

  Gtk.main
