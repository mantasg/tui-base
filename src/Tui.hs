{-# LANGUAGE OverloadedStrings #-}

module Tui where

import Control.Monad.IO.Class (liftIO)
import System.Exit (die)
import System.Directory

import Data.List (sort, filter)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Cursor.Simple.List.NonEmpty

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

tui :: IO ()
tui = do
    initialState <- buildInitialState
    endState <- defaultMain tuiApp initialState
    print endState

data TuiState =
    TuiState { tuiStatePaths :: NonEmptyCursor FilePath }
    deriving (Show, Eq)

type ResourceName = String

tuiApp :: App TuiState e ResourceName
tuiApp =
    App
        { appDraw = drawTui
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleTuiEvent
        , appStartEvent = pure
        , appAttrMap = const $ attrMap mempty 
            [ ("selected", fg red)
            ]
        }

buildInitialState :: IO TuiState
buildInitialState = do
    contents <- getDirectoryContents "."
    case NE.nonEmpty (sort $ filter (/=".") contents) of
        Nothing -> die "There are no contents."
        Just ne -> pure $ TuiState $ makeNonEmptyCursor ne

makeGreedy :: Widget a -> Widget a
makeGreedy w = w { vSize = Greedy, hSize = Greedy }

drawTui :: TuiState -> [Widget ResourceName]
drawTui ts = 
    let nec = tuiStatePaths ts
     in [ makeGreedy $ border $ vBox 
            [ border $ vBox $ [ drawPath True "Hello World" ]
            , border $ vBox $ concat 
                  [ map (drawPath False) $ reverse $ nonEmptyCursorPrev nec
                  , pure $ drawPath True $ nonEmptyCursorCurrent nec
                  , map (drawPath False) $ nonEmptyCursorNext nec
                  ]
            ]
        ]

drawPath :: Bool -> FilePath -> Widget n
drawPath b = (if b then withAttr "selected" else id) . str

handleTuiEvent :: TuiState -> BrickEvent n e -> EventM n (Next TuiState)
handleTuiEvent s e =
    case e of
        VtyEvent vtye ->
            case vtye of
                EvKey (KChar 'q') [] -> halt s
                EvKey KDown []       -> do
                    let nec = tuiStatePaths s
                    case nonEmptyCursorSelectNext nec of 
                        Nothing   -> continue s
                        Just nec' -> continue $ s { tuiStatePaths = nec' }
                EvKey KUp []       -> do
                    let nec = tuiStatePaths s
                    case nonEmptyCursorSelectPrev nec of 
                        Nothing   -> continue s
                        Just nec' -> continue $ s { tuiStatePaths = nec' }
                EvKey KEnter[]    -> do
                    s' <- liftIO $ setCurrentDirectory (nonEmptyCursorCurrent $ tuiStatePaths s) >> buildInitialState
                    continue s'

                _ -> continue s                
        _ -> continue s
