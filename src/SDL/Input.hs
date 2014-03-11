{-# LANGUAGE TemplateHaskell #-}
module SDL.Input ( keyDownEvent
                 , keyUpEvent
                 , wasKeyPressed
                 , wasKeyReleased
                 , isKeyHeld
                 , Input
                 , makeInput
                 ) where

import Control.Lens ( makeLenses
                    , (%~)
                    , (^.)
                    , Getting
                    )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as Map
import qualified Graphics.UI.SDL as SDL

type KeyMap = Map.Map SDL.Scancode Bool
data Input = Input { _held :: KeyMap
                   , _pressed :: KeyMap
                   , _released :: KeyMap
                   }
    deriving (Show)
makeLenses ''Input

makeInput :: Input
makeInput = Input Map.empty Map.empty Map.empty

scancode :: SDL.Event -> SDL.Scancode
scancode = SDL.keysymScancode . SDL.keyboardEventKeysym

keyDownEvent :: Input -> SDL.Event -> Input
keyDownEvent input event =
    let insert m = Map.insert (scancode event) True m
        input' = held %~ insert $ input
        input'' = pressed %~ insert $ input'
    in input''

keyUpEvent :: Input -> SDL.Event -> Input
keyUpEvent input event =
    let insert is_pressed m = Map.insert (scancode event) is_pressed m
        input' = released %~ (insert True) $ input
        input'' = pressed %~ (insert False) $ input'
    in input''

keyLookup :: Getting KeyMap Input KeyMap -> Input -> SDL.Scancode -> Bool
keyLookup getter input code = fromMaybe False $ Map.lookup code (input^.getter)

wasKeyPressed :: Input -> SDL.Scancode -> Bool
wasKeyPressed = keyLookup pressed

wasKeyReleased :: Input -> SDL.Scancode -> Bool
wasKeyReleased = keyLookup released

isKeyHeld :: Input -> SDL.Scancode -> Bool
isKeyHeld = keyLookup held
