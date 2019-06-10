module Main where

import Lib
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "Hello World!"
    button <- UI.button # set UI.text "Click me!"
    getBody window #+ [element button]
    on UI.click button $ const $ do
        element button # set UI.text "I have been clicked!"

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup
