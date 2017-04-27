{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main (module Main) where

import qualified Control.Exception  as E
import           System.Environment (getEnv)
import           Types
import           Yesod

import           Simulation
import           World

-- to do:
-- add Pause button
-- set maximum time for simulation
-- make curWorld a local variable

data Gravity = Gravity

instance Yesod Gravity

mkYesod "Gravity" [parseRoutes|
  /          HomeR    GET
  /advance   AdvanceR POST
  /solar     SolarR   GET
  /world4    World4R  GET
  /world2    World2R  GET
  /world2neg World2negR GET
  /extra/drawworld.js DrawWorldR GET
  /display-params DisplayParamsR GET
|]

boxColor, bodyColor :: String

boxColor   = "#000"
bodyColor  = "#333"

boxSizeX, boxSizeY  :: Int
boxSizeX    = 600
boxSizeY    = 600

framesPerS :: Int
framesPerS = 16

getHomeR :: HandlerT Gravity IO Html
getHomeR = defaultLayout $ do
  setTitle "Gravity"

  [whamlet|
    <div #box>
      <p>
      <canvas #sky width=#{boxSizeX} height=#{boxSizeY}>
         Your browser doesn't support HTML 5
      <p>
        Field interaction demo based on one of
        <a href="http://www.cse.unsw.edu.au/~chak/" target="_blank">
        Manuel Chakravarty</a>'s Haskell course exercises.
        The simulation is done in Haskell on the server.
        Client code uses HTML 5 to display instantaneous positions of bodies.
        It communicates with the (stateless) server using JSON.
        The web site is written in
        <a href="http://www.yesodweb.com/" target="_blank">Yesod</a>.
        <div>
          <button #reset>Reset
          <select>
            <option value="solar"> Inner planets
            <option value="world4"> Four stars
            <option value="world2"> Two particles
            <option value="world2neg"> Two negative particles
  |]

  toWidget [cassius|
    #box
      width:#{show boxSizeX}px
      height:#{show boxSizeY}px
      margin-left:auto
      margin-right:auto
    canvas
      background-color:#{boxColor}
    body
      background-color:#{bodyColor}
      color:#eee
      font-family:Arial,Helvetica,sans-serif
      font-size:small
    a
      text-decoration:none
      color:#bdf
    #sky
      border:1px solid #888
  |]

  addScriptRemote "//ajax.googleapis.com/ajax/libs/jquery/1.9.1/jquery.min.js"
  addScript DrawWorldR

--------------------
-- Server side logic
--------------------

postAdvanceR :: Handler Value
postAdvanceR = do
    -- Parse the request body to a data type as a JSON value
    world <- requireJsonBody
    -- user time in seconds
    let userTime = 1.0 / fromIntegral framesPerS
    let worldTime = userTime * usrToWrldTime world
    -- do the simulation
    returnJson $ advanceWorld worldTime world

getSolarR  :: Handler Value
getSolarR  = returnJson solarWorld

getWorld4R :: Handler Value
getWorld4R = returnJson world4

getWorld2R :: Handler Value
getWorld2R = returnJson world2

getWorld2negR :: Handler Value
getWorld2negR = returnJson world2neg

getDrawWorldR :: Handler ()
getDrawWorldR = sendFile "text/javascript" "extra/drawworld.js"

getDisplayParamsR :: HandlerT Gravity IO String
getDisplayParamsR =
  return $ concat [show framesPerS, " ", show boxSizeX, " ", show boxSizeY]

main :: IO ()
main = do
    portEither <- getPortEither
    let port = case portEither of
                        Right val -> read val
                        Left _    -> 3000
    -- start the server
    warp port Gravity
  where
    -- try to get the port from environment
    getPortEither :: IO (Either IOError String)
    getPortEither = E.try (getEnv "PORT")
