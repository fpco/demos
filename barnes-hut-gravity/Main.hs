{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
module Main where

import Yesod
import Data.Aeson
import System.Environment (getEnv)
import qualified Control.Exception as E

import Json
import Types
import WorldDefs
import Simulation (advanceWorld)

-- to do:
-- set maximum time for simulation

data Gravity = Gravity

instance Yesod Gravity

mkYesod "Gravity" [parseRoutes|
  /          HomeR    GET
  /advance   AdvanceR POST
  /solar     SolarR   GET
  /grid      GridR    GET
  /galaxy    GalaxyR  GET
  /simple    SimpleR  GET
  /world4    World4R  GET
  /app.js    AppJsR   GET
  /jquery.js JqueryR  GET
|]

boxColor   = "#000"::String
bodyColor  = "#333"::String
boxSize    = 850::Int
framesPerS = 16::Int

getHomeR = defaultLayout $ do
  setTitle "Gravity"

  [whamlet|
    <div #box>
      <p>
        Gravitational interaction demo based on one of
        <a href="http://www.cse.unsw.edu.au/~chak/" target="_blank">Manuel Chakravarty</a>'s
        Haskell course exercises. The simulation is done in Haskell on the server.
        Client code uses HTML 5 to display instantaneous positions of bodies.
        It communicates with the (stateless) server using JSON. The web site is written in
        <a href="http://www.yesodweb.com/" target="_blank">Yesod</a>.
        <div .left>
          <button #reset>Reset
          <button #pause>Pause
          <select>
            <option value="solar"> Inner planets
            <option value="world4"> Four stars
            <option value="grid"> A large grid of planets
            <option value="galaxy"> A galaxy with a central black hole
            <option value="simple"> Sun and Earth
          <input #bhtree type=checkbox checked>
          <label for=bhtree> Show BHTree
          <input #discardx type=checkbox checked>
          <label for=discardx> Discard particles that go out of bounds
        <div .right>
          FPS:
          <span #fps> ???

      <canvas #sky width=#{boxSize} height=#{boxSize}>
         Your browser doesn't support HTML 5
  |]

  toWidget [cassius|
    #box
      width:#{show boxSize}px
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
    .left
      float: left;
    .right
      float: right;
    #fps
      background: white;
      border: 1px solid gray;
      color: black;
      border-radius: 5px;
      padding: 1px 10px;
  |]

  addScript JqueryR
  toWidget [julius|

    // Wrap everything in Simulation Module
    var Simulation = {};

    // The URL to advance the world
    Simulation.advanceUrl = "@{AdvanceR}";

    // The desired fps
    Simulation.fps = #{toJSON framesPerS};

    // map from simulation names to urls
    Simulation.urls =
      { solar:   "@{SolarR}"
      , grid:    "@{GridR}"
      , galaxy:  "@{GalaxyR}"
      , simple:  "@{SimpleR}"
      , world4:  "@{World4R}"
      };
  |]

  addScript AppJsR

-- Server side logic

postAdvanceR :: Handler RepJson
postAdvanceR = do
    world <- parseJsonBody_
    -- user time in seconds
    let userTime = 1.0 / fromIntegral framesPerS
    jsonToRepJson $ advanceWorld userTime world

getSolarR  :: Handler RepJson
getSolarR  = jsonToRepJson solarWorld
getGridR   :: Handler RepJson
getGridR   = jsonToRepJson gridWorld
getGalaxyR :: Handler RepJson
getGalaxyR = jsonToRepJson galaxyWorld
getSimpleR :: Handler RepJson
getSimpleR = jsonToRepJson simpleWorld
getWorld4R :: Handler RepJson
getWorld4R = jsonToRepJson world4

-- Static Resources
getJqueryR :: Handler ()
getJqueryR = sendFile "text/javascript" "jquery.min.js"
getAppJsR :: Handler ()
getAppJsR = sendFile "text/javascript" "app.js"

-- Main function
main = do
    portEither <- getPortEither
    let port = case portEither of
                        Right val -> read val
                        Left _    -> 3000
    warp port Gravity
  where
    getPortEither :: IO (Either IOError String)
    getPortEither = E.try (getEnv "PORT")

