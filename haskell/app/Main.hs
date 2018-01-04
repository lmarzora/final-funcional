module Main where

import Lib
import Geometry
import Vector


main :: IO ()
main = let
    positions = relativePositions 4 3
    point = pointOnScreen 0 0 camera
  in do
      print point

-- main :: IO ()
-- main = let 
--   positions = relativePositions 4 3
--   rays = map (\(x,y) -> rayThroughPixel x y camera) positions
--   colors = map (\r -> pixelColorFromRay r lights scene 1) rays
--   in do
--     print positions
--     print rays
--     print colors 

scene :: [SceneObject]
scene = [SceneObject (Sphere (4, 0, 10) 4.0) (Material (1, 0, 0) 20 1 0.1)]

camera :: Camera
camera = Camera (0, 0, 0) (0, 0, 0) 90 (Screen 640 480) 

lights :: [PointLight]
lights = [PointLight (1, 1, 0.5) (5, -2, 0) (1, 1, 1)]
-- import System.Environment (getArgs)
-- import Control.Distributed.Process
-- import Control.Distributed.Process.Node (initRemoteTable, runProcess)
-- import Control.Distributed.Process.Backend.SimpleLocalnet
-- import Control.Monad (forever, forM_)
    
-- main :: IO ()
-- main = do
--   args <- getArgs

--   case args of
--     ["master", host, port] -> do
--       backend <- initializeBackend host port initRemoteTable
--       startMaster backend (master backend)
--     ["slave", host, port] -> do
--       backend <- initializeBackend host port initRemoteTable
--       startSlave backend

-- master :: Backend -> [NodeId] -> Process ()
-- master backend slaves = do
--   -- Do something interesting with the slaves
--   liftIO . putStrLn $ "Slaves: " ++ show slaves
--   -- Terminate the slaves when the master terminates (this is optional)
--   terminateAllSlaves backend