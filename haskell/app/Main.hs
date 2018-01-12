module Main where

import Lib
import Geometry
import Vector
import Image

-- main :: IO ()
-- main = let
--     positions = [(0, 0), (0.5, 0.5), (1, 1)]
--     points = map (\(x,y) -> pointOnScreen x y camera) positions
--     rays = map (\(x,y) -> rayThroughPixel x y camera) positions
--     in do
--         print points
--         print rays

main :: IO ()
main = let 
        positions = relativePositions 640 480
        rays = map (\(x,y) -> rayThroughPixel x y camera) positions
        colors = map (\r -> pixelColorFromRay r lights scene 2) rays
        in do
            writeImage "tmp.ppm" 640 480 colors

scene :: [SceneObject]
scene = [SceneObject (Sphere (0, 0, 20) 10) (Material (0, 0.5, 1) 20 1 0.1)]
        -- SceneObject (Sphere (5, 2, 1) 1) (Material (1, 0.5, 0) 4 0.25 0.5)]

camera :: Camera
camera = Camera (0, 0, -2) (0, 0, 0) 90 (Screen 640 480) 

lights :: [PointLight]
lights = [PointLight (1, 1, 1) (0, 0, 0) (1, 1, 1)]
--         PointLight (-1, 1, 0) (-10, 0 ,7) (1, 0, 0.5)]
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