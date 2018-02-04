{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lib
import Geometry
import Vector
import Image
import Data.List



import System.Environment (getArgs)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forever, forM_)


-- main :: IO ()
-- main = let 
--         positions = relativePositions 640 480
--         rays = map (\(x,y) -> rayThroughPixel x y camera) positions
--         colors = map (\r -> pixelColorFromRay r lights scene 2) rays
--         in do
--             writeImage "tmp.ppm" 640 480 colors

scene :: [SceneObject]
scene = [SceneObject (Sphere (0, 0, 20) 10) (Material (0, 0.5, 1) 20 1 0.1)]
        -- SceneObject (Sphere (5, 2, 1) 1) (Material (1, 0.5, 0) 4 0.25 0.5)]

camera :: Camera
camera = Camera (0, 0, -2) (0, 0, 0) 90 (Screen 640 480) 

lights :: [PointLight]
lights = [PointLight (1, 1, 1) (0, 0, 0) (1, 1, 1)]
--         PointLight (-1, 1, 0) (-10, 0 ,7) (1, 0, 0.5)]

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

sortImage :: [(Int, (Double, Double, Double))] -> [(Double, Double, Double)]
sortImage image = 
  let sorted = sortBy (\(a1, b1) (a2, b2) -> compare a1 a2) image
  in
    map snd sorted
  
getImage :: Int -> Int -> Process [(Double, Double, Double)]
getImage width height = go [] (width*height)
  where
    go :: [(Int, (Double, Double, Double))] -> Int -> Process [(Double, Double, Double)]
    go image 0 = return (sortImage image)
    go image n = do
      response <- expect
      go (response:image) (n-1)


computePixelColor :: ProcessId -> Int -> Double -> Double -> [SceneObject] -> [PointLight] -> Camera -> Process()
computePixelColor master id x y scene light camera = send master (id, (pixelColorFromRay (rayThroughPixel x y camera) lights scene 5))

slave :: (ProcessId, ProcessId, ([SceneObject], [PointLight], Camera)) -> Process()
slave (master, workqueue, (scene, lights, cam)) = do
        liftIO . putStrLn $ "SLAVE"
        us <- getSelfPid
        go us scene lights cam
        where go us scene lights cam = do
                send workqueue us
                receiveWait
                  [ match $ \(id, (x,y)) -> do
                    computePixelColor master id x y scene lights camera
                    go us scene lights cam
                  , match $ \() -> return ()
                  ]
remotable ['slave]

master :: Backend -> [SceneObject] -> [PointLight] -> Camera -> [NodeId] -> Process()
master backend scene lights camera slaves = do
        liftIO . putStrLn $ "MASTER"
        liftIO . putStrLn $ "Slaves: " ++ show slaves
        us <- getSelfPid
        workqueue <- spawnLocal $ do
                liftIO . putStrLn $ "WORKQUEUE"
                forM_ (pixels 640 480) $ \(x,y) -> do
                  them <- expect
                  liftIO . putStrLn $ show (x,y)
                  send them (x + y * 640, relativePosition x y 640 480)
                forever $ do
                  pid <- expect
                  send pid ()
        forM_ slaves $ \nid -> spawn nid ($(mkClosure 'slave) (us, workqueue, (scene, lights, camera)))
        image <- getImage 640 480
        liftIO $ writeImage "traced.ppm" 640 480 image
        terminateAllSlaves backend

rtable :: RemoteTable
rtable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port rtable 
      startMaster backend $ \slaves -> do
        master backend scene lights camera slaves
    ["slave", host, port] -> do
      backend <- initializeBackend host port rtable 
      startSlave backend
    