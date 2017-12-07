module Lib
    ( someFunc
    ) where

import Data.Word

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Color = (Word8, Word8, Word8)
type Vector3 = (Double, Double, Double) 

data Screen = Screen {width::Word, height::Word}
data Ray = Ray {origin::Vector3, direction::Vector3}
data Material = Material {color::Color, specular_power::Double, shininess::Double, reflectivity::Double}
data Sphere = Sphere {center::Vector3, radius::Double}
data Triangle = Triangle {v1::Vector3, v2::Vector3, v3::Vector3} 
data Plane = Plane {normal::Vector3, dist2Plane::Double}
data PointLight = PointLight {difuse::Color, location::Vector3, specular::Color}
data Camera = Camera {position::Vector3, rotation::Vector3, fov::Word, screen::Screen}
data Intersection = Intersection {intDistance::Double, intersect::Vector3, normalDir::Vector3}


sumVec :: Vector3 -> Double
sumVec (x,y,z)= x+y+z

binaryOp :: Vector3 -> Vector3 -> (Double -> Double -> Double) -> Vector3
binaryOp (x1, y1, z1) (x2, y2, z2) op = (op x1 x2, op y1 y2, op z1 z2)

add :: Vector3 -> Vector3 -> Vector3
add v1 v2 = binaryOp v1 v2 (+)

sub :: Vector3 -> Vector3 -> Vector3
sub v1 v2 = binaryOp v1 v2 (-)

mul :: Vector3 -> Vector3 -> Vector3
mul v1 v2 = binaryOp v1 v2 (*)

square :: Vector3 -> Vector3
square v = mul v v

simpleOp :: Vector3 -> (Double -> Double) -> Vector3
simpleOp (x, y, z) f = (f x, f y, f z)

scalarMult :: Vector3 -> Double -> Vector3
scalarMult v num = simpleOp v ((*) num)

normalize :: Vector3 -> Vector3
normalize (x, y, z) = let norm = sqrt . sumVec $  (x^2, y^2, z^2) in (x/norm, y/norm, z/norm) 

class Intersectable a where
    intersects :: a -> Ray -> Maybe (Intersection)

instance Intersectable Sphere where
    intersects (Sphere center radius) (Ray origin direction) =
        let a = sumVec . square $ direction
            b = 2 * (sumVec $ mul direction $ sub origin center)
            c = (sumVec . square) (sub origin center) - radius ^ 2
            discriminant = b ^ 2 - 4 * a * c
            e = 0.001
        in
            if discriminant > e then
                let t0 = (-b + sqrt discriminant)/2
                    t1 = (-b - sqrt discriminant)/2
                    in
                        if t0 >= 0 && t1 >= 0 then
                            let distance = min t0 t1
                                intersect = add origin (scalarMult direction distance)
                                normalDir = normalize (sub intersect center)
                            in
                                Just (Intersection distance intersect normalDir)
                        else
                            Nothing
            else
                Nothing


instance Intersectable Triangle where
    intersects (Triangle v1 v2 v3) (Ray origin direction) = Nothing

instance Intersectable Plane where
    intersects (Plane normal distance) (Ray origin direction) = Nothing