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

dotProduct :: Vector3 -> Vector3 -> Double
dotProduct = curry $ sumVec . (uncurry mul)
 
crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (ux, uy, uz) (vx, vy, vz) = (uy*vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx)

class Intersectable a where
    intersects :: a -> Ray -> Maybe (Intersection)

instance Intersectable Sphere where
    intersects (Sphere center radius) (Ray origin direction) =
        let a = sumVec . square $ direction
            b = 2 * (dotProduct direction $ sub origin center)
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
                                Just $ Intersection distance intersect normalDir
                        else
                            Nothing
            else
                Nothing


instance Intersectable Triangle where
    intersects (Triangle v1 v2 v3) (Ray origin direction) =
        let e = 0.000001
            edge1 = sub v2 v1
            edge2 = sub v3 v1
            p = crossProduct direction edge2
            det = dotProduct edge1 p
        in
            if det < e then
                let t = sub origin v1
                    u = dotProduct t p
                in
                    if u < 0 || u > det then
                        let q = crossProduct t edge1
                            v = dotProduct direction q
                        in
                            if v < 0 || (u + v) > det then
                                let dist = (dotProduct edge2 q) / det
                                    intersect = add origin $ scalarMult direction dist
                                    normalDir = normalize $ crossProduct v1 v2
                                in
                                    Just $ Intersection dist intersect normalDir
                            else
                                Nothing
                    else
                        Nothing
            else
                Nothing

instance Intersectable Plane where
    intersects (Plane normal distance) (Ray origin direction) = 
        let e = 0.001
            vd = dotProduct normal direction
        in
            if vd < 0 then
                let v0 = - dotProduct normal origin + distance
                    dist = v0/vd
                in
                    if dist < e then
                        let intersect = add origin $ scalarMult direction dist in
                            Just $ Intersection dist intersect normal
                    else
                        Nothing
            else
                Nothing