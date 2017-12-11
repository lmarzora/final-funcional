module Lib
    ( someFunc
    ) where

import Data.Maybe

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Color = (Double, Double, Double)
type Vector3 = (Double, Double, Double) 

data Screen = Screen {screenWidth::Int, screenHeight::Int}
data Ray = Ray {origin::Vector3, direction::Vector3}
data Material = Material {color::Color, specularPower::Integer, shininess::Double, reflectivity::Double} deriving Eq
data Shape = Sphere {center::Vector3, radius::Double}
            | Triangle {v1::Vector3, v2::Vector3, v3::Vector3}
            | Plane {normal::Vector3, dist2Plane::Double} deriving Eq
data PointLight = PointLight {diffuse::Color, location::Vector3, specular::Color}
data Camera = Camera {position::Vector3, rotation::Vector3, camFov::Int, screen::Screen}
data Intersection = Intersection {intDistance::Double, intersect::Vector3, normalDir::Vector3}
data SceneObject = SceneObject {shape::Shape, material::Material} deriving Eq

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

neg :: Vector3 -> Vector3
neg = flip scalarMult (-1)

normalize :: Vector3 -> Vector3
normalize (0, 0, 0) = (0, 0, 0)
normalize (x, y, z) = let norm = sqrt . sumVec $  (x^2, y^2, z^2) in (x/norm, y/norm, z/norm) 

dotProduct :: Vector3 -> Vector3 -> Double
dotProduct = curry $ sumVec . (uncurry mul)
 
crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (ux, uy, uz) (vx, vy, vz) = (uy*vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx)

class Intersectable a where
    intersects :: a -> Ray -> Maybe (Intersection)

instance Intersectable Shape where
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


getFirstIntersection :: [Maybe (Intersection, SceneObject)] -> Maybe (Intersection, SceneObject)
getFirstIntersection [] = Nothing
getFirstIntersection [o] = o
getFirstIntersection (o:objects) = let getDistance = intDistance . fst in
                                        foldr (\mi1 mi2 ->
                                            do
                                                i1 <- mi1 
                                                i2 <- mi2 
                                                if getDistance i1 < getDistance i2 then
                                                    return i1 
                                                else 
                                                    return i2
                                                ) o objects

getIntersections :: Ray -> [SceneObject] -> [Maybe (Intersection, SceneObject)]
getIntersections ray objects = map (\o -> (flip intersects ray . shape) o >>= Just . (flip (,) o)) objects

-- traceRayThroughPixel:: Double -> Double -> Camera -> [PointLight] -> [SceneObject] -> Int -> Color
-- traceRayThroughPixel x y cam lights objects 0 = (0,0,0)
-- traceRayThroughPixel x y cam lights objects depth =
--     let ray = rayThroughPixel x y cam 
--         intersection = getFirstIntersection $ getIntersections ray objects
--     in
--         case intersection of 
--             Just (intersect, object) -> 
--                 foldr (\ pointLight finalColor -> 
--                     let reflect = scalarMult $ traceRayThroughPixel)
--             Nothing -> (0, 0, 0)

pixelColorFromRay :: Ray -> [PointLight] -> [SceneObject] -> Int -> Color
pixelColorFromRay ray lights objects depth =
    let
        intersection = getFirstIntersection $ getIntersections ray objects
        dir = direction ray
    in
        case intersection of 
            Just (intersectData, object) -> 
                foldr (\pointLight finalColor -> 
                    let 
                        lightLocation = location pointLight
                        hitLocation = intersect intersectData
                        normalDirection = normalDir intersectData
                        ref =  (reflectivity . material) object
                        pow = (specularPower . material) object
                        shine = (shininess . material) object
                        specCol = specular pointLight
                        difCol = diffuse pointLight
                        rayRef = Ray hitLocation (bounceVectorOffPlane dir normalDirection)
                        reflect = scalarMult (pixelColorFromRay rayRef lights objects (depth-1)) ref
                        lightContrib = add (diffuseTerm object lightLocation hitLocation normalDirection) (specularTerm dir lightLocation hitLocation normalDirection pow shine specCol)
                        res = add finalColor reflect
                    in
                        if shadowFactor lightLocation hitLocation object objects then
                            add res $ mul difCol lightContrib
                        else
                            res
                ) (0, 0, 0) lights
            Nothing -> (0, 0, 0)


shadowFactor :: Vector3 -> Vector3 -> SceneObject -> [SceneObject] -> Bool
shadowFactor lightLocation hitLocation object objects = 
    let lightDirection = normalize $ sub hitLocation lightLocation
        shadowRay = Ray lightLocation lightDirection
    in
        case getFirstIntersection $ getIntersections shadowRay objects of
            Just (_, obj) -> if obj == object then True else False
            Nothing -> False

diffuseTerm :: SceneObject -> Vector3 -> Vector3 -> Vector3 -> Vector3
diffuseTerm obj lightLocation hitLocation hitNormal =
    scalarMult (color (material obj)) (max 0 (dotProduct hitNormal (normalize (sub lightLocation hitLocation))))

specularTerm :: Vector3 -> Vector3 -> Vector3 -> Vector3 -> Integer -> Double -> Color -> Vector3
specularTerm eyeVector lightLocation hitLocation hitNormal power shine col =
    let negEye = neg eyeVector
        subVec = normalize $ sub lightLocation hitLocation
        addVec = normalize $ add subVec negEye
    in
        scalarMult col (shine * ((dotProduct addVec hitNormal) ^ power))

rayThroughPixel :: Double -> Double -> Camera -> Ray
rayThroughPixel x y cam = Ray (position cam) $ pointOnScreen x y cam

pointOnScreen :: Double -> Double -> Camera -> Vector3
pointOnScreen x y cam = 
    let fov = fromIntegral $ camFov cam
        width = fromIntegral $ screenWidth $ screen cam
        height = fromIntegral $ screenHeight $ screen cam
        pos = position cam
    in
        foldr add pos [
            scalarMult (0, 0, 1) (focalLenght fov width),
            ((x - 0.5) * width, 0, 0),
            (0, (y - 0.5) * height, 0)
            ]

focalLenght :: Double -> Double -> Double
focalLenght angle dimension = dimension / (tan angle * (pi/180) / 2)


bounceVectorOffPlane :: Vector3 -> Vector3 -> Vector3
bounceVectorOffPlane vector normal = add vector $ scalarMult normal $ 2 * dotProduct normal (neg vector)