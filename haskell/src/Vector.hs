module Vector
( Vector3,
    sumVec,
    binaryOp,
    add,
    sub,
    mul,
    square,
    simpleOp,
    scalarMult,
    neg,
    normalize,
    dotProduct,
    crossProduct,
    bounceVectorOffPlane
) where

type Vector3 = (Double, Double, Double)
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

bounceVectorOffPlane :: Vector3 -> Vector3 -> Vector3
bounceVectorOffPlane vector normal = add vector $ scalarMult normal $ 2 * dotProduct normal (neg vector)