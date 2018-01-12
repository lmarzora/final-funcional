module Geometry
    (Shape(..)) where
    
import Vector
data Shape = Sphere {center::Vector3, radius::Double}
            | Triangle {v1::Vector3, v2::Vector3, v3::Vector3}
            | Plane {normal::Vector3, dist2Plane::Double} deriving (Eq, Show)

