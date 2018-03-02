main = interact id

type V3 = (Int, Int, Int)
data ParticleData = ParticleData {
    p :: V3,
    v :: V3,
    a :: V3
} deriving (Show, Eq)

quadratic :: Float -> Float -> Float -> [Float]
quadratic a b c =
    if sqrtPart < 0 then []
    else if sqrtPart == 0 then [-b / (a * 2)]
    else [(-b + d) / (a * 2), (-b - d) / (a * 2)]
    where
        sqrtPart = b * b - 4*a*c
        d = sqrt sqrtPart

intersections
    ParticleData {p=(px1, py1, pz1), v=(vx1, vy1, vz1), a=(ax1, ay1, az1)}
    ParticleData {p=(px2, py2, pz2), v=(vx2, vy2, vz2), a=(ax2, ay2, az2)} =
        quadratic a b c
    where
        a = px1 - px2
        b = vx1 - vx2 + (ax2 - ax1) // 2
        c = (ax1 - ax2) / 2

particles = [
    ParticleData {p = (-6,0,0), v = (3,0,0), a = (0,0,0)},
    ParticleData {p = (-4,0,0), v = (2,0,0), a = (0,0,0)},
    ParticleData {p = (-2,0,0), v = (1,0,0), a = (0,0,0)},
    ParticleData {p = (3,0,0), v = (-1,0,0), a = (0,0,0)}
    ]
