module Simulation (moveParticle, accelerate, advanceWorld) where
  
import World
import Physics

-- Move a particle according to its velocity for the given number of (simulated) seconds.
--
moveParticle :: Float -> Particle -> Particle
moveParticle 0 p = p
moveParticle t (Particle m p@(px,py) v@(vx,vy)) = Particle m (px+vx*t,py+vy*t) v
    
-- Accelerate a particle in dependence on the gravitational force excerted by all other particles for
-- the given number of (simulated) seconds.
--
accelerate :: Float -> [Particle] -> [Particle]
accelerate time [] = []
accelerate time [x] = [x]
accelerate time xs = map (accpars time xs) xs
  where 
    accpars :: Float -> [Particle] -> Particle -> Particle
    accpars time [] x =x 
    accpars time (x:xs) p@(Particle m n v@(vx,vy)) = accpars time xs (Particle m n (vx+a*time,vy+b*time))
      where 
        (a,b)=force p x


-- Progressing the world state
--
advanceWorld :: unused -> Float -> World -> World
advanceWorld _ time w@(World a b c []) = w
advanceWorld _ time w@(World a b c xs) = World a b c new
  where
    t=time/c
    pars=accelerate t xs
    new = map (moveParticle t) pars
