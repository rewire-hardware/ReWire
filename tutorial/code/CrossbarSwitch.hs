{-# LANGUAGE DataKinds #-}

import Prelude hiding ((^), (+))
import ReWire

switch :: t -> t -> Bool -> (t, t)
switch x _ True  = (x,x)
switch x y False = (x,y)

type W8 = W 8

data Maybe4 = Maybe4 (Maybe W8) (Maybe W8) (Maybe W8) (Maybe W8)

type Bool16 = (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool)

crossbar :: Maybe4 -> Bool16 -> Maybe4 
crossbar (Maybe4 x10 x20 x30 x40) (c11,c12,c13,c14,c21,c22,c23,c24,c31,c32,c33,c34,c41,c42,c43,c44)
   = let
          (x41,y31) = switch x40 Nothing c41
          (x42,y32) = switch x41 Nothing c42
          (x43,y33) = switch x42 Nothing c43
          (_,y34) = switch x43 Nothing c44

          (x31,y21) = switch x30 y31 c31
          (x32,y22) = switch x31 y32 c32
          (x33,y23) = switch x32 y33 c33
          (_,y24) = switch x33 y34 c34

          (x21,y11) = switch x20 y21 c21
          (x22,y12) = switch x21 y22 c22
          (x23,y13) = switch x22 y23 c23
          (_,y14) = switch x23 y24 c24

          (x11,y10) = switch x10 y11 c11
          (x12,y20) = switch x11 y12 c12
          (x13,y30) = switch x12 y13 c13
          (_,y40) = switch x13 y14 c14
     in
       Maybe4 y10 y20 y30 y40

data Inp = Inp Maybe4 Bool16 | NoInput
            
data Out = Out Maybe4 | Nix

dev :: Inp -> ReacT Inp Out Identity ()
dev (Inp m4 b16) = signal (Out (crossbar m4 b16)) >>= dev
dev NoInput      = signal Nix >>= dev

start :: ReacT Inp Out Identity ()
start = signal Nix >>= dev

