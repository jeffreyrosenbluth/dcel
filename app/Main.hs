{-# LANGUAGE OverloadedLists #-}

module Main where

import DCEL

main :: IO ()
main = putStrLn "Hello DCEL"

p0 = Point 0 3
p1 = Point 1 1
p2 = Point 2 0

h01 = HalfEdge2D 0 0 1 (Just 2) Nothing
h10 = HalfEdge2D 1 0 0  Nothing (Just 3)
h12 = HalfEdge2D 1 0 3 (Just 3) (Just 0)
h21 = HalfEdge2D 2 0 2 (Just 1) (Just 2)

f0 = Face2D 0

v0 = Vertex2D p0 0
v1 = Vertex2D p1 1
v2 = Vertex2D p2 3

dcel0 = VectorDCEL [Vertex2D p0 0, Vertex2D p1 1]
                   [f0]
                   [ HalfEdge2D 0 0 1 (Just 1) Nothing
                   , HalfEdge2D 1 0 0 Nothing (Just 0)
                   ]

dcel1 = VectorDCEL [v0, v1, v2] [f0] [h01, h10, h12, h21]
