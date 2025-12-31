module Puzzle where

import Data.Maybe
import Data.List

type Position = (Integer, Integer) -- (x, y) Co-ordinates
type Elem = (Bool, Position, Region) -- True: Crown, False: x
type Colour = Integer
type Region = ([Position], Colour)
type Puzzle = ([Region], [[Maybe Elem]]) -- The second argument is basically a grid of Elements

eliminatePositions :: Puzzle -> Puzzle
eliminatePositions (regions, grid) = let rowRegions = map (\(Just r) -> r) (filter isJust (map (\r -> if verifyRow r then (Just r) else Nothing) regions))
                                         colRegions = map (\(Just r) -> r) (filter isJust (map (\r -> if verifyCol r then (Just r) else Nothing) regions))
                                         rowFillRegions = map (\(Just r) -> r) (filter isJust (map (\r -> if verifyRowFill r (toInteger $ length grid) then (Just r) else Nothing) regions))
                                     in (regions, (markCols colRegions . markRows rowRegions . markRowFill rowFillRegions) grid)

markRowFill :: [Region] -> [[Maybe Elem]] -> [[Maybe Elem]]
markRowFill = markRowFill

markRow :: Region -> [[Maybe Elem]] -> [[Maybe Elem]]
markRow r grid = let (((_, y):_), _) = r
                     xs = map (\(x, _) -> x) (fst r)
                     row = grid!!(fromInteger y)
                     newRow = map (\(i, e) -> case e of
                                                  (Just (_, (x, _), _)) | x `elem` xs -> e
                                                                        | otherwise -> Just $ (False, (x, y), r)  
                                                  Nothing -> Just $ (False, (fromInteger i, y), r)) (zipWith (\i1 i2 -> (fromInteger i1, i2)) [0..(toInteger $ length row)] row)
                 in take (fromInteger y) grid ++ [newRow] ++ drop (fromInteger (y + 1)) grid

markRows :: [Region] -> [[Maybe Elem]] -> [[Maybe Elem]]
markRows [r] g = markRow r g
markRows (r:rs) g = let g' = markRow r g
                    in markRows rs g'

markCol :: Region -> [[Maybe Elem]] -> [[Maybe Elem]]
markCol r grid = let (((x, _):_), _) = r
                     ys = map (\(_, y) -> y) (fst r)
                     (!!!) :: [[Maybe Elem]] -> Integer -> [Maybe Elem]
                     (!!!) es idx = map (!!(fromInteger idx)) es
                     col = grid!!!x
                     newCol = map (\(i, e) -> case e of
                                                  (Just (_, (_, y), _)) | y `elem` ys -> e
                                                                        | otherwise -> Just $ (False, (x, y), r)  
                                                  Nothing -> Just $ (False, (fromInteger x, i), r)) (zipWith (\i1 i2 -> (fromInteger i1, i2)) [0..(toInteger $ length col)] col)
                     gridt = transpose grid
                 in transpose $ take (fromInteger x) gridt ++ [newCol] ++ drop (fromInteger (x + 1)) gridt

markCols :: [Region] -> [[Maybe Elem]] -> [[Maybe Elem]]
markCols [r] g = markCol r g
markCols (r:rs) g = let g' = markCol r g
                    in markCols rs g'

-- This function is used by eliminatePositions to verify if a certain region is limited to only one row.
verifyRow :: Region -> Bool
verifyRow ((p:ps), _) = let (_, yInit) = p
                        in case filter (\(_, y) -> y /= yInit) ps of
                               [] -> True
                               _ -> False

-- This function is used by eliminatePositions to verify if a certain region is limited to only one column.
verifyCol :: Region -> Bool
verifyCol ((p:ps), _) = let (xInit, _) = p
                        in case filter (\(x, _) -> x /= xInit) ps of
                               [] -> True
                               _ -> False

-- This function is used by eliminatePositions to verify if a certail region occupies and entire row.
verifyRowFill :: Region -> Integer -> Bool
verifyRowFill (ps, _) size = let ys = sort $ map (\(x, y) -> y) ps
                             in size `elem` (map (\(_, c) -> c) . map (\x -> (head x, toInteger $ length x)) . group) ys

-- This function is used by eliminatePositions to verify if a certail region occupies and entire column.
