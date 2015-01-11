module Misc where
import List(..)
import Maybe

last : List a -> a
last = reverse >> head

isJust : Maybe a -> Bool
isJust x = if x == Nothing then False else True

fromJust : Maybe a -> a
fromJust (Just x) = x

crossApply : List (a -> b) -> List a -> List b
crossApply fs xs = foldr (\a  -> (++) <| (flip map) xs a) [] fs

crossApply3 : (a -> b -> c -> d) -> List a -> List b -> List c -> List d
crossApply3 f a b c = map f a `crossApply` b `crossApply` c

iterateN : Int -> (a -> a) -> a -> List a
iterateN len f x = if len == 0 then [] else x :: iterateN (len-1) f (f x)

zip : List a -> List b -> List (a,b)
zip xs ys = case (xs, ys) of 
    ((x::xz), (y::yz)) -> (x,y) :: zip xz yz 
    otherwise -> []

index : a -> List a -> Maybe Int
index v xs = 
    let helper xs i = if 
            | xs == [] -> Nothing
            | head xs == v -> Just i
            | otherwise -> helper (tail xs) (i + 1)
    in  helper xs 0

retrieve : Int -> List a -> Maybe a 
retrieve idx xs = if 
    | xs == [] || idx < 0 -> Nothing
    | idx == 0 -> Just <| head xs
    | otherwise -> retrieve (idx - 1) (tail xs)