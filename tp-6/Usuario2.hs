import MapV1


ejMap :: Map Int Int
ejMap = assocM 4 4
        $ assocM 3 3
        $ assocM 2 2
        $ assocM 1 1
        $ emptyM

ejMap2 :: Map String Int
ejMap2 = assocM "d" 8
        $ assocM "c" 7
        $ assocM "b" 6
        $ assocM "a" 5
        $ emptyM

fromJust :: Maybe a -> a -- O(1)
-- PRECOND: No puede ser Nothing
fromJust (Just x) = x

jus :: Int -> Map Int Int -> Int
jus i m = case lookupM i m of
         Just s -> s
         Nothing -> error "no hay valor dado" 