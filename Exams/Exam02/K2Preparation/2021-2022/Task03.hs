{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-unused-matches #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Werror #-}

allObjects :: [(String, [String])] -> [String]
allObjects xss = concatMap step xss
    where
        step (_, values) = filter step' values
            where
                step' value = case lookup value xss of
                    (Just _) -> False
                    Nothing -> True

cleanUp :: [(String, [String])] -> [(String, [String])]
cleanUp xss
    | null emptyValues = xss
    | otherwise = cleanUp newElements
        where
            newElements = map removeEmpty (filter (\(_, values) -> not (null values)) xss)
            removeEmpty (key, values) = (key, filter (`notElem` emptyValues) values)
            emptyValues = toBeRemoved xss

toBeRemoved :: [(String, [String])] -> [String]
toBeRemoved xss = map (\(key, _) -> key) filtered
    where
        filtered = filter (\(_, values) -> null values) xss

inv :: [(String, [String])]
inv = [ ("docs", ["ids", "invoices"]), ("ids", ["passport"]),  ("invoices", []), ("memes", []),
        ("family", ["new year", "birthday"]), ("funny", ["memes"]), ("pics", ["family", "funny"]) ]

-- >>> allObjects inv
-- ["passport","new year","birthday"]

-- >>> toBeRemoved inv
-- ["invoices","memes"]

-- >>> cleanUp inv
-- [("docs",["ids"]),("ids",["passport"]),("family",["new year","birthday"]),("pics",["family"])]
