module Main where

import RuleMe (export, openURL, lunch, breakfast, na, dinner, dessert, sides, condiments)
import Text.HTML.TagSoup

url :: String
url = "http://www.ruled.me/keto-recipes/condiments/"

getUrls :: [Tag String] -> [String]
getUrls = map f . filter (~== "<a rel=bookmark>")
    where f = fromAttrib "href"

toPage :: String -> Int -> String
toPage base 1 = base
toPage base i = base ++ "page/" ++ (show i) ++ "/"

        
main :: IO ()
main =  do
    urls <- (concat . map getUrls) <$> (sequence $ map openURL pages)
    sequence_ $ map (export condiments) urls
    return ()
    where pages = map (toPage url) [1..3]