module Main where

import RuleMe (export, openURL, breakfast, na)
import Text.HTML.TagSoup



url = "http://www.ruled.me/keto-recipes/breakfast/"

getUrls :: [Tag String] -> [String]
getUrls = map f . filter (~== "<a rel=bookmark>")
    where f = fromAttrib "href"
    
main :: IO ()
main = do
    tags <- openURL url
    sequence_ $ map (export breakfast) (getUrls tags)
    
    
    