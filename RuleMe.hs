module RuleMe where

import Data.List
import Network.HTTP
import Text.HTML.TagSoup
import Network.URI (parseURI)
import Text.XML.Plist
import Data.ByteString (unpack)
import Data.Word8
import Data.List.Split

get url = let uri = case parseURI url of
                          Nothing -> error $ "Invalid URI: " ++ url
                          Just u -> u in 
              
              simpleHTTP (defaultGETRequest_ uri) >>= getResponseBody

data Ingredient = Ingredient {
    quantity :: String,
    description :: String,
    measurment :: String
} deriving(Show)

data Recipe = Recipe {
    title :: String,
    source :: String,
    image :: [Word8],
    steps :: [([Word8], String)],
    ingredients :: [Ingredient],
    yield :: String
} deriving(Show)

openURL x = parseTags <$> (getResponseBody =<< simpleHTTP (getRequest x))

breakfast = PlArray [
        PlDict[
         ("CATEGORY_ID", PlInteger 84)
        ,("ITEM_TYPE_ID", PlInteger 102)
        ,("NAME", PlString "Breakfast")
        ,("USER_ADDED", PlBool False)
        ]
    ]
    
na = PlArray []

getRecipe url = do
    tags <- openURL url -- list of tags w/ list of opening can closing tags...
    let t = crunch $ takeWhile (~/= "</div>") $ (dropWhile (~/= "<div class=articleTitle>") tags)
    let imageUrl = fromAttrib "src" $ head $ filter (~== "<img>" ) $ dropWhile (~/= "<div class=postImage_f>") tags
    i <- get imageUrl
    s <- parseSteps tags
    return Recipe { 
        title = t, 
        source = url, 
        image = unpack $ i, 
        steps = s,
        ingredients = parseIngredients tags,
        yield = parseYields tags
    }

parseIngredients = map (parseIngredient . words . innerText . takeWhile (~/= "</li>")) . sections (~== "<li>") . takeWhile (~/= "</ul>") . dropWhile (~/= "<ul id=zlrecipe-ingredients-list>")

parseIngredient xs = Ingredient { quantity = xs !! 0, measurment = xs !! 1, description = (unwords $ drop 2 xs) }

parseYields = drop 7 . head . filter (isPrefixOf "Yields") . map (innerText . takeWhile (~/= "</p>")) . sections (~== "<p>")

parseContent tags = reverse $ drop 1 $ reverse $ map (takeWhile (~/= "</p>")) $ sections (~== "<p>") ps
                    where ps = tail $ dropWhile (~/= "</h2>") $ dropWhile (~/= "</ul>") $ takeWhile (~/= "<table>") $ dropWhile (~/= "<div class=entry-content>") tags

parseSteps :: [Tag String] -> IO ([([Word8], String)])
parseSteps tags = do { i <- images; return $ zipWith (\s i -> (unpack i, s)) steps i }
        where images = sequence (map (get . fromAttrib "src" . head . filter (~== "<img>")) $ filter (or . map (~== "<img>")) ss)
              steps = map (unwords . drop 1 . words . innerText . takeWhile (~/= "</p>")) $ filter (and . map (~/= "<img>")) ss
              ss = parseContent tags

crunch = unwords . words . innerText

toStepsPlist steps = map f steps
    where f i = PlDict [
             ("DIRECTION_TEXT", PlString " ")
            ,("IMAGE",  PlData (fst i))
            ,("IS_HIGHLIGHTED", PlBool False)
            ,("LABEL_TEXT", PlString (snd i))
            ,("VARIATION_ID", PlInteger (-1))
            ]

toIngredientPlist is = map f is
    where f i = PlDict [
             ("DESCRIPTION", PlString (description i))
            ,("DIRECTION", PlString " ")
            ,("INCLUDED_RECIPE_ID", PlInteger (-1))
            ,("IS_DIVIDER", PlBool False)
            ,("IS_MAIN", PlBool False)
            ,("MEASUREMENT", PlString (measurment i))
            ,("QUANTITY", PlString (quantity i))
            ]
        
toPlist c r = PlDict [
     ("AFFILIATE_ID", PlInteger (-1))
    ,("CATEGORIES", c)
    ,("COURSE_ID", PlInteger 0)
    ,("COURSE_NAME", PlString "--")
    ,("CUISINE_ID", PlInteger (-1))
    ,("DIFFICULTY", PlInteger 0)
    ,("DIRECTIONS", PlString $ unlines $ map (\p -> snd p) s)
    ,("DIRECTIONS_LIST", PlArray $ toStepsPlist $  s)
    ,("IMAGE", PlData (image r))
    ,("INGREDIENTS", PlArray $ toIngredientPlist $ (ingredients r))
    ,("INGREDIENTS_TREE",  PlArray $ toIngredientPlist $ (ingredients r))
    ,("KEYWORDS", PlString "")
    ,("MEASUREMENT_SYSTEM", PlInteger 0)
    ,("NAME", PlString (title r))
    ,("NOTE", PlString "")
    ,("NOTES_LIST", PlArray [])
    ,("NUTRITION", PlString "")
    ,("PREP_TIMES", PlArray [])
    ,("PUBLICATION_PAGE", PlString "")
    ,("SERVINGS", PlInteger 0)
    ,("SOURCE", PlString (source r))
    ,("SUMMARY", PlString "")
    ,("TYPE", PlInteger 102)
    ,("URL", PlString (source r))
    ,("YIELD", PlString (yield r))
    ]
    where s = steps $ r

export c url = do
    r <- getRecipe url
    writePlistToFile filename (PlArray  [toPlist c r])
    where filename = ((splitOn "/" url) !! 3) ++ ".mgourmet"