This module's dependencies are:

> -- import qualified Data.List as Dl

This function is a prototype of the function used to seperate statements into workable symbols.
It would need more special symbols like '...', "...", f"...", [...], or {...}. but this is the basic idea.

> split :: (Eq a) => a -> [a] -> [[a]]
> split _ [] = []
> split delim sentence@(head:rest)
>       | head == delim = [delim] : (split delim rest)
>       | otherwise = let (word, others) = span (/= delim) sentence
>                     in word : (split delim others)

And this function is an alternate version of the split function to allow for multiple delimiters.
This still keeps the delimiter for further parsing. Also removes empty lists where there are two delimiters.

> splitMany :: (Eq a) => [a] -> [a] -> [[a]]
> splitMany delims list
>     | delims == [] = [list]
>     -- | (length delims) == 1 = split (delims !! 0) list Keeping this in case an edge case arises.
>     | list == [] = []
>     | (length list) == minlen = [list]
>     | otherwise = if minlen > 0 then (take minlen list):latterPart
>                   else latterPart
>                   where minlen = minimum [len | delim <- delims, let len = length $ takeWhile (/= delim) list]
>                         latterPart = [(list !! minlen)]:(splitMany delims (drop (minlen + 1) list))

And this part contains the main function, the beginning point.
For now, it is barren, Only containing tests.

> main =
>         do
>             string <- getLine
>             delims <- getLine
>             let delim = delims !! 0
>             let splitString = split delim string
>             putStrLn (foldl1 (\a b -> a ++ [delim] ++ b) (splitString))
>             putStrLn (foldl1 (++) (filter (/= [delim]) (splitString)))
>             putStrLn (foldl (\x y -> concat (filter (/= [y]) (split y x))) string delims)
>             putStrLn (foldl1 (\x y -> x ++ "." ++ y) (splitMany delims string))

Needs Data.List so I left it out. Dependencies stinky.      putStrLn $ Dl.intercalate (delim:"NOT" ++ [delim]) $ filter (/= [delim]) (splitString)
