
-- This generates the prelude index to the report by hyper-linking all
-- of the names in a hand-written document.  This is probably too
-- obscure to bother explaining.  Hardwires for the Haskell report at the
-- moment.

module Main where

import Data.Char
import System.IO
import System.IO.Error

main = do refs <- readRefFile "reportrefs"
          doFiles refs ["prelude-index"]

doFiles r files = do mapM (doFile r) files
                     putStrLn "Done."

doFile r f = catchIOError
               (do putStrLn ("Reading " ++ f ++ ".idx")
                   ls <- readFile (f ++ ".idx")
                   let output = expandAllRefs r (lines ls)
                   writeFile ("haskell-report-html/" ++ f ++ ".html")
                             (unlines output))
               (\err -> putStrLn ("Error: " ++ show err))

-- This sets up the parts of the state that need to be reset at the start of
-- each file.

type Refs = [(String,String)]

expandAllRefs r ls = expandAll1 r False ls
expandAll1 r table [] = []
expandAll1 r table (l:ls) | l == "#table" = expandAll1 r True ls
                          | l == "#endtable" = expandAll1 r False ls
                          | table = ("<tr><td><tt>" ++ nbspaces (expandRefs r l)
                                     ++ "</tt></td></tr>") : rest
                          | otherwise = (expandRefs r l) : rest
 where rest = expandAll1 r table ls

expandRefs :: Refs -> String -> String
expandRefs r "" = ""
expandRefs r ('#':l) = expandRef r "" l
expandRefs r (c:cs) = c : expandRefs r cs

expandRef r txt ('V':l) = expandVar r txt (parseRef l)
expandRef r txt ('I':l) = expandInstance r txt (parseRef l)
expandRef r txt ('T':l) = expandTycon r txt (parseRef l)
expandRef r txt ('L':l) = expandLink r txt (parseRef l)
expandRef r txt ('S':l) = expandSect r txt (parseRef l)
expandRef r txt ('&':l) = "</tt></td><td><tt>" ++ expandRefs r l
expandRef r txt ('#':l) = "#" ++ expandRefs r l
expandRef r txt ('.':l) = expandRefs r l
expandRef r txt l = error ("Bad ref:" ++ l ++ "\n")

parseRef = break (\c -> isSpace c || c == '#')

expandVar r txt (v,rest) = let n = mangleVar v
                               f = lookup n r in
                             case f of
                               Nothing -> trySig r txt v rest n
                               _ -> anchor v f n txt ++ expandRefs r rest

expandTycon r txt (t,rest) = let n = mangleTycon t
                                 f = lookup n r in
                             anchor t f n txt ++ expandRefs r rest

expandInstance r txt (c,'#':rest ) = let (t,rest') = parseRef rest
                                         n = mangleInstance c t
                                         f = lookup n r in
                                       anchor c f n txt ++ expandRefs r rest'
expandInstance r txt (c,l) = error ("bad instance " ++ l ++ "\n")

expandSect r txt (s,rest) = let n = mangleSect s
                                f = lookup n r in
                              "(see " ++ anchor s f n txt ++ ")" ++
                               expandRefs r rest

expandLink r _ (t,'#':l') = expandRef r t l'
expandLink r _ (l,l') = error ("Bad link: " ++ l ++ l' ++ "\n")

trySig r txt v rest n =
   let c = parseClass rest
       n = mangleTycon c
       f = lookup n r in
     anchor v f n txt ++ expandRefs r rest

anchor str file tag txt =
         case file of
           Just f -> "<a href=\"" ++ f ++ ".html#" ++ tag ++
                      "\">" ++ t ++ "</a>"
           Nothing -> "Bad tag:" ++ tag ++ " " ++ t
     where
       t = htmlS $ if txt == "" then str else txt

mangleVar n = "$v" ++ mangleName (filter (\c -> not (c `elem` "()")) n)

mangleTycon n = "$t" ++ mangleName n

mangleInstance c t = "$i" ++ mangleName c ++ "$$" ++ mangleName t

mangleSect s = "sect" ++ s


mangleName r = concat $
               map (\c -> case c of '(' -> "$P"
                                    ')' -> "$C"
                                    '-' -> "$D"
                                    '[' -> "$B"
                                    ']' -> "$c"
                                    ',' -> "$x"
                                    '#' -> "$p"
                                    '$' -> "$D"
                                    '|' -> "$b"
                                    '!' -> "$E"
                                    '&' -> "$A"
                                    '^' -> "$U"
                                    '>' -> "$G"
                                    '<' -> "$L"
                                    '=' -> "$Q"
                                    _   -> [c]) r

mangleType t = mangleName (case t of
                              "(IO" -> "IO"
                              "(a->b)" -> "->"
                              "[a]" -> "[]"
                              x -> x)



readRefFile :: String -> IO [(String, String)]
readRefFile f = catchIOError
                      (do l <- readFile f
                          return (map parseKV (lines l)))
                      (\e -> do putStrLn ("Can't read ref file: " ++ f)
                                print e
                                return [])

parseKV l = let (k,l1) = span (/= '=') l
                val    = case l1 of
                           ('=':v) -> trim v
                           _       -> ""
              in (trimr k,val)

parseClass s = let s1 = (skip "(" . skip "::") s
                   (c,_) = span isAlpha (trim s1) in
                 c

trim s = dropWhile isSpace s
trimr s = reverse (dropWhile isSpace (reverse s))
starts [] _ = True
starts _ [] = False
starts (a:as) (b:bs) | a == b = starts as bs
                     | otherwise = False

skip val s = if val `starts` (trim s) then
                drop (length val) (trim s)
             else s


htmlEncode '>' = "&gt;"
htmlEncode '<' = "&lt;"
htmlEncode '&' = "&amp;"
htmlEncode c   = [c]

htmlS s = concat (map htmlEncode s)

nbspaces [] = []
nbspaces (' ' : cs) = "&nbsp;" ++ nbspaces cs
nbspaces ('<':cs) = ['<'] ++ c ++ nbspaces r where
                        (c,r) = span (/= '>') cs
nbspaces (c:cs) = c:nbspaces cs
