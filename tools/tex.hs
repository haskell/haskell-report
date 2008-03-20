{-
This is the pre-processor which converts the Latex / verbatim of the
Haskell Report into HTML.

This program has evolved in a rather unplanned manner and is now a 
jumble of absolutely crappy code, tacked-on features, no documentation, 
and other fun stuff.  I dislike the other tex -> html translations so
I wrote my own.  So there.  At least you can tweak the heck out of
this thing to make the html look just right if you want to.

Lots of stuff could be made more automatic.  There's still a bunch of
stuff I do by hand to latex code.  Especially the images in figures.

The parsing code here is absolutely terrible.  I wrote it before the
parser library was part of Hugs.  Also everything is way too
stateful.  I will cheerfully refund your money if you don't like this
software. 

This code runs under Hugs 1.4.  Probably under all the other Haskell
compilers too.

To run, just change to the directory containing the .tex / .hs / .verb
files and run main.  It always reads `html.config' to setup
parameters.

Configuration file stuff:

aux = file            Add definitions in a latex .aux file
htmldir = directory   Output directory
files = file1, file2, ...  Files to be processed.   Cumulative.
index = file          Index file to be generated if given.  All
                      section headings are hyperlinked from the index.
refs = file           file resolving refs to files.  Maps latex tags
                      onto the html file where they are defined.
morerefs = file       For refs to another document for hyperlinks
                      across docs
style = s1, s2, ...   Set document style(s).   Styles:
                         report   -- latex document style
                         article  -- latex document style
                         microsoftsymbols  -- Use microsoft symbol font
~mac = value          Define macros used in raw html
#comment              Only on separate lines.

You can probably make the generated stuff a lot prettier by changing
the html in the page header / footer.


Within the latex file, here's what's special:

%** raw html    ~foo expands the definition of foo.  ~prev and ~next
                are special 
%*anchor on/off
                Add labels to Haskell source code
%*ignore        Define lines to be ignored.
%*endignore
%*section n     Set current section number.  Gotta do this for appendix.


Some special latex commands (you need to add the definitions to the
latex file).

\newcommand{\anchor}[2]{#2}  -- 
\anchor{url}{text in anchor}

\newcommand{\inputHS}{\input}
\input{foo}   include the Haskell source file foo.hs

\newcommand{\outlinec}{\outline}  % Centered outlines in html


No expansion of Tex definitions is attempted.  You gotta add every
user-defined latex command to the master table.  Many primitive latex
commands are supported but not all.

          John Peterson

-}

module Main where

import Control.Monad(foldM)
import System.IO
import Data.Char(isSpace, isAlpha, isDigit)

data FontStyle = RM | IT | TT | Bold | Sym | UL
    deriving (Eq,Show)

data Font = Font FontStyle Int String   -- Has a size and color
    deriving (Eq, Show)

-- Not in H98
instance Show (a->b) where
  show f = "Function"

ttFont = Font TT 3 ""
mathFont = Font IT 3 ""
romanFont = Font RM 3 ""


data HaskellDef =
  HVars [String] | HTycon String | HInstance String String
     deriving Show


data HTML = HProtect [HTML] -- Boundary for local font changes
          | HString String  -- data characters in text mode
          | HISO String     -- ISO character name
          | HPara           -- Paragraph break 
          | HCmd String     -- a raw html command
          | HEol            -- a soft end of line
          | HVerb String    -- A string of chars in Verb mode
          | HVerbLine String -- as above with a newline 
          | HAnchor String HTML -- Hyperlinked text
          | HDef String     -- hyperlink label definition
          | HSub HTML       -- Subscript 
          | HSuper HTML     -- Superscript
          | HQuote HTML     -- Quoted text
          | HCenter HTML    -- Centered text
          | HFont FontStyle -- Font selection.
          | HColor String   -- Color selection.
          | HSize Int       -- Size selection.
          | HLineBreak      -- Hard line break (<br>)
          | HTable String String [Int] HTML -- Tables.  
          | HSep            -- Column separation in a table
          | HHdr Int String HTML   -- Heading with anchor
          | HList String HTML -- Itemized lists and friends 
          | HItem HTML      -- List bullet 
          | HSpecial Int    -- Special character
          | HEmpty
   deriving Show


type PC = IO (State, String, [String])
type PCFun = State -> String -> [String] -> PC

emit :: HTML -> PCFun
emit h s = doChar (h +++ s)

h +++ s = s {html = h : html s}
h +++. s = s {html = h : dropWhite (html s)}

dropWhite (HEol : h) = dropWhite h
dropWhite (HPara : h) =  dropWhite h
dropWhite h = h


delimiter :: String -> PCFun
delimiter d s l ls = do -- putStr ("Closing: " ++ d ++ "\n") 
                        (head (delims s)) d s l ls

catchd :: (String -> [HTML] -> PCFun) -> State -> String -> [String] ->
          PCFun -> PC
catchd f s l ls k = do -- putStr ("open at " ++ l ++ "\n")
                       k s' l ls where
  s' = s {delims = (passHtml f (html s)) : delims s, html = []}
  passHtml f h = \d s -> f d (reverse (html s))
                             (s {html = h, delims = tail (delims s)} ) 

reqDelim :: String -> ([HTML] -> HTML) -> PCFun -> (String -> [HTML] -> PCFun)
reqDelim d f k = \d' h s l ls -> if d == d' then
                                    k (f h +++ s) l ls
                                 else
                                    do putStr ("Missing " ++ d ++ ", found " ++
                                               d' ++ " instead.\n")
                                       doChar s l ls


reqDelim1 :: String -> ([HTML] -> HTML) -> PCFun -> (String -> [HTML] -> PCFun)
reqDelim1 d f k = \d' h s l ls -> if d == d' then
                                    k (f h +++ s) l ls
                                  else
                                    do putStr ("Missing " ++ d ++ ", found " ++
                                              d' ++ " instead.\n")
                                       return (s,l,ls)


getArg :: (HTML -> PCFun) -> PCFun
getArg k s l ls = getArgf1 '{' '}' k s l ls

getArg2 :: (HTML -> HTML -> PCFun) -> PCFun
getArg2 f = getArg (\h1 -> getArg (\h2 -> f h1 h2)) 

getArg3 :: (HTML -> HTML -> HTML -> PCFun) -> PCFun
getArg3 f = getArg (\h1 -> getArg (\h2 -> getArg (\h3 -> f h1 h2 h3)))

getArgf1 :: Char -> Char -> (HTML -> PCFun) -> PCFun
getArgf1 open close k s l ls = getArgf2 (trim l) ls where
  getArgf2 "" [] = do putStr "EOF where arg expected\n"
                      delimiter "EOF" s "" [] 
  getArgf2 "" (l:ls) = getArgf2 (trim l) ls
  getArgf2 l@(c:cs) ls | c == open =
       catchd (reqDelim [close] HProtect (retArg k))
         s cs ls doChar 
                      | otherwise = k HEmpty s l ls

retArg :: (HTML -> PCFun) -> PCFun
retArg k s l ls = k (head (html s)) (s {html = tail (html s)}) l ls


-- This program reads in a configuration file as a argument.  If no
-- argument is given, the default is "config.html"

main = do configFile <- getConfig
          s <- parseConfig configFile
          s' <- processFiles s
          writeRefFile s'
          writeIndexFile s'

getConfig :: IO String
getConfig = return "html.config"

writeRefFile s = 
  case (refFile s) of
     "" -> return ()
     f  -> do putStr ("Writing reference file " ++ f ++ "\n")
              catch (writeFile f (concat (map fmtKV (newRefMap s))))
                    (\e -> do putStr ("Can't write ref file: " ++ f ++ "\n" ++ show e)
                              return ())
  where fmtKV (k,v) = k ++ "=" ++ v ++ "\n" 


writeIndexFile s = 
  case (indexFile s) of
     "" -> return ()
     f  -> do putStr ("Writing index file " ++ f ++ "\n")
              let cmd = case (lookup "indexHeader" (macdefs s)) of
                           Just _ -> macroExpand s "~indexHeader" ++ "\n"
                           _ -> ""
                  hdrs = concat (map (\h -> [HItem HEmpty, h, HEol])
	                             (reverse (indexHTML s)))
                  idx = htmlToString 
                          (HProtect [HCmd cmd, HList "item" (HProtect hdrs)])
              catch (writeFile f idx)
                    (\e -> do putStr ("Can't write index file: " ++ f ++ "\n")
                              return ())


-- parseConfig parses a configuration file to yield an initial state
-- An entry in the config file is a 'tag = value' pair.  Tags supported are:

-- aux = file            Add definitions in a latex .aux file
-- proganchors = file    Anchor the listed productions.  One per file line.
-- syntaxanchors = file  Anchor the listed functions.  One per file line.
-- htmldir = directory   Output directory
-- files = file1, file2, ...  Files to be processed.   Cumulative.
-- index = file          index file to be generated if given
-- refs = file           file resolving refs to files.
-- style = style         Set document style
-- ~mac = value          Macro definition
-- #comment              Only on separate lines.

parseConfig :: String -> IO IState 
parseConfig f = catch 
                  (do c <- readFile f 
                      foldM configLine initState (lines c))
                  (\e -> error ("Can't read configuration file " ++ f))

configLine s l | "#" `starts` l = return s
               | l == ""        = return s
               | otherwise =
 let (k,v) = parseKV l in
    case k of
     "aux" -> do putStr ("Reading aux file " ++ v ++ "\n")
                 a <- readAuxFile v
                 return (s {auxInfo = auxInfo s ++ a})
     "proganchors" -> do a <- readAnchorFile v
                         return (s {panchors = panchors s ++ a})
     "files" -> return (s {sourceFiles = sourceFiles s ++ parseCommas v})
     "htmldir" -> return (s {htmlDir = v})
     "index" -> return (s {indexFile = v})
     "refs" -> do refs <- readRefFile v 
                  return (s {refFile = v, refMap = refs})
     -- Has to come after refs
     "morerefs" -> do morerefs <- readRefFile v 
                      return (s {refMap = refMap s ++ morerefs})
     "style" -> return (s {style = parseCommas v})
     '~':m  -> return (s {macdefs = (m,v) : macdefs s})
     _ ->  badline  where
       badline = error ("Bad line in config:\n" ++ l ++ "\n")

readRefFile :: String -> IO [(String, String)]
readRefFile f = catch (do l <- readFile f
			  length l `seq`	-- Ugh!  Make sure the file is
						-- completely read, because we are
						-- going to write to it at the end
                           return (map parseKV (lines l)))
                      (\e -> do putStr ("Can't read ref file: " ++ f ++ "\n")
                                return [])

readAuxFile :: String -> IO [(String,String)]
readAuxFile f = catch (do l <- readFile f
                          return (processAuxLines (lines l)))
                      (\e -> do putStr ("Can't read aux file: " ++ f ++ "\n")
                                return [])

readAnchorFile :: String -> IO [String]
readAnchorFile f = catch (do l <- readFile f
                             return (lines l))
                         (\e -> do putStr ("Can't read anchor file: "
                                            ++ f ++ "\n") 
                                   return [])

-- Look for \newlabel{label}{value} in aux files.  Ignore all else.

processAuxLines :: [String] -> [(String,String)]
processAuxLines [] = []
processAuxLines (l:ls) | "\\newlabel" `starts` l = f 10 3 ""
                       | "\\bibcite" `starts` l = f 9 2 "$"
                       | otherwise = rest
  where
    rest = processAuxLines ls
    f x1 x2 prefix = (prefix++name,val) : rest where
      dropNewLabel = drop x1 l  -- kills \newlabel{ or \bibcite{
      (name,r) = break (== '}') dropNewLabel
      r' = drop x2 r
      val = takeWhile (/= '}') r'


parseKV l = let (k,l1) = span (/= '=') l
                val    = case l1 of
                           ('=':v) -> trim v
                           _       -> ""
              in (trimr k,val)

parseCommas :: String -> [String]
parseCommas "" = []
parseCommas l = let (tok,l') = break (== ',') l in
                  filter (/= ' ') tok :
                    (case l' of "" -> []
                                (_:l'') -> parseCommas l'')


-- Data Defintions

-- Current mode in Latex source

data IState = IState { sourceFiles :: [String],
                       auxInfo :: [(String,String)],
                       panchors :: [String],
                       sanchors :: [String],
                       anchorMode :: Bool,
                       htmlDir :: String,
                       sourceFile :: String,
                       indexFile :: String,
                       indexHTML :: [HTML],
                       verbMode :: Bool,
                       section :: [String],
                       avoidSectionBump :: Bool,
                       inMath :: Bool,
                       inQuotes :: Bool,
                       refFile :: String,
                       refMap :: [(String,String)],
                       newRefMap :: [(String,String)],
                       style :: [String],
                       macdefs :: [(String,String)]}
  deriving Show

initState = IState { sourceFiles = [], auxInfo = [], panchors = [],
                     sanchors = [], anchorMode = False,
                     htmlDir = "", sourceFile = "", indexFile = "",
                     indexHTML = [],
                     verbMode = False, section = [], 
                     avoidSectionBump = False, inQuotes = False,
                     inMath = False, refFile = "", refMap = [], newRefMap = [],
                     style = ["article"], macdefs=[]}

-- This is the `changable' part of the state.  Save updates by pushing
-- global stuff into the `gs' field. 

data State = State {html :: [HTML],
                    delims :: [String -> PCFun], 
                    gs :: IState}
  deriving Show

-- This is the initial state at the start of the file

initLS :: IState -> State
initLS s = State {gs = s, delims = [], html = []}

-- These control the formatting of syntax

processFiles :: IState -> IO IState
processFiles s = do --putStr (show s ++ "\n")
                    doFile (sourceFiles s) s "" where
  doFile [] s _        =  return s
  doFile (file:fs) s p = do s' <- processFile (addp s p fs) file
                            doFile fs s' file
  addp s p fs = s {macdefs = ("prev",fname p) : 
                             ("next",case fs of (f:_) -> fname f; _ -> "") :
                             macdefs s}
  fname f = let (file,_) = parseFileName f in file

processFile :: IState -> String -> IO IState
processFile s f = let (file,ext) = parseFileName f
                      s' = s {sourceFile = file, verbMode = ext == ".verb"}
                      outFile = htmlDir s ++ file ++ ".html" in
                   do
                     catch
                      (do putStr ("Reading " ++ f ++ "\n")
                          l <- readFile f
                          (html,s'') <- texToHtml s' (lines l)
                          putStr ("Writing " ++ outFile ++ "\n")
                          catch (do writeFile outFile (htmlToString html)
                                    return s'')
                                (\e -> do putStr ("Write to " ++ outFile ++
                                                  " failed.\n")
                                          return s'))
                      (\e -> do putStr ("File " ++ outFile ++
                                        " error " ++ (show e) ++ "\n")
                                return s')

parseFileName f = let (re,rf) = span (/= '.') (reverse f) in
  case (re,rf) of
   (_,'.':f') -> (reverse f','.':(reverse re))
   _ -> (f,"")



-- This sets up the parts of the state that need to be reset at the start of
-- each file.

texToHtml :: IState -> [String] -> IO (HTML, IState)
texToHtml s ls = do (s',_,_) <- catchd
                                 (reqDelim1 "EOF" HProtect
                                  (\s l ls -> return (s,l,ls)))
                                 (initLS s) "" ls doChar
--                    print (html s')
                    return (HProtect (html s'),gs s')

-- This is the main loop for scanning each line of the input file.

scanLines :: State -> [String] -> PC
scanLines s [] = delimiter "EOF" s "" []
scanLines s (l:ls) | htmlLine l = handleCommand s l ls
--                   | l == "@" && verbMode (gs s) = doCodeLines s ls
                   | l == "@@@" && verbMode (gs s) = doSyntaxLines s ls
                   | texComment l = scanLines s ls
		   | l == "" = scanLines (HPara +++. s) (dropWhile (== "") ls)
	           | otherwise = doChar s l ls
  where
   -- Lines starting %* are commands to this preprocessor.
   htmlLine l = case l of 
                  ('%':'*':_) -> True
                  _ -> False
   texComment l = case l of ('%':_) -> True
                            _ -> False
 
-- This handles commands to this preprocessor endcoded as %* lines in the 
-- source.  

handleCommand s l ls | "*" `starts` c = 
                          scanLines (HCmd (macroExpand (gs s) (tail c) ++ "\n")
                                         +++ s) ls
                     | l == "%*ignore" = case 
					   dropWhile (/= "%*endignore") ls of
                        [] -> error "Missing %*endignore"
                        (l:ls) -> scanLines s ls 
                     | otherwise = do s' <- docmd htmlCommands
                                      scanLines s' ls
  where
    c = drop 2 l
    (cmd,args') = span isAlpha c
    args = trim args'
    docmd :: [(String,State -> String -> IO State)] ->
              IO State
    docmd [] = do putStr ("Bad preprocessor cmd: " ++ cmd ++ "\n")
                  putStr ("Args: " ++ args ++ "\n")
                  return s
    docmd ((c,fn):cs) | c == cmd = fn s args
                      | otherwise = docmd cs


-- Worlds stupidest macro expansion

macroExpand s str = exp1 str where
  exp1 "" = ""
  exp1 ('~':str) = let (n,r) = span isAlpha str
                       d = lookup n (macdefs s) in
                     case d of
                       Just v -> exp1 (v++r)
                       Nothing -> "Missing var " ++ n ++ "\n" ++ exp1 r 
  exp1 (c:str) = c : exp1 str

-- These are handlers for the special commands to this preprocessor (%*).

htmlCommands :: [(String, State -> String -> IO State)]
htmlCommands = [("anchor",doAddAnchors),
                ("section",doSetSection),
                ("dump", doDump)]

-- These are the handlers for the commands.  They return a new state and
-- some html

-- %*anchor on       adds designated anchors to syntax and source code
-- %*anchor off

doAddAnchors :: State -> String -> IO State
doAddAnchors s a = return (s {gs = (gs s) {anchorMode = a == "on"}}) 

-- %*section n       sets current section to n (can't set subsections)
--                   care is needed to avoid bumping the section
--                   on the next \section.

doSetSection :: State -> String -> IO State
doSetSection s a = return
                    (s {gs = (gs s) {section = s1,
                                     avoidSectionBump = not (null s1)}})
   where s1 = if null a then [] else [a]

-- For debugging

doDump :: State -> String -> IO State
doDump s a = do putStr ("Syntax anchors: \n" ++ 
                           concat (map (++ " ") (sanchors (gs s))))
                putStr ("Code Anchors:\n" ++
                         concat (map (++ " ") (panchors (gs s))))
                putStr ("Refs:\n" ++ 
                         concat (map (\a -> show a ++ " ") (refMap (gs s))))
                return s

-- This handles lines in verbatim mode as initiated by a single @ on a line.
-- Don't mix line-by-line verbatim and within a line verbatim!

-- This is where Haskell definitions are anchored

doCodeLines s (l:ls) | l == "@" = scanLines s ls
                     | otherwise = do s' <- emitCodeLine s l
                                      doCodeLines s ls

emitCodeLine s l | anchorMode (gs s) =
                     case maybeAnchor l of
                        Nothing -> return (HVerbLine l +++ s)
                        Just anc ->
                          let labs = entityToLabels anc
                              a = HProtect (map HDef labs ++ [HVerbLine l]) in 
                            do s' <- registerAnchors labs s
                               return (a +++ s')
                 | otherwise = return (HVerbLine l +++ s)

emitCodeLines s [] = return s
emitCodeLines s (l:ls) = do s' <- emitCodeLine s l
                            emitCodeLines s' ls

maybeAnchor :: String -> Maybe HaskellDef

maybeAnchor l | "data " `starts` l = Just (HTycon (dataName (drop 5 l)))
              | "newtype " `starts` l = Just (HTycon (dataName (drop 8 l)))
              | "type " `starts` l = Just (HTycon (dataName (drop 5 l)))
              | "class " `starts` l = Just (HTycon (dataName (drop 6 l)))
              | "instance " `starts` l = Just (anchorInstance (drop 9 l))
              | not (null l) && (not (isSpace (head l))) =
                     maybeAnchorSignature l
              | otherwise = Nothing

dropContext l = trim (dc1 l l) where
  dc1 ('=' : '>' : l') l = l'
  dc1 [] l = l
  dc1 (_:l1) l = dc1 l1 l

dataName s = getTok (trim (dropContext s))

-- Totally Bogus!

anchorInstance s = let s' = trim (dropContext s)
                       (c,s'') = break isSpace s'
                       s1 = trim s''
                       t = case (getTok s1) of
                              "(IO" -> "IO"
                              "(a->b)" -> "a->b"
                              "[]" -> "[a]"
                              x -> x   in
                     HInstance c t

maybeAnchorSignature s = case parseCL s of
                           ([],_) -> Nothing
                           (names,r) ->
                             case (trim r) of
                               (':' : ':' : c : _) ->
                                 if isSpace c then
                                     Just (HVars names)
                                     else Nothing
                               _ -> Nothing          

parseCL ('(':s) = parseOp s
parseCL s@(c:_) | isAlpha c = let (var,s') = span (\c -> isAlpha c ||
                                                         c == '_' ||
                                                         isDigit c ||
                                                         c == '\'') s
                                  (v,r) = parseCL1 (trim s')  in
                                (var:v,r)
                | otherwise = ([],"")
                                    
parseOp s = let (op,s') = break (== ')') s in
               case s' of
                 (')':s) -> let (v,r) = parseCL1 s in (op:v,r)
                 _ -> ([],"")

parseCL1 (',':s) = parseCL (trim s)
parseCL1 s = ([],s)

-- This handles lines of syntax definition

syntaxCols :: [Int]
syntaxCols = [100,20,250]

doSyntaxLines :: State -> [String] -> PC
doSyntaxLines s ls = do (hs, gs', ls') <- doSyntaxLine (initLS (gs s)) ls
                        let h' = HTable "cellspacing=0 cellspacing=0"
                                        "" syntaxCols (HProtect hs)
                        scanLines (s {gs = gs' {inMath = inMath (gs s)},
                                      html = h' : html s}) ls'
                                 
doSyntaxLine s []     = error "Eof in syntax mode!"
doSyntaxLine s (l:ls) | l == "@@@" = return (html s, gs s, ls)
                      | emptyLine l = doSyntaxLine (HLineBreak +++ s)
                                                   (dropWhile emptyLine ls)
                      | definitionLine l = defLine s l ls
                      | otherwise = continuedLine s l ls

emptyLine "" = True
emptyLine ('%':_) = True
emptyLine _ = False

definitionLine l = case (trim l) of
                          ('|':cs) -> False
                          _ -> True

-- This handles lines defining Haskell syntax (@@@).  These lines either
-- start with a non-terminal or a |.  This determines which:

continuedLine s l ls = do (hs,gs) <- texToHtml ((gs s) {inMath = True})
                                               ["& @|@ &" ++ l']
                          let h' = unProtect hs ++ [HLineBreak]
                          doSyntaxLine (s {gs = gs, html = html s ++ h'}) ls 
  where
      l' = tail (trim l)  -- trim the leading |

defLine s l ls = do (hs,gs) <- texToHtml ((gs s) {inMath = True})
                                      [tok ++ tok' ++ 
                                       " & " ++ arrow ++ l']
                    let gs' = if doAnchor then
                                 removeSyntaxAnchor tok gs else
                                 gs
                        h' = a ++ unProtect hs ++ [HLineBreak]
                    doSyntaxLine (s {gs = gs', html = html s ++ h'}) ls 
   where
      doAnchor = anchorMode (gs s) && tok `elem` sanchors (gs s)
      (tok,rest) = span isAN l
      (tok',r') = break isSpace rest
      (_,r'') = span isSpace r'
      (l',hasArrow) = case r'' of ('-':'>':cs) -> (cs, True)
                                  cs -> (cs, False)
      arrow | hasArrow = " {\\tt \\rightarrow} &"
            | otherwise = ""
      a = if doAnchor then [HAnchor ("syntax-" ++ tok) HEmpty] else []

removeSyntaxAnchor tok gs = gs          -- Needs to be fixed!

unProtect (HProtect x) = unProtect' x
unProtect x = [x]

unProtect' [HProtect x] = unProtect' x
unProtect' x = x


-- This is the main character dispatcher

doChar :: PCFun
doChar s "" ls = scanLines (HEol +++ s) ls  -- end of line
doChar s (c:cs) ls =
    case c of
     '@' | verbMode (gs s) 
         -> case cs of 
              ('@':cs') -> put "@" cs'
              _ -> do (s',l',ls') <- doVerb s cs ls
                      doChar s' l' ls'
     '%' -> scanLines s ls  -- Tex comment 
     '\\' -> doTexCommand s cs ls 
     '$' -> doChar (setMath s (not mathMode)) cs ls
     '"' | verbMode (gs s)
         -> if inQuotes (gs s) then
               delimiter "\"" (s {gs = (gs s) {inQuotes = False}}) cs ls
                               else
               catchd (reqDelim "\"" (\h -> HProtect (HFont IT:h)) 
                                (\s -> doChar (setMath s mathMode)))
                      (s {gs = (gs s) {inMath = True, inQuotes = True}})
                      cs ls doChar
     '~' -> put " " cs
     '_' -> if mathMode then subsuper HSub else put "_" cs
     '^' -> if mathMode then subsuper HSuper else put "^" cs
     '\'' -> case cs of
               ('\'':cs') -> put "\"" cs'
               _ -> put "'" cs
     '`' -> case cs of
               ('`':cs') -> put "\"" cs'
               _ -> put "`" cs
     '{' -> catchd (reqDelim "}" HProtect doChar) s cs ls doChar 
     '&' -> emit HSep s cs ls
     '}' -> delimiter "}" s cs ls
     _ -> let (l',l'') = break (`elem` "@%\\$\"~_^'`{&}") cs in
            put (c:l') l'' 
  where
    mathMode = inMath (gs s)
    put str r = emit (HString str) s r ls
    put' h r = emit h s r ls
    subsuper fn = getArg (doSuper fn) s (addBraces cs) ls
    doSuper fn h s l ls = emit (fn h) s l ls
    addBraces "" = ""
    addBraces (s@('{':cs)) = s
    addBraces (c:cs) = '{':c:'}':cs   -- Won't catch x^\foo

popMath s = doChar (setMath s False)

setMath s b = s {gs = (gs s) {inMath = b}} 

doTexCommand s l ls = 
     let (cmd,args) = parseTexCommand l
         doISO = emit (HISO (drop 3 cmd)) s args ls
         badCmd = do putStr ("Tex command " ++ cmd ++ " not recognized\n")
                     doChar s args ls
 in
      if "ISO" `starts` cmd then doISO else
       case lookup cmd texCommands of
        Just hndlr -> hndlr s (trim args) ls
        Nothing -> badCmd

parseTexCommand "" = ("","")
parseTexCommand cs | isAlpha (head cs) =
                       let (t,a) = span (\c -> isAlpha c || isDigit c ||
                                               c == '*') cs in
                         (t, a)
                   | otherwise = 
                       ([head cs],tail cs)

-- This handles verbatim mode.  This should not be handling code mode
-- which starts with a single @ on a line.


doVerb s "" [] = error "End of file in verb mode"
doVerb s "" (l:ls) = doVerb (HVerb "\n" +++ s) l ls
doVerb s ('@':'@':l) ls = doVerb (HVerb "@" +++ s) l ls
doVerb s ('@':l) ls = return (s, l, ls)   -- Exit verb mode
doVerb s l ls = doVerb (HVerb l' +++ s) l'' ls
  where
    (l',l'') = span (/= '@') l

--- Indexing stuff

maybeIndex :: State -> String -> HTML -> IO (String, State)
maybeIndex s sect title = 
 let tag = "sect" ++ sect in
  case (indexFile (gs s)) of
    "" -> return (tag,s)
    _  -> let h = HAnchor (sourceFile (gs s) ++ ".html#" ++ tag) title in
            return (tag, s {gs = (gs s) {indexHTML = h : indexHTML (gs s)}}) 

{-

inTable s = case tableStuff s of
               Nothing -> False
               _ -> True


-- 
    let Just (a,i,m) = tableStuff s
        s' = setTableStuff (Just (a,i+1,m)) s in
     ("</td><td " ++ encodeAlign (a !! (i+1)) ++ ">")
            ++. doChar s' cs ls
  | inTable s && c == '\\' && "\\" `starts` cs =
    let Just (a,_,m) = tableStuff s
        s' = setTableStuff (Just (a,0,m)) s in
      ("</td></tr><tr><td" ++ encodeAlign (a !! 0) ++ ">") ++.
              doChar s' (skipBracket (tail cs)) ls

  | mathMode s = doMChar 
    case c of
     '$' -> exitMath
-}


-- This strips off latex command args.  Not robust at all!  Should do a 
-- deeper parse.  Missing arguments are ignored.

ignore1 :: Char -> Char -> Int -> String -> [String] -> (String,[String])
ignore1 opn cls k l ls = ignore2 k l ls 0
  where
   ignore2 0 l ls lev = (l,ls)
   ignore2 k "" [] lev = ("",[])
   ignore2 k "" (l:ls) lev = ignore2 k l ls lev
   ignore2 k (c:cs) l lev | c == opn             = ignore2 k cs l (lev+1)
                          | c == cls && lev == 0 = (cs,l) -- Error!
                          | c == cls && lev == 1 = ignore2 (k-1) cs l 0
                          | c == cls             = ignore2 k cs l (lev-1)
                          | isSpace c            = ignore2 k cs l lev
                          | lev == 0             = (c:cs,l)
                          | otherwise            = ignore2 k cs l lev 

-- This is a really poor parser for simple string args to commands.
-- Assume (for now) that no line breaks are in the argument

getSArg :: String -> [String] -> (String,String,[String])
getSArg l ls = getSArg2 '{' '}' l ls

getBArg :: String -> [String] -> (String,String,[String])
getBArg l ls = getSArg2 '[' ']' l ls

getSArg2 open close s ls = (s1, s2, ls) where
    (s1,s2) = case trim s of
       (c:cs) -> if c == open then b cs 0 else ("",c:cs)
       str -> ("",str)
    b "" i = ("","")
    b (c:cs) i | c == open = cons1 open (b cs (i+1))
    b (c:cs) 0 | c == close = ("",cs)
    b (c:cs) i | c == close = cons1 close (b cs (i-1))
    b (c:cs) i = cons1 c (b cs i)
    cons1 c (cs,x) = (c:cs,x)

starts [] _ = True
starts _ [] = False
starts (a:as) (b:bs) | a == b = starts as bs
                     | otherwise = False

alternate :: [a] -> a -> [a]
alternate [] _ = []
alternate [l] _ = [l]
alternate (l:ls) a = l : a : alternate ls a

trim s = dropWhile isSpace s
trimr s = reverse (dropWhile isSpace (reverse s))

getTok s = takeWhile (\c -> not (isSpace c)) s

isReport s = "report" `elem` style (gs s) 

isArticle s = "article" `elem` style (gs s)

useMSSymbols s = "microsoftsymbols" `elem` style (gs s) 

sectStyle s | isReport s = 1
            | isArticle s = 0
            | otherwise = error "Bad style"

(.|.) :: PCFun -> PCFun -> PCFun
f1 .|. f2 = \s -> if useMSSymbols s then f1 s else f2 s

---   Handlers for Tex commands

texCommands :: [(String,PCFun)]
texCommands = [("\\",doEol),
               ("newline",doEol),
               ("",ignore 0),
               ("-",ignore 0),
               ("markboth",ignore 2),
               ("begin",doBegin),
               ("end",doEnd),
               ("item",doItem),
               ("bibitem",doBibItem),
               ("Large",emit (HSize 4)),
               ("bf",emit (HFont Bold)),
               ("tt",emit (HFont TT)),
	       ("vspace",ignore 1),
	       ("em",emit (HFont IT)),
	       ("cite",doCite),
	       ("noindent",ignore 0),
               ("Haskell",use "Haskell "),
               ("chapter",doSection 1),   -- only in report 
               ("chapter*",embed (HHdr 1 "")),   -- only in report 
               ("section",\s -> doSection (1+sectStyle s) s),
               ("section*",\s -> embed (HHdr (2+sectStyle s) "") s),
               ("subsection",\s -> doSection (2+sectStyle s) s),
               ("subsection*",\s -> embed (HHdr (3+sectStyle s) "") s),
               ("subsubsection",\s -> doSection (3+sectStyle s) s),
               ("subsubsection*",\s -> embed (HHdr (3+sectStyle s) "") s),
               ("subsubsubsection",\s -> doSection (4+sectStyle s) s),
	       ("paragraph",embed (HHdr 3 "")),
	       ("paragraph*",embed (HHdr 3 "")),
	       ("label",doLabel),
	       ("ref",doRef),
	       ("sref",\s -> doRef (HString "Section " +++ s)),   -- mpj
               ("bi",doBeginItemize),
               ("ei",delimiter "itemize"),
               ("bto",doBeginTabularB),    -- has a border
               ("eto",delimiter "tabular"),
               (">",emit HSep),
               ("=",emit HSep),
               ("kill",ignore 0),
               ("underline",embed (\h -> HProtect [HFont IT, h])),
               ("`",ignore 0),
               ("'",ignore 0),
               ("\"",ignore 0),	-- Ignore accents
               ("#",use "#"),
               ("%",use "%"),
               ("frac", doFrac),
               ("emptystr", use "[]"),
               ("ignorehtml",ignore 1),
	       ("bprog",emit (HVerb "\n")),
	       ("bprogNoSkip",ignore 0),
	       ("eprog", emit (HVerb "\n")),
	       ("eprogNoSkip",ignore 0),
	       ("par",ignore 0),
               ("footnote",bracket " (" ")"),
               ("bkqA",use "`"),
               ("bkqB",use "`"),
               ("bkq",use "`"),
               ("protect",ignore 0),
               ("qquad",emit (HVerb "  ")),
               ("mbox",ignore 0),
	       ("anchor",doAnchor),
               ("outline",embed boxed),
               ("outlinec",embed (HCenter . boxed)),
	       ("ecaption",embed (\a -> (HCenter (HHdr 3 "" a)))),
               ("nopagebreak",ignoreB),
               ("rm",embed (inFont RM)),
               ("it",emit (HFont IT)),
               ("bf",emit (HFont Bold)),
               ("input",doInput),
	       ("[",doEqn),
               ("]",delimiter "\\]"),
               (" ",use " "),
               ("O",use "O"),
               ("tau",use "t"),
               ("verb",doVerbc),
               ("ba",doBa "ea"),
               ("bt",doBa "et"),
               ("ea",delimiter "ea"),
               ("et",delimiter "et"),
               ("rangle",use ">"),
               ("langle",use "<"),
               ("tr",embed (inFont RM)),
	       ("{",use "{"),
	       ("}",use "}"),
	       ("index",ignore 1),
	       ("indexclass",ignore 1),
	       ("indexdi",ignore 1),
               ("indexsyn",ignore 1),
               ("indextt",ignore 1),
               ("indexmodule",ignore 1),
               ("indextycon",ignore 1),
               ("indexsynonym",ignore 1),
               ("arity",expandS  (\a -> "{\\rm arity(}" ++ a ++ "{\\rm )}")),
               ("infix",expandS  (\a -> "{\\rm infix(}" ++ a ++ "{\\rm )}")),
               ("nopagebreak",ignoreB),
               ("struthack",ignore 1),
               ("hline",ignore 0),
               ("centerline",ignore 0),
               ("hspace*",ignore 1),
               ("pageref",ignore 1),
               ("/",ignore 0),
               (",",use " "),
               ("kappa",emitSym "k"),
               ("overline",embed (\h -> HProtect [HFont UL, h])),
               ("multicolumn",ignore 2),
               ("pi",emitSym "p"),
               ("epsilon",emitSym "e"),
               ("prime",use "'"),
               ("clearpage",ignore 0),
               ("medskip",ignore 0),
               ("syn",bracket "[" "]"),
               ("S",use "report section "),
               ("newpage",ignore 0),
               ("_",use "_"),
               ("epsbox",ignore 1),
               ("caption",ignore 1),
               ("hspace",ignore 1),
               ("cleardoublepage",ignore 0),
               ("bprogB",ignore 0),
               ("eprogB",ignore 0),
               ("startnewsection",ignore 0),
               ("inputHS",doInputHS),
               ("pagenumbering",ignore 1),
               ("pagestyle",ignore 1),
               ("setcounter",ignore 2),
               ("LaTeX",use "LaTeX"),
               ("copyright", emit (HISO "copy")),
               ("newblock", ignore 0),
               ("figcaption", doFigCaption),
-- For the tutorial:
               ("see",doSee),
-- For mpj:
	       ("in", emitSymc 8712),
               ("ts",ignore 0),
               ("la",use "<"),
               ("ra",use ">"),
               ("prei",ignore 0),
               ("nextitem",doItem), 
               ("smon",ignore 0),
               ("smoff",ignore 0),
               ("metaArg",embed (\h -> HProtect [HFont RM, h])),
               ("inbox",embed boxed),
               ("clarg",getArg3 (\h1 h2 h3 -> emit (
                                 boxed (HProtect [
                                         inFont IT h2,
                                         HString " ",
                                         inFont RM h1])))),
               ("cls",ignore 2),
               ("cldone",ignore 0),
               ("helponly",ignore 1),
               ("htmlonly",ignore 1),
               ("latexonly",ignore 1),
               ("latexignore",ignore 1),
               ("helprefn",getArg2 (\h1 h2 -> emit h1)),
               ("parag",embed (HHdr 3 "")),
               ("showURL",getArg2 (\h1 h2 -> 
                           emit (HProtect [HProtect [HFont TT,h1],h2]))),
               ("mailto",getArg (\h -> 
                          emit (HProtect [HFont TT,h]))),
-- For Haskore
               ("extended",ignore 0),
               ("basic", ignore 1),
               ("small", ignore 0), -- No need for small print in this version

               ("bot",emitSym "^" .|. use "_|_"),
	       ("ne",emitSymc 185 .|. use "/="),
               ("cdots",emitSymc 188 .|. use "..."),
               ("red",emitSymc 222 .|. use "=>"),
               ("neq",emitSymc 185 .|. use "/="),
               ("equiv",emitSymc 186 .|. use "="),
               ("Rightarrow",emitSymc 222 .|. use "=>"),
               ("rightarrow",emitSymc 174 .|. use "->"),
               ("ast",emitSym "*" .|. use "*"),
               ("forall",emitSym "\"" .|. use "forall "),
               ("geq",emitSymc 179 .|. use ">="),
               ("leq",emitSymc 163 .|. use "<="),
               ("ldots",emitSymc 188 .|. use "..."),
               ("geq2",emit (HProtect [symFont [HSpecial 179],HString "2"])
                     .|. use ">=2"),
               ("geq0",emit (HProtect [symFont [HSpecial 179],HString "0"])
                     .|. use ">=0"),
               ("hprime", embed $ \h -> HProtect [prime_font, h])
  ]

prime_font = HColor "#6600CC"

use :: String -> PCFun
use str = emit (HString str)

symFont :: [HTML] -> HTML
symFont x = HProtect (HFont Sym : x)

emitSym :: String -> PCFun
emitSym s = emit (symFont [HString s])

emitSymc :: Int -> PCFun
emitSymc c = emit (symFont [HSpecial c])

ignore k s l ls= let (l',ls') = ignore1 '{' '}' k l ls in
                   doChar s l' ls'

ignoreB s l ls = let (l',ls') = ignore1 '[' ']' 1 l ls in
                  doChar s l' ls'

bracket s1 s2 = getArg (\h -> emit (HProtect [HString s1, h, HString s2]))

embed :: (HTML -> HTML) -> PCFun
embed hf = getArg (\h -> emit (hf h))

withArg :: (HTML -> State -> IO (HTML, State)) -> PCFun
withArg f = getArg (\h s l ls -> do (h',s') <- f h s
                                    emit h' s' l ls)

withBArg :: (String -> State -> IO (HTML, State)) -> PCFun
withBArg f s l ls = let (arg,l',ls') = getBArg l ls in
                      do (h,s') <- f arg s 
                         emit h s' l' ls'

expandS :: (String -> String) -> PCFun
expandS f s l ls = let (str,l',ls') = getSArg l ls in
  doChar s (f str ++ l') ls'

boxed = HTable "border=2 cellpadding=3" "l" []

inFont f h = HProtect [HFont f, h]

doEol s l ls = let (l',ls') = ignore1 '[' ']' 1 l ls in
                 emit HLineBreak s l' ls'

doBegin s l ls = let (a,l',ls') = getSArg l ls in
  if a == "tabular" || a == "table" then
     let (d,l'',ls'') = getSArg l' ls' in
         catchd (reqDelim a (doBlock a d) doChar) s l'' ls'' ignoreB
  else if a == "thebibliography" then  -- Just ignore it
     let (_,l'',ls'') = getSArg l' ls' in
       doChar s l'' ls''
  else if a == "verbatim" then doVerbatim  s l' ls' "\\end{verbatim}" 
  else if a == "code" then doVerbatim  s l' ls' "\\end{code}" 
  else catchd (reqDelim a (doBlock a "") doChar) s l' ls' ignoreB
   
doBeginItemize s l ls = 
    catchd (reqDelim "itemize" (doBlock "itemize" "") doChar) s l ls doChar 

doBeginArray s l ls = 
  let (d,l',ls') = getSArg l ls in
    catchd (reqDelim "array" (doBlock "array" d) doChar) s l' ls' doChar 

doBeginTabular s l ls = 
  let (d,l',ls') = getSArg l ls in
    catchd (reqDelim "tabular" (doBlock "tabular" d) doChar) s l' ls' doChar 

doBeginTabularB s l ls = 
  let (d,l',ls') = getSArg l ls in
    catchd (reqDelim "tabular" 
       (doBlock "tabular" ("border=2;" ++ d)) doChar) s l' ls' doChar 

doVerbatim s "" [] e = error "End of file in verbatim mode"
doVerbatim s l ls@(l':ls') e | e `starts` l =
                                doChar s (drop 14 l) ls
                             | otherwise = 
                                doVerbatim (HVerb (l ++ "\n") +++ s) l' ls' e

doBlock a d hs = 
 let h = HProtect hs in
  case a of
    "itemize" -> HList "item" h
    "quote" -> HQuote h
    "flushright" -> HTable "" "r" [] h
    "center" -> HCenter h
    "enumerate" -> HList "enum" h
    "description" -> HList "description" h
    "figure" -> boxed h  -- What to do with figures??
    "figure*" -> boxed h  -- What to do with figures??
    "table" -> HCenter h
    "tabular" -> HTable opts d' [] h
    "tabbing" -> HTable opts d' [] h
    "exercise" -> HProtect [HPara,
                            HProtect
                               [HFont Bold, HString "Exercise", HLineBreak],
                            h,
                            HPara]
    "theorem" -> HProtect [HPara,
                            HProtect
                               [HFont Bold, HString "Theorem", HLineBreak],
                            h,
                            HPara]
    "axiom"   -> HProtect [HPara,
                            HProtect
                               [HFont Bold, HString "Axiom", HLineBreak],
                            h,
                            HPara]
    "haskellprime" -> HProtect [prime_font, h]
    _ -> HString ("Unknown begin: \\begin{" ++ a ++ "}\n")
 where (opts,d') = case (break (== ';') d) of
			(str,"") -> ("", str)
			(o,';':s) -> (o,s)

doEnd s l ls = let (a,l',ls') = getSArg l ls in
   if a == "thebibliography"
       then doChar s l' ls'
       else delimiter a s l' ls' 

doItem = withBArg (\tag s -> do (h,_) <- texToHtml (gs s) [tag]
                                return (HItem h,s))

doBibItem s l ls = let (_,l',ls') = getBArg l ls 
                       (tag',l'',ls'') = getSArg l' ls'
                       tag = "$" ++ tag'
                       ref = case lookup tag (auxInfo (gs s)) of
                               Just n -> n
                               _ -> tag'  in
   do s' <- registerAnchor tag s
      emit (HProtect [HDef tag,
                      HString ("[" ++ ref ++ "] ")]) s' l'' ls''

doVerbc s (delim:cs) ls = doChar (HVerb str +++ s) l' ls' where
   (str, l',ls') = takeDelim cs ls 
   takeDelim "" [] = error "EOF in \\verb"
   takeDelim "" (l:ls) = let (s',l',ls') = takeDelim l ls in
                           ("\n" ++ s',l',ls')
   takeDelim (c:cs) ls | c == delim = ("",cs,ls)
                       | c == '@' && safeHead cs == '@' =
                            let (s',l',ls') = takeDelim (tail cs) ls in
                              ('@':s',l',ls')
                       | otherwise = let (s',l',ls') = takeDelim cs ls in
                                          (c:s',l',ls')

safeHead (x:xs) = x
safeHead _ = '?'

doSection k = withArg (\sectName s ->
                         do 
                           let s' = maybeBump s k
                               sectNum = sectionNumber s'
                           if k<3 then putStr (sectNum ++ "\n") else return ()
                           let html = HProtect
                                       [HString sectNum, HVerb "  ", sectName]
                           (anchor,s'') <- maybeIndex s' sectNum html
                           s3 <- registerAnchor anchor s''
                           return (HHdr (k+1) anchor html,s3))

maybeBump s k = s {gs = mb1 (gs s)} where
  mb1 s | avoidSectionBump s = s {avoidSectionBump = False}
        | otherwise = s {section = bumpSect (section s) k}

bumpSect [] i = ["1"]
bumpSect (s:_) 1 = [incStr s]
bumpSect (s:ss) i = s : bumpSect ss (i-1)

incStr s = show (i+1) where
  i :: Int
  i = case reads s of
          [(j,_)] -> j
          _ -> 0

doLabel s l ls = let (a,l',ls') = getSArg l ls in
   do s' <- registerAnchor a s
      emit (HDef a) s' l' ls' 

registerAnchor name s =
   case refFile (gs s) of
    "" -> return s
    _ ->  return (s {gs = (gs s) {newRefMap = (name,sourceFile (gs s))
                                              : newRefMap (gs s)}})

registerAnchors names s =
   case refFile (gs s) of
    "" -> return s
    _ ->  return (s {gs = (gs s)
                             {newRefMap =
                                 map (\name -> (name,sourceFile (gs s))) names
                                 ++ newRefMap (gs s)}})

doRef s l ls = let (a,l',ls') = getSArg l ls
                   txt = lookup a (auxInfo (gs s))
                   file = lookup a (refMap (gs s))
                   anc x = case file of
                             Just fn -> HAnchor (fn ++ ".html#" ++ a) x
                             Nothing -> x
 in
     case txt of
       Just x -> emit (anc (HString x)) s l' ls'
       Nothing -> do putStr ("Ref not found: " ++ a ++ "\n") 
                     emit (HString "??") s l' ls'


doCite s l ls = let (a,l',ls') = getSArg l ls
                    args = map ("$"++) (parseCommas a)
                    refs = refMap (gs s)
                    aux = auxInfo (gs s)
                    anc a x = case lookup a refs of
                               Just fn -> HAnchor (fn ++ ".html#" ++ a) x
                               Nothing -> x
                    cites = alternate cites' (HString ", ")
                    cites' = map (\a -> case lookup a aux of
                                         Just r -> anc a (HString r)
                                         _ -> HString "??") args      
 in
   do sequence (map (\v -> case (lookup v aux) of
                              Nothing ->
                                 putStr ("Cite not found: " ++ tail v ++ "\n")
                              _ -> return ()) args)
      emit (HProtect ([HString "["] ++ cites ++ [HString "]"])) s l' ls'

doAnchor s l ls = let (url,l',ls') = getSArg l ls in
  embed (\a -> (HAnchor url a)) s l' ls'

doInput s l ls = let (f,l',ls') = getSArg l ls in
      do putStr ("Reading input " ++ f ++ "\n")
         newLines <- catch (do l1 <- readFile (f ++ (if verbMode (gs s)
                                                        then ".verb"
                                                        else ".tex"))
                               return (lines l1)) 
                         (\err -> do putStr ("Input error: " ++ show err)
                                     return [])
         doChar s "" (newLines ++ [l'] ++ ls')

doInputHS s l ls = let (f,l',ls') = getSArg l ls in
      do putStr ("Reading Haskell input " ++ f ++ ".hs\n")
         newLines <- catch (do l1 <- readFile (f ++ ".hs")
                               return (lines l1)) 
                         (\err -> do putStr ("Input error: " ++ show err)
                                     return [])
         s' <- emitCodeLines s newLines
         doChar s' l' ls'

doEqn s l ls = catchd (reqDelim "\\]"
                         (\h -> HProtect ([HPara] ++ h ++ [HPara])) popMath)
                      (setMath s True) l ls doChar

-- Pretty awful rendition of frac.

doFrac = getArg2 (\d n -> emit (HProtect [HString "",
                                          n,
                                          HString "/(",
                                          d,
                                          HString ") "]))

doFigCaption = getArg (\c s l ls ->
                        let (lab,l',ls') = getSArg l ls
                            fignum = case lookup lab (auxInfo (gs s)) of
                                        Just l -> l
                                        Nothing -> "??"
                           in
                             emit (HCenter (
                                   HProtect [
                                    HFont Bold,
                                    HString ("Figure " ++ fignum ++ ": "),
                                    c])) s l' ls')

-- As far as I can figure out, table elements are not in math mode
-- even when the table itself is encountered while in math mode (?)

doBa d s l ls = let (arg,l1,ls1) = getBArg l ls
                    (align,l2,ls2) = getSArg l1 ls1 in
                   catchd (reqDelim d 
                            (\h -> HTable "" align [] (HProtect h)) doChar)
                           s l2 ls2 doChar
   

sectionName s = b (section (gs s)) where
  b [] = ""
  b [x] = x ++ " "
  b (x:xs) = x ++ "." ++ b xs

doSee s l ls = let (ref,l',ls') = getSArg l ls
                   txt = lookup ref (auxInfo (gs s))
                   file = lookup ref (refMap (gs s))
                   anc x = case file of
                            Just fn ->
                              HAnchor ("../report/" ++ fn ++ ".html#" ++ ref) x
                            Nothing -> x
 in
     case txt of
       Just x -> emit (anc (HProtect [HISO "sect" ,HString x])) s l' ls'
       Nothing -> do putStr ("Ref not found: " ++ ref ++ "\n") 
                     emit (HString "??") s l' ls'


-- This is the HTML output phase


htmlToString :: HTML -> String
htmlToString h = emitRM h romanFont

-- The HTML state consists of the current HTML string, the current font,
-- the new font, and the newline flag.

type HTMLS = (String,Font,Font,Bool)

ht1 :: HTML -> String -> Font -> Font -> Bool -> HTMLS
ht1 h s cf nf@(Font style sz color) nl =
  case h of
   HProtect hs -> let hs' = rotateDefs hs
                      (s',cf',nf',nl') = emitl hs' s cf nf nl in
                    (s',cf',nf,nl')
   HString str -> hemit str
   HISO str -> hemitV ("&" ++ isoTrans str ++ ";")
   HPara -> ("<p>\n"++s,cf,nf,True)
   HCmd str -> huse str
   HEol -> huse "\n"
   HVerb str -> hemitTT str False
   HVerbLine str -> hemitTT (str ++ "\n") True
   HAnchor str h -> remit ("<a href=\"" ++ str ++ "\">") "</a>" h False
   HDef str -> huse ("<a name=\"" ++ str ++ "\"></a>")  
   HSub h -> remit "<sub>" "</sub>" h False
   HSuper h -> remit "<sup>" "</sup>" h False
   HCenter h -> remit "<div align=center>" "</div>" h True
   HQuote h -> remit "<blockquote>" "</blockquote>" h True
   HFont f -> newFont (Font f sz color)
   HColor color' -> newFont (Font style sz color')
   HSize i -> newFont (Font style i color)
   HSep -> hemit " -- Bad & -- "
   HLineBreak -> ("<br>\n"++s,cf,nf,True)
   HHdr i s h -> remit (addAnchor s ++ "<h" ++ show i ++ ">")
                       ("</h" ++ show i ++ ">")
                       h True
   HTable opts align' widths (HProtect hs) ->
    let align = filter (`elem` "lrc") align' in
     case parseTable hs of
        [] -> (s,cf,nf,nl)
        (r:rs) -> (exitFont cf ++
                   "<table " ++ opts ++ ">\n" ++
                    doTableRow align widths r ++
                    concat (map (doTableRow align []) rs)
                    ++ "</table>\n" ++ s,romanFont, nf,True)
   HItem h -> huse "Dangling \\item"
   HList style (HProtect h) -> 
      let hs = parseItems h
          items = concat (map singleItem hs)
          singleItem (HItem h:hs) =
             case style of
               "description" -> "<DT>" ++ emitRM h nf ++ "</DT>\n" ++ r
               _ -> "<LI>" ++ r
            where r = emitRM (HProtect hs) nf 
          singleItem _ = ""
          (start,end) = case style of     
                          "item" -> ("<UL>","</UL>")
                          "enum" -> ("<OL>","</OL>")
                          "description" -> ("<DL>","</DL>")
      in (exitFont cf ++ start ++ items ++ end ++ s, romanFont, nf, True)
   HEmpty -> (s,cf,nf,nl)
   HSpecial c -> hemitV ("&#" ++ show c ++ ";")
   _ -> error ("No emit for " ++ show h)
  where
     hemit str = (changeFont cf nf ++ htmlEncode str s, nf, nf, False)
     hemitV str = (changeFont cf nf ++ str ++ s, nf, nf, False)
     huse str = (str ++ s,cf,nf, False)
     remit s1 s2 h nl' =
       let (s',cf',nf',_) = ht1 h (exitFont cf' ++ s2 ++ s) romanFont nf nl in
        (exitFont cf ++ s1 ++ s', romanFont, nf ,nl')
     emitl [] s cf nf nl = (s, cf, nf, nl)
     emitl (h:hs) s cf nf nl = let (s1,cf1,nf1,nl1) = ht1 h sn cf nf nl
                                   (sn,cfn,nfn,nln) = emitl hs s cf1 nf1 nl1 in
                                (s1,cfn,nfn,nln)
     hemitTT str forceNL = (changeFont cf ttFont ++ 
                            (if forceNL && (not nl) then "<br>\n" else "") ++
                            htmlEncodeVerb str s,
                            ttFont, nf, forceNL)
     newFont f = (s,cf,f,nl)
     doTableRow align widths rows = "<tr>" ++
                             tr1 align ws rows ++
                             "</tr>"  where
          aligns = align ++ repeat 'l'
          ws :: [Int]
          ws = widths ++ repeat 0
     tr1 (a:as) = tr2 a as     -- default align = 'l' 
     tr1 []     = tr2 'l' []
     tr2 a as (w:ws) = tr3 a as w ws   -- default width = 0
     tr2 a as []     = tr3 a as 0 []
     tr3 a as w ws (r:rs) = tableRow a w r ++ tr1 as ws rs
     tr3 _ _  0 _  []     = []
     tr3 a as w ws []     = tableRow a w [HEmpty] ++ tr1 as ws []
 
     tableRow a w r =
                 "<td" ++ case a of
                          'l' -> ""
                          'c' -> " align=center"
                          'r' -> " align=right"
                          _   -> error ("Bad alignment: " ++ [a] ++ "\n")
                       ++ case w of
                           0 -> ""
                           n -> " width=" ++ show n
                       ++ ">" ++ emitRM (HProtect r) nf ++ "</td>"
 
     addAnchor "" = ""
     addAnchor s = "<a name=\"" ++ s ++ "\"></a>\n"

htmlEncode [] s = s
htmlEncode ('>':cs) s = "&gt;" ++ htmlEncode cs s
htmlEncode ('<':cs) s = "&lt;" ++ htmlEncode cs s
htmlEncode ('&':cs) s = "&amp;" ++ htmlEncode cs s
htmlEncode (c  :cs) s = c : htmlEncode cs s

htmlEncodeVerb [] s = s
htmlEncodeVerb ('>' :cs) s = "&gt;" ++ htmlEncodeVerb cs s
htmlEncodeVerb ('<' :cs) s = "&lt;" ++ htmlEncodeVerb cs s
htmlEncodeVerb ('&' :cs) s = "&amp;" ++ htmlEncodeVerb cs s
htmlEncodeVerb (' ' :cs) s = "&nbsp;" ++ htmlEncodeVerb cs s
htmlEncodeVerb ('\n':cs) s = "<br>\n" ++ htmlEncodeVerb cs s
htmlEncodeVerb (c   :cs) s = c : htmlEncodeVerb cs s

parseTable :: [HTML] -> [[[HTML]]]
parseTable hs = map (\r -> splitList r isHSep)
                    (splitList hs isHLineBreak)

isHSep HSep = True
isHSep _ = False
 
isHLineBreak HLineBreak = True
isHLineBreak _ = False
 
splitList :: [a] -> (a -> Bool) -> [[a]]
splitList [] _ = []
splitList (l:ls) sep | sep l = [] : splitList ls sep
                     | otherwise = acons l (splitList ls sep) 

parseItems :: [HTML] -> [[HTML]]
parseItems [] = []
parseItems (h@(HItem _) : hs) = [] : acons h (parseItems hs)
parseItems (h : hs) = acons h (parseItems hs)

acons :: a -> [[a]] -> [[a]]
acons x [] = [[x]]
acons x (l:ls) = (x:l):ls

changeFont f1 f2 = if f1 /= f2 then exitFont f1 ++ enterFont f2 else ""

enterFont (Font s 3 "") = enterFontStyle s
enterFont (Font s sz color) = "<font " ++ fs sz ++ fc color ++ ">"
                              ++ enterFontStyle s
  where
     fs 3 = ""
     fs x = "size=" ++ show x ++ " "
     fc "" = ""
     fc c = "color=" ++ c

enterFontStyle RM = ""
enterFontStyle IT = "<I>"
enterFontStyle Bold = "<B>"
enterFontStyle TT = "<tt>"
enterFontStyle Sym = "<font face=\"symbol\">"
enterFontStyle UL = "<u>"

exitFont (Font s 3 "") = exitFontStyle s
exitFont (Font s _ _) = exitFontStyle s ++ "</font>" 
exitFontStyle RM = ""
exitFontStyle IT = "</I>"
exitFontStyle Bold = "</B>"
exitFontStyle TT = "</tt>"
exitFontStyle Sym = "</font>"
exitFontStyle UL = "</u>"

emitRM h f = let (s,cf,nf,nl) = ht1 h (exitFont cf) romanFont f True in s
                 
isAN x = isAlpha x || isDigit x

-- According to the lastest W3 stuff I have these names wrong.

isoTrans "brkbar" = "#166"
isoTrans "hibar" = "#175"
isoTrans "suptwo" = "#178"
isoTrans "supthree" = "#179"
isoTrans "supone" = "#185"
isoTrans "fracfourth" = "#188"
isoTrans "frachalf" = "#189"
isoTrans "fracthreeforths" = "#190"
isoTrans x = x 

sectionNumber s = b (section (gs s)) where
  b [] = ""
  b [s] = s
  b (s:s') = s ++ "." ++ b s'

-- This pulls labels in front of section headers.  In Tex you have
-- \section{blah} \label{blah} which is the wrong order for the html.

rotateDefs :: [HTML] -> [HTML]
rotateDefs defs = rd1 (reverse defs) [] where 
  rd1 [] a = a
  rd1 (d@(HDef _) : hs) a = let (headers,rest) = span isHeader hs in
          rd1 rest ([d] ++ reverse headers ++ a)
  rd1 (x:xs) a = rd1 xs (x:a) 
  isHeader HPara = True
  isHeader HEol = True
  isHeader (HHdr _ _ _) = True
  isHeader _ = False


--- These are the name manglers to map names with symbols onto alphas

entityToLabels :: HaskellDef -> [String]
entityToLabels (HTycon t) = ["$t" ++ mangleName t]
entityToLabels (HInstance c t) = ["$i" ++ mangleName c ++ "$$" ++
                                   mangleName t]
entityToLabels (HVars vars) = map (\v -> "$v" ++ mangleName v) vars

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
                              "(a->b)" -> "(->)"
                              "[a]" -> "[]"
                              x -> x)
