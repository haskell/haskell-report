module System ( 
    ExitCode(ExitSuccess,ExitFailure),
    getArgs, getProgName, getEnv, system, exitWith, exitFailure
  ) where

data ExitCode = ExitSuccess | ExitFailure Int 
                deriving (Eq, Ord, Read, Show)

getArgs 		:: IO [String]
getProgName 		:: IO String
getEnv        		:: String -> IO String
system        		:: String -> IO ExitCode
exitWith   		:: ExitCode -> IO a
exitFailure		:: IO a
