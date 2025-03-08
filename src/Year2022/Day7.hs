module Year2022.Day7 (partOne, partTwo) where

import MeLude
import qualified Data.Map as M
import Util (readInt, safeHead, split, splitOnce, trim)

data CdTarget = TargetUp | TargetRoot | TargetDown !String
  deriving (Show)

data TerminalLine = LsLine | CdLine !CdTarget | FileLine !String !Int | DirLine !String
  deriving (Show)

data FileSystem = Directory !(M.Map String FileSystem) | File !String !Int
  deriving (Show)

parseLine :: String -> Either String TerminalLine
parseLine line@('$' : command) = case trim isSpace command of
  "ls" -> Right LsLine
  'c' : 'd' : args -> case trim isSpace args of
    ".." -> Right $ CdLine TargetUp
    "/" -> Right $ CdLine TargetRoot
    folder -> Right $ CdLine $ TargetDown folder
  _ -> Left $ "unknown command " <> line
parseLine ('d' : 'i' : 'r' : folder) = Right $ DirLine $ trim isSpace folder
parseLine line = case splitOnce ' ' line of
  Just (size, name) -> readInt size >>= Right . FileLine (trim isSpace name)
  Nothing -> Left $ "Failed to parse command " <> line

parse :: String -> Either String [TerminalLine]
parse input = split '\n' input <&> trim isSpace & traverse parseLine

fileSystemSize :: FileSystem -> Int
fileSystemSize (File _ size) = size
fileSystemSize (Directory entries) = M.assocs entries <&> snd <&> fileSystemSize & sum

insertEntry :: [String] -> (Maybe FileSystem -> FileSystem) -> FileSystem -> FileSystem
insertEntry [] new sys = new $ Just sys
insertEntry (next : rest) new sys@(File _ _) = sys
insertEntry [next] new (Directory entries) = Directory $ M.alter (Just . new) next entries
insertEntry (next : rest) new (Directory entries) = Directory $ M.adjust (insertEntry rest new) next entries

assembleFileSystem :: [TerminalLine] -> FileSystem
assembleFileSystem = snd . foldl' fold ([], Directory M.empty)
  where
    fold :: ([String], FileSystem) -> TerminalLine -> ([String], FileSystem)
    fold state LsLine = state
    fold (cwd, sys) (CdLine TargetUp) = (init cwd, sys)
    fold (cwd, sys) (CdLine TargetRoot) = ([], sys)
    fold (cwd, sys) (CdLine (TargetDown dir)) = (cwd ++ [dir], sys)
    fold (cwd, sys) (FileLine name size) =
      (cwd, insertEntry (cwd ++ [name]) (const $ File name size) sys)
    fold (cwd, sys) (DirLine name) = (cwd, insertEntry (cwd ++ [name]) (fromMaybe $ Directory M.empty) sys)

fileSystemEntries :: FileSystem -> [FileSystem]
fileSystemEntries file@(File _ _) = [file]
fileSystemEntries dir@(Directory children) =
  dir : (M.assocs children >>= fileSystemEntries . snd)

isDirectory :: FileSystem -> Bool
isDirectory (File _ _) = False
isDirectory (Directory entries) = True

smallDirectories :: FileSystem -> [FileSystem]
smallDirectories =
  fileSystemEntries
    >>> filter ((<= 100000) . fileSystemSize)
    >>> filter isDirectory

solvePartOne :: [TerminalLine] -> Int
solvePartOne = assembleFileSystem >>> smallDirectories >>> map fileSystemSize >>> sum

diskSize :: Int
diskSize = 70000000

updateSize :: Int
updateSize = 30000000

solvePartTwo :: [TerminalLine] -> Int
solvePartTwo lines =
  fileSystemEntries system
    & filter isDirectory
    <&> fileSystemSize
    & filter (>= needed)
    & sort
    & safeHead
    & fromMaybe 0
  where
    system = assembleFileSystem lines
    needed = updateSize - (diskSize - fileSystemSize system)

partOne :: String -> Either String String
partOne input = parse input <&> solvePartOne <&> show

partTwo :: String -> Either String String
partTwo input = parse input <&> solvePartTwo <&> show
