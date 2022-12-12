{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Arrow ((>>>))
import Control.Lens hiding (children)
import Data.Bifunctor (first)
import Data.Foldable (toList)
import Data.Generics.Product (field)
import Data.List (foldl', sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)

main :: IO ()
main = interact $ \input -> let
  commands = readCommands (fmap words (lines input))
  tree = climb (buildDirectoryTree commands)
  annotatedTree = annotateSizes (tree ^. field @"current")
  target = 30000000 - (70000000 - (annotatedTree ^. field @"files" . _1))
  bigEnough = sort (filter (>= target) (fmap fst (toList annotatedTree)))
  in show (getSum (head bigEnough))

readCommands :: [[String]] -> [Command]
readCommands (["$", "cd", p ] : cs) = let
  p' = case p of
    "/" -> Root
    ".." -> Parent
    x -> Relative x
  in Cd p' : readCommands cs
readCommands (["$", "ls"] : rest) = let
  (o, cs) = first (fmap readOutput) (break (\s -> head s == "$") rest)
  in Ls o : readCommands cs
readCommands [] = []
readCommands x = error ("Unrecognised command: " <> show x)

readOutput :: [String] -> Either (Directory [File]) File
readOutput ["dir", name] = Left (Directory name [] Map.empty)
readOutput [s, filename] = Right (File filename (read s))
readOutput x = error ("Unrecognised output: " <> show x)

buildDirectoryTree :: [Command] -> DirectoryTree
buildDirectoryTree = foldl' go root
  where
    root :: DirectoryTree
    root = ZipTree {current = Directory "/" [] Map.empty, parents = []}

    go :: DirectoryTree -> Command -> DirectoryTree
    go tree (Cd Root) = climb tree
    go tree (Cd Parent) = climbOne tree
    go ZipTree{current, parents} (Cd (Relative name)) =
      case Map.lookup name (current ^. field @"children") of
        Nothing -> let
          new = Directory
            { name = name
            , files = []
            , children = Map.empty
            }
          in ZipTree { current = new, parents = current : parents }
        Just child -> let
          parent = current & field @"children" %~ Map.delete name
          in ZipTree { current = child, parents = parent : parents }
    go tree (Ls output) = tree &
      field @"current" %~ ((field @"files" .~ []) >>> (\t -> foldr addOrUpdateChild t output))
      where
        addOrUpdateChild :: Either (Directory [File]) File -> Directory [File] -> Directory [File]
        addOrUpdateChild = either maybeAddDirectory addFile

        addFile :: File -> Directory [File] -> Directory [File]
        addFile f = field @"files" %~ (f :)

        maybeAddDirectory :: Directory [File] -> Directory [File] -> Directory [File]
        maybeAddDirectory child =
          field @"children" . at (child ^. field @"name")
            %~ (Just . fromMaybe child)

climb :: DirectoryTree -> DirectoryTree
climb t@ZipTree {parents = []} = t
climb t = climb (climbOne t)

climbOne :: DirectoryTree -> DirectoryTree
climbOne ZipTree {parents = []} = error "Can't climb above root"
climbOne ZipTree {current, parents = p : ps} = ZipTree
  { current = p & field @"children" %~ Map.insert (current ^. field @"name") current
  , parents = ps
  }

type DirectoryTree = ZipTree (Directory [File])

data ZipTree a = ZipTree
  { current :: a
  , parents :: [a]
  } deriving (Generic, Functor, Foldable, Traversable)

instance Show a => Show (ZipTree a) where
  show ZipTree{ current, parents } =
    "Tree with " <> show (length parents) <> " parents.\n" <> show current

data Directory a = Directory
  { name :: String
  , files :: a
  , children :: Map String (Directory a)
  } deriving (Generic, Functor, Foldable, Traversable)

instance Show a => Show (Directory [a]) where
  show Directory{name, files, children} =
    name <> ": " <> show (length files) <> " files, "
      <> show (Map.size children) <> " children\n"
      <> concatMap (show . snd) (Map.toList children)

annotateSizes :: Directory [File] -> Directory (Sum Integer, [File])
annotateSizes d@Directory{files, children, ..} =
  Directory { files = (size d, files)
            , children = fmap annotateSizes children
            , ..
            }

size :: Directory [File] -> Sum Integer
size (Directory{files, children}) =
  foldMap (^. field @"filesize" . to Sum) files
  <> foldMap size children

data Command
  = Cd Path
  | Ls [Either (Directory [File]) File]
  deriving (Generic)

instance Show Command where
  show (Cd p) = "$ cd " <> case p of
    Root -> "/"
    Parent -> ".."
    Relative f -> f
  show (Ls output) = "$ ls" <> unlines (fmap (either f show) output)
    where
      f :: Directory [File] -> String
      f d = "dir " <> d ^. field @"name"

data Path
  = Root
  | Parent
  | Relative String
  deriving (Generic, Show)

data File = File
  { filename :: String
  , filesize :: Integer
  }
  deriving (Generic, Show)
