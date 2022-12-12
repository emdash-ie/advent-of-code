{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Main where

import Control.Arrow ((>>>))
import Control.Lens hiding (children)
import Data.Bifunctor (first)
import Data.Fix (Fix(..))
import Data.Foldable (toList)
import Data.Generics.Product (field)
import Data.Generics.Sum (_Ctor)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(..))
import GHC.Generics (Generic)

main :: IO ()
main = interact $ \input -> let
  commands = readCommands (fmap words (lines input))
  tree = climb (buildDirectoryTree commands)
  annotatedTree = fmap (\d -> (d, size d)) tree
  _smallSizes = fmap snd (filter (\(_, n) -> n <= 100000) (toList annotatedTree))
  namedSizes = fmap (\(Fix d) -> (d ^. field @"name", size (Fix d))) tree
  in show namedSizes

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

readOutput :: [String] -> Either (Fix Directory) File
readOutput ["dir", name] = Left (Fix (Directory name [] Map.empty))
readOutput [s, filename] = Right (File filename (read s))
readOutput x = error ("Unrecognised output: " <> show x)

buildDirectoryTree :: [Command] -> DirectoryTree
buildDirectoryTree = foldl' go root
  where
    root :: DirectoryTree
    root = ZipTree {current = Fix (Directory "/" [] Map.empty), parents = []}

    go :: DirectoryTree -> Command -> DirectoryTree
    go tree (Cd Root) = climb tree
    go tree (Cd Parent) = climbOne tree
    go ZipTree{current, parents} (Cd (Relative name)) =
      case Map.lookup name (current ^. to unFix . field @"children") of
        Nothing -> let
          new = Directory
            { name = name
            , files = []
            , children = Map.empty
            }
          in ZipTree { current = Fix new, parents = current : parents }
        Just child -> let
          parent = current & _Ctor @"Fix" . field @"children" %~ Map.delete name
          in ZipTree { current = child, parents = parent : parents }
    go tree (Ls output) = tree &
      field @"current" %~ ((_Ctor @"Fix" . field @"files" .~ []) >>> (\t -> foldr addOrUpdateChild t output))
      where
        addOrUpdateChild :: Either (Fix Directory) File -> Fix Directory -> Fix Directory
        addOrUpdateChild = either maybeAddDirectory addFile

        addFile :: File -> Fix Directory -> Fix Directory
        addFile f = _Ctor @"Fix" . field @"files" %~ (f :)

        maybeAddDirectory :: Fix Directory -> Fix Directory -> Fix Directory
        maybeAddDirectory child =
          _Ctor @"Fix" . field @"children" . at (unFix child ^. field @"name")
            %~ (Just . fromMaybe child)

climb :: ZipTree a -> ZipTree a
climb t@ZipTree {parents = []} = t
climb t = climb (climbOne t)

climbOne :: ZipTree a -> ZipTree a
climbOne ZipTree {parents = []} = error "Can't climb above root"
climbOne ZipTree {current, parents = p : ps} = ZipTree
  { current = p & _Ctor @"Fix" . field @"children" %~ Map.insert (current ^. field @"name") current
  , parents = ps
  }

type DirectoryTree = ZipTree (Fix Directory)

data ZipTree a = ZipTree
  { current :: a
  , parents :: [a]
  } deriving (Generic, Functor, Foldable, Traversable)

instance Show a => Show (ZipTree a) where
  show ZipTree{ current, parents } =
    "Tree with " <> show (length parents) <> " parents.\n" <> show current

data Directory a = Directory
  { name :: String
  , files :: [File]
  , children :: Map String a
  } deriving (Generic, Functor, Foldable, Traversable)

instance Show a => Show (Directory a) where
  show Directory{name, files, children} =
    name <> ": " <> show (length files) <> " files, "
      <> show (Map.size children) <> " children\n"
      <> concatMap (show . snd) (Map.toList children)

size :: Fix Directory -> Sum Integer
size (Fix Directory{files, children}) =
  foldMap (^. field @"filesize" . to Sum) files
  <> foldMap size children

data Command
  = Cd Path
  | Ls [Either (Fix Directory) File]
  deriving (Generic)

instance Show Command where
  show (Cd p) = "$ cd " <> case p of
    Root -> "/"
    Parent -> ".."
    Relative f -> f
  show (Ls output) = "$ ls" <> unlines (fmap (either f show) output)
    where
      f :: Fix Directory -> String
      f (Fix d) = "dir " <> d ^. field @"name"

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
