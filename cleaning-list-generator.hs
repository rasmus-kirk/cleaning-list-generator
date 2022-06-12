#!/usr/bin/env nix-shell
#!nix-shell -i runghc -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [ pandoc random-shuffle shh shh-extras ])"

-- Haskell script to generate dorm cleaning list

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}

import Shh

import Data.Default
import Data.ByteString.Lazy.Internal
import Data.Time.Calendar.WeekDate
import Data.Time.Clock
import Data.Time.Calendar

import System.Environment
import Control.Monad

import Data.List.Split

import System.Random.Shuffle

type RoomNumber = String
type Name = String
type Dorm = [(Name, RoomNumber)]
type Dates = [String]

load SearchPath ["pandoc"]

rotate :: Int -> [a] -> [a]
rotate  =  drop <> take

parseFile :: FilePath -> IO Dorm
parseFile filePath = do
	lines <- readLines filePath
	return $ map (pair . splitOn ",") lines
	where
		readLines :: FilePath -> IO [String]
		readLines = fmap lines . readFile

		pair :: [a] -> (a, a)
		pair [a,b] = (a,b)
		pair _ = error "ParseError"

getDates :: Int -> Int -> Integer -> [String]
getDates weekNumber weekCount year =
	let f y x = case fromWeekDateValid year x 7 of
	       Nothing -> f (year+1) (x-52)
	       Just a -> a
	    g = show . (f year)
	in map g [x | x <- [weekNumber..weekNumber+weekCount+1]]

dormsToTable :: Dorm -> Dorm -> Dates -> [String]
dormsToTable dormA dormB dates =
	let head = "Room (v.) | Name (v.) | Room (h.) | Name (h.) | Date \n"
	    seperator = "--|--|--|--|-- \n"
	    body = map (\(a,b) -> a ++ " | " ++ b ++ "\n") $ zip (zipWith (curry g) dormA dormB) dates
	in [head] ++ [seperator] ++ body
	where g ((a, b), (c, d)) =
		a ++ " | " ++ b ++ " | " ++ c ++ " | " ++ d

main = do
	args <- getArgs
	let year :: Integer
	    year = read $ args !! 0
	let week = read $ args !! 1

	dorm <- parseFile "./input-list.txt"
	dormShuffled <- shuffleM dorm
	let dormRotated = rotate (length dorm `div` 2) dormShuffled

	now <- getCurrentTime
	let (year, _, _) = toGregorian $ utctDay now
	let dates = getDates week (length dorm) year

	let table = dormsToTable dormShuffled dormRotated dates

	writeFile "./test.md" ""
	forM_ table (appendFile "./test.md")
	pandoc "test.md" "-o" "test.pdf"

