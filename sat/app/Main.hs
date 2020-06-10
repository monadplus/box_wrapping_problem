{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Main where

import           Control.Exception
import           Control.Monad     (when)
import           Data.Coerce
import           Data.Foldable     (traverse_)
import           Data.Functor
import           Data.List         (intercalate)
import           SAT               hiding (runSolver)
import           System.IO
import           System.Timeout    (timeout)
import           Text.Printf
import           Debug.Trace

main :: IO ()
main = do
  (rawStatement, statement) <- readStatement
  traverse_ putStrLn rawStatement
  r <- findSolution statement
  maybe (print "UNSAT") printBoxWrappingSolution r

data LengthError = LengthError
  deriving stock (Show)
  deriving anyclass (Exception)

findSolution :: S -> IO (Maybe BWPSolution)
findSolution = loop Nothing
  where
    loop acc s = do
      res <- timeout 4000000 $ runSolver s
      case res of
        Just (SAT (s', sol)) ->
          case translateSolution s' sol of

            -- Invalid output (e.g. [1])
            Nothing -> return acc

            Just bwp -> do
              let bestLength = min (s^.maxLength) (len bwp)
              loop (Just $ best acc bwp) (updateStatement (bestLength - 1) s)

        _ -> return acc


best :: Maybe BWPSolution -> BWPSolution -> BWPSolution
best Nothing x = x
best (Just bwp1) bwp2
  | len bwp2 < len bwp1 = bwp2
  | otherwise           = bwp1

runSolver :: S -> IO (Result MiosSolution)
runSolver initial = do
  let statement = runSAT buildSAT initial
      cnf = statement ^. clauses . to coerceCNF
      description = getDescription statement
  sol <- solveSAT description cnf
  if isSAT sol
    then return $ SAT (statement, sol)
  else
    return UNSAT

isSAT :: MiosSolution -> Bool
isSAT = not . null

updateStatement :: Int -> S -> S
updateStatement newMaxLen s =
  s & maxLength .~ newMaxLen
    & clauses .~ []

writeSAT :: S -> IO ()
writeSAT s =
  withFile "statement.cnf" WriteMode $ \h -> do
    hPutStrLn h $ "p cnf " ++ nvars ! nclauses
    traverse_ (hPrint h)
              (s^.clauses)
  where
    nclauses = lengthOf (clauses.traverse) s
    nvars = maximum . fmap abs . concat $ (coerce (s^.clauses) :: [[Int]])
