{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module SAT (
    module Control.Lens
  , module SAT.Mios
  , module SAT
  ) where

----------------------------------------------
-- Imports
----------------------------------------------

import           Control.Lens               hiding (inside)
import           Control.Monad.State.Strict
import           Data.Bits
import           Data.Coerce
import           Data.Foldable              (traverse_)
import           Data.List                  (nub, sortOn)
import           Data.Maybe                 (mapMaybe)
import qualified Data.Ord                   as Ord
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import           Data.Tuple                 (swap)
import           SAT.Mios                   hiding (SAT, UNSAT, validate)
import           Text.Read                  (readMaybe)

----------------------------------------------
-- Data Types
----------------------------------------------

type BoxIndex = Int
type RotIndex = Int
type CellIndex = Int

data Box = Box
    { _width  :: Int
    , _height :: Int
    }
  deriving stock (Show)
makeLenses ''Box

instance Eq Box where
  b1 == b2 =
    b1^.width == b2^.width
      && b1^.height == b2^.height

-- By total area
instance Ord Box where
  b1 `compare` b2 =
    let area b = b^.width * b^.height
     in area b1 `compare` area b2

isSquare :: Box -> Bool
isSquare b = b^.width == b^.height

type Coord = (Int, Int)

----------------------------------------------
-- BWP -> SAT
----------------------------------------------

-- The problem will be translated to rectangular box where each position is a variable.
--
-- It will additionally require:
--  - A vector of rotations variables, one for each box.
--       A negated rotation variable means that the box has been rotated.
--  - A vector of coordinates vector for the overlapping.
--
--      +--------------+
--     /|             /| |
--    / |            / | |
--   *--+-----------*  | |
--   |  |           |  | |  maxLength
--   |  |           |  | |
--   |  |           |  | |
--   |  |           |  | |
--   |  +-----------|--+
--   | /            | / /
--   |/             |/ /  #boxes
--   *--------------* /
--    -------------
--        width

newtype Variable = Variable { variable :: Int }
  deriving newtype (Show)

-- | Disjunctive clause
newtype Clause = Clause { variables :: [Variable] }

instance Show Clause where
  show (Clause vars) =
    unwords $ fmap show (vars ++ [Variable 0])

-- | Conjunction of disjunction of variables.
type CNF = [Clause]

-- | State S for the building of the BWP.
data S = S
    { _boxes     :: [Box]
    , _w         :: Int        -- ^ w
    , _maxLength :: Int        -- ^ maxLength
    , _clauses   :: CNF
    , _extraVars :: Int
    }
  deriving stock (Show)
makeLenses ''S

-- | Use this for new variables in the AMO encodings
getNewVars :: Int -> StateS [Variable]
getNewVars n = do
  start <- extraVars <<+= n
  return $ coerce [start..start+n-1]

-- | Lens for the computed value #boxes.
getNBoxes :: S -> Int
getNBoxes = length . _boxes

-- | Count of all variables used in the statement.
--
-- tl  = B*W*L
-- cl  = B*W*L
-- rot = B
--
getNVariables :: S -> Int
getNVariables s =
  let nboxes = getNBoxes s
   in (nboxes * (s^.w) * (s^.maxLength))*2 + nboxes

-- | The SAT Monad
type StateS a = State S a
type SAT = StateS ()

neg :: Variable -> Variable
neg = Variable . negate . variable

emptyClause :: Clause
emptyClause = Clause []

addClause :: Clause -> SAT
addClause c = clauses %= (c :)

addClauses :: [Clause] -> SAT
addClauses = traverse_ addClause

singleClause :: Variable -> Clause
singleClause var = Clause [var]

-- TODO refactor abstraction ..
class Disjunctive a b where
  (\/) :: a -> b -> Clause
  infixr 8 \/

instance Disjunctive Variable Variable where
  var1 \/ var2 = Clause [var1, var2]

instance Disjunctive Variable Clause where
  var \/ Clause{..} = Clause (var:variables)

instance Disjunctive Clause Variable where
  Clause{..} \/ var = Clause (var:variables)

instance Disjunctive (Maybe Variable) (Maybe Variable) where
  Just var1 \/ Just var2 = var1 \/ var2
  Just var \/ Nothing    = var \/ emptyClause
  Nothing \/ Just var    = var \/ emptyClause
  Nothing \/ Nothing     = emptyClause

newS :: [Box] -> Int -> Int -> S
newS boxes width maxLen =
  let nboxes = length boxes
   in S{ _boxes     = boxes
       , _w         = width
       , _maxLength = maxLen
       , _clauses   = []
       , _extraVars = (nboxes*width*maxLen)*2 + nboxes + 1
       }

getTL :: Coord -> BoxIndex -> StateS Variable
getTL coord b = (\s -> getTLS s coord b) <$> get

getTLS :: S -> Coord -> BoxIndex -> Variable
getTLS S{..} (x, y) b =
  Variable (x + y*_w + b*_w*_maxLength + 1)

getRot :: RotIndex -> StateS Variable
getRot b = (`getRotS` b) <$> get

getRotS :: S -> RotIndex -> Variable
getRotS S{..} b =
  Variable ((length _boxes * _w * _maxLength) + 1 + b)

getCell :: Coord -> CellIndex -> StateS Variable
getCell coords c = (\s -> getCellS s coords c) <$> get

getCellS :: S -> Coord -> CellIndex -> Variable
getCellS s@S{..} coord c =
  Variable (cellIndexStart + cellOffset)
  where
    nboxes = getNBoxes s
    cellIndexStart      = (nboxes*_w*_maxLength) + nboxes + 1 -- TODO I added +1
    Variable cellOffset = getTLS s coord c

----------------------------------------------
-- Encoding
----------------------------------------------


-- | Conjunction of variables.
conjunctionOf :: [Variable] -> Clause
conjunctionOf = foldr (\/) emptyClause

exactlyOne :: [Variable] -> SAT
exactlyOne xs = do
  atLeastOne xs
  atMostOne xs

-- | At Least One Constraint
atLeastOne :: [Variable] -> SAT
atLeastOne = addClause . conjunctionOf

-- TODO: configurable
atMostOne :: [Variable] -> SAT
atMostOne = atMostOneLogarithmic

-- At most one constraint: quadractic encoding
atMostOneQuadratic :: [Variable] -> SAT
atMostOneQuadratic variables = addClauses
  [ neg (variables !! i) \/ neg (variables !! j)
  | i <- [0  ..(length variables - 1)]
  , j <- [i+1..(length variables - 1)]
  ]

-- At Most One Constraint (Logarithmic Encoding)
atMostOneLogarithmic :: [Variable] -> SAT
atMostOneLogarithmic xs = do
  ys <- getNewVars m
  traverse_ (addClause . getClause) [ (x,y) | x <- zip [0..] xs, y <- zip [0..] ys]

    where
      n = length xs
      m = ceiling $ logBase @Double 2 $ fromIntegral n

      getClause :: ((Int, Variable), (Int, Variable)) -> Clause
      getClause ((i, x),(j, y))
        | testBit j i = neg x \/ y
        | otherwise   = neg x \/ neg y


-- At most one constraint: heule encoding
atMostOneHeule :: [Variable] -> StateS [Clause]
atMostOneHeule variables = error "Not implemented"

----------------------------------------------
-- Constraints
----------------------------------------------

-- | By symmetry, place the biggest box on the coordinate (0,0).
biggestBoxTopLeft :: SAT
biggestBoxTopLeft = do
  addClause =<< singleClause <$> getTL (0,0) 0 -- boxes are sorted in decreasing ord.
  allCoords <- paperRollCoords
  let p (x,y) = x > 0 || y > 0
      allCoordsExceptTL = filter p allCoords
  tls <- traverse (`getTL` 0) allCoordsExceptTL
  traverse_ (addClause . singleClause . neg) tls


-- | Boxes must be placed exactly once in the paper roll.
exactlyOneClauses :: SAT
exactlyOneClauses =
  applyForAllBoxes_ $ \(b, _, coords) ->
    exactlyOne =<< traverse (`getTL` b) coords

-- | Boxes must be placed inside the paper roll.
insideTheBounds :: SAT
insideTheBounds =
  void $ applyForAllBoxes $ \(b, box, allCoords) ->
    forM_ allCoords $ \coord -> do
      tl  <- getTL coord b
      rot <- getRot b
      addBoundingClause coord box rot tl
  where
    addBoundingClause coord box rot tl
      | isSquare box =
        whenM (not <$> inside coord box) $ addClause (neg tl \/ emptyClause)
      | otherwise = do
         whenM (not <$> inside coord box)        $ addClause (neg tl \/ rot)
         whenM (not <$> insideRotated coord box) $ addClause (neg tl \/ neg rot)

overlappingVariables :: SAT
overlappingVariables = do
  S{..} <- get
  sequence_ [ addCellsForEachBoxAndCoordinates (x,y) (b, box)
                 | (b, box) <- zip [0..] _boxes
                 , x <- [0.._w - 1]
                 , y <- [0.._maxLength -1]]
  where
    addCellsForEachBoxAndCoordinates (x,y) (b, box)
      | isSquare box = do
          addCellsNoRotation (x,y) (b,box)
          -- Only once
          when (x == 0 && y == 0) $ do
            rot <- getRot b
            addClause (neg rot \/ emptyClause)

      | otherwise = do
          addCellsNoRotated (x,y) (b,box)
          addCellsRotated   (x,y) (b,box)

    addCellsNoRotation :: Coord -> (BoxIndex, Box) -> SAT
    addCellsNoRotation (x,y) (b, box@Box{..}) =
      forM_ (area box (x,y)) $ \(i,j) ->
        do
        --whenM (inside (i,j) box) $ do
          tl  <- getTL (x,y) b
          cl  <- getCell (i,j) b
          addClause $ neg tl \/ cl

    addCellsNoRotated :: Coord -> (BoxIndex, Box) -> SAT
    addCellsNoRotated (x,y) (b, box@Box{..}) =
      forM_ (area box (x,y)) $ \(i,j) ->
        do
        --whenM (inside (i,j) box) $ do
          tl  <- getTL (x,y) b
          cl  <- getCell (i,j) b
          rot <- getRot b
          addClause $ neg tl \/ cl \/ rot

    addCellsRotated :: Coord -> (BoxIndex, Box) -> SAT
    addCellsRotated (x,y) (b, box@Box{..}) =
      forM_ (areaRotated box (x,y)) $ \(i,j) ->
        do
        --whenM (insideRotated (i,j) box) $ do
          tl  <- getTL (x,y) b
          cl  <- getCell (i,j) b
          rot <- getRot b
          addClause $ neg tl \/ cl \/ neg rot

amoOverlapping :: SAT
amoOverlapping = do
  coords <- paperRollCoords
  nboxes <- uses boxes length
  forM_ coords $ \(x,y) -> do
    bxsVars <- traverse (getCell (x,y)) [0..nboxes-1]
    atMostOne bxsVars

-- | Boxes can't overlap.
noOverlapping :: SAT
noOverlapping =
  overlappingVariables >> amoOverlapping

buildSAT :: SAT
buildSAT = do
  biggestBoxTopLeft
  exactlyOneClauses
  insideTheBounds
  noOverlapping

----------------------------------------------
-- SAT -> BWP
----------------------------------------------

(!) :: (Show a, Show b) => a -> b -> String
(!) x y = show x <> " " <> show y
infixr 8 !

type MiosSolution = [Int]

data Result a = UNSAT | SAT (S, a)
  deriving stock (Functor)

data BoxSol = BoxSol
    { tl :: Coord
    , br :: Coord
    }

instance Show BoxSol where
  show (BoxSol (tl_x, tl_y) (br_x, br_y)) =
    tl_x ! tl_y <> "  " <> br_x ! br_y

data BWPSolution = BWPSolution
    { len          :: Int
    , boxSolutions :: [BoxSol]
    }

type RawBoxesSol = [Int] -- ^ Length = w * maxLength * #boxes
type RawBoxSol = [Int]   -- ^ Length = w * maxLength

type RawRotsSol = [Int]  -- ^ Length = #boxes
type RawRotSol = Int

getSections :: S -> MiosSolution -> (RawBoxesSol, RawRotsSol)
getSections S{..} sol = (boxSec, rotSec)
  where
    (boxSec, rem) = splitAt (length _boxes*_w*_maxLength) sol
    rotSec = take (length _boxes) rem

getCoord :: S -> RawBoxSol -> Coord
getCoord S{..} raw =
  let pos = length $ takeWhile (< 0) raw
   in (pos `mod` _w,  pos `div` _w)

translateBox :: S -> Box -> RawBoxSol -> RawRotSol -> BoxSol
translateBox s b rawBox rawRot =
  --traceShow b $ traceShowId
    BoxSol{ tl = (tl_x, tl_y)
          , br = (tl_x + w - 1, tl_y + h - 1)
          }
  where
    (tl_x,tl_y) = getCoord s rawBox
    rot = if rawRot > 0 then swap else id
    (w, h) = rot (b^.width, b^.height)

getBWPLength :: [BoxSol] -> Int
getBWPLength =
  (+) 1 . foldr (\BoxSol{..} -> max (snd br)) 0

translate :: S -> MiosSolution -> BWPSolution
translate s@S{..} sol = BWPSolution{..}
  where
    (rawBoxes, rawRots) = getSections s sol

    boxSolutions =
      let (_, _, bxs) =
            foldr go (rawBoxes, rawRots, []) (reverse _boxes)
      in reverse bxs

    go box (bs, rs, acc) =
      let (b  , bs') = splitAt (_w * _maxLength) bs
          ([r], rs') = splitAt 1 rs
      in (bs', rs', translateBox s box b r : acc)

    len = getBWPLength boxSolutions


-- | Check the length of the output of the solver
validate :: S -> MiosSolution -> Maybe MiosSolution
validate statement raw =
  if length solution < leastLength
    then Nothing
    else Just solution
  where
    solution = nub raw

    leastLength =
      let nboxes = getNBoxes statement
       in nboxes*(statement^.w)*(statement^.maxLength) + nboxes

translateSolution :: S -> MiosSolution -> Maybe BWPSolution
translateSolution statement =
  fmap (translate statement) . validate statement

----------------------------------------------
-- Parsing & Pretty Printing
----------------------------------------------

-- | Input Example
--
--   10 10
--   3  2 4
--   6  1 2
--   1  2 2


-- | Output Example
--
--   10 10
--   3   2 4
--   6   1 2
--   1   2 2
--   4
--   0 0   3 1
--   5 2   8 3
--   6 0   9 1
--   2 2   3 3
--   4 0   4 1
--   1 2   1 3
--   4 2   4 3
--   0 2   0 3
--   9 2   9 3
--   5 0   5 1

readBoxes :: Int -> Int -> IO ([Text], [Box], Int)
readBoxes w = go [] [] 0
  where
    go acc bs !maxLen 0 = pure (acc, bs, maxLen)
    go acc bs !maxLen n = do
      line <- T.getLine
      let [m, width, height] = mapMaybe readInt (T.splitOn " " line)
      let b   = Box width height
          bs' = replicate m b
          l   = m * max width height
          --l   = min (upperBoundLength m width height)
                    --(upperBoundLength m height width)
      go (acc ++ [line]) (bs ++ bs') (maxLen + l) (n - m)

    --upperBoundLength :: Int -> Int -> Int -> Int
    --upperBoundLength m width height =
      --let boxesPerRow = w `div` min width height
          --minRows = ceiling (fromIntegral m / fromIntegral boxesPerRow :: Double)
       --in minRows * max width height


readInt :: Text -> Maybe Int
readInt = readMaybe . T.unpack

-- | Read input and translate it to the BWP
readStatement :: IO ([String], S)
readStatement = do
  str <- T.getLine
  let [w, n] = mapMaybe readInt (T.splitOn " " str)
  (strs, bxs, maxLen) <- readBoxes w n
  let rawStatement = T.unpack <$> (str:strs)
      statement = newS (sortOn Ord.Down bxs) w maxLen
  return (rawStatement, statement)

-- | Prints the BWP solution to the expected format
printBoxWrappingSolution :: BWPSolution -> IO ()
printBoxWrappingSolution BWPSolution{..} =
  print len >> traverse_ print boxSolutions

----------------------------------------------
-- Utils
----------------------------------------------

-- | Like 'when', but where the test can be monadic.
whenM :: Monad m => m Bool -> m () -> m ()
whenM b t = ifM b t (pure ())

-- | Like 'unless', but where the test can be monadic.
unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b = ifM b (pure ())

-- | Like @if@, but where the test can be monadic.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f

runSAT :: SAT -> S -> S
runSAT = execState

coerceCNF :: CNF -> [[Int]]
coerceCNF = coerce

getDescription :: S -> CNFDescription
getDescription s = CNFDescription
   nVars                           -- # variables
   (lengthOf (clauses.traverse) s) -- # clauses
   mempty                          -- FilePath
  where
    nVars = maximum . fmap abs . concat $ (coerce (s^.clauses) :: [[Int]])

paperRollCoords :: StateS [Coord]
paperRollCoords = do
  S{..} <- get
  return [ (x,y) | x <- [0..       _w - 1]
                 , y <- [0.._maxLength -1]]

forAllBoxes :: StateS [(BoxIndex, Box, [Coord])]
forAllBoxes = do
  coords <- paperRollCoords
  let tupled3 (a,b) c = (a,b,c)
  fmap (`tupled3` coords) <$> allBoxes -- allButFirst

allBoxes :: StateS [(BoxIndex, Box)]
allBoxes = do
  s <- get
  return $ zip [0..] (s^.boxes)

-- All boxes except the first one
allButFirst :: StateS [(BoxIndex, Box)]
allButFirst = do
  s <- get
  return $ zip [1..] (s ^. boxes . to tail)

applyForAllBoxes :: ((BoxIndex, Box, [Coord]) -> StateS r) -> StateS [r]
applyForAllBoxes f =  do
  bxsAndCoords <- forAllBoxes
  traverse f bxsAndCoords

applyForAllBoxes_ :: ((BoxIndex, Box, [Coord]) -> StateS ()) -> SAT
applyForAllBoxes_ f =  do
  bxsAndCoords <- forAllBoxes
  traverse_ f bxsAndCoords

area :: Box -> Coord -> [Coord]
area Box{..} (x,y) =
  [ (i,j)
    | i <- [x..x + _width - 1]
    , j <- [y..y + _height - 1]]

areaRotated :: Box -> Coord -> [Coord]
areaRotated Box{..} (x,y) =
  [ (i,j)
    | i <- [x..x + _height - 1]
    , j <- [y..y +  _width - 1]]

inside :: Coord -> Box -> StateS Bool
inside (x,y) Box{..} = do
  S{..} <- use id
  return (x + _width - 1 < _w && y + _height - 1 < _maxLength)

insideRotated :: Coord -> Box -> StateS Bool
insideRotated (x,y) Box{..} = do
  S{..} <- use id
  return (x + _height - 1 < _w && y + _width - 1 < _maxLength)
