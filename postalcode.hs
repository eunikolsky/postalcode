import Data.Either
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List
import Text.Parsec hiding (State)

numStates = 9
maskLength = 4

data State = E | F
  deriving (Show, Eq)

newtype Digit = Digit [State]
  deriving Show

digits :: [Digit]
digits = fmap parseDigit . cutDigits $ digitsString
  where parseDigit = fromRight (error "unparseable digit") . parse pDigit "" . unlines

cutDigits :: [String] -> [[String]]
cutDigits =
  unfoldr (\cols -> if null cols then Nothing else Just (transpose $ take 3 cols, drop 4 cols))
  . transpose

digitsString =
  [ "+-+   + --+ --+ | | +--   / --+ +-+ +-+"
  , "| |  /|   |  /  | | |    /   /  | | | |"
  , "| | / |   + +-+ +-+ +-+ +-+ +   +-+ +-+"
  , "| |   |  /   /    |   | | | |   | |  / "
  , "+-+   | +-- /     | --+ +-+ |   +-+ /  "
  ]

{-
digits =
  [ Digit [F, F, E, F, E, F, E, F, F]
  , Digit [E, E, F, F, E, E, E, F, E]
  , Digit [F, E, E, F, E, E, F, E, F]
  , Digit [F, E, F, E, F, E, F, E, E]
  , Digit [E, F, E, F, F, E, E, F, E]
  , Digit [F, F, E, E, F, E, E, F, F]
  , Digit [E, E, F, E, F, F, E, F, F]
  , Digit [F, E, F, E, E, F, E, E, E]
  , Digit [F, F, E, F, F, F, E, F, F]
  , Digit [F, F, E, F, F, E, F, E, E]
  ]
-}

type Parser = Parsec String ()

pFilledState :: Char -> Parser State
pFilledState f = choice [F <$ char f, E <$ char ' ']

pHoriz :: Parser State
pHoriz = pLimit *> pFilledState '-' <* pLimit
  where pLimit = oneOf " +-|/"

pVert :: Parser [State]
pVert = do
  a <- pStraight
  b <- pDiagonal
  c <- pStraight
  pure [a, b, c]
  where
    pStraight = pFilledState '|'
    pDiagonal = pFilledState '/'

pDigit :: Parser Digit
pDigit = do
  a <- pHoriz <* endOfLine
  bs <- pVert <* endOfLine
  c <- pHoriz <* endOfLine
  ds <- pVert <* endOfLine
  e <- pHoriz <* endOfLine <* eof
  pure . Digit $ a : bs <> [c] <> ds <> [e]

newtype Mask = Mask { unMask :: IntSet }
  deriving Show

allMasks :: [Mask]
allMasks = allMasksOfLength maskLength

allMasksOfLength :: Int -> [Mask]
allMasksOfLength n = fmap (Mask . IS.fromList)
  . filter ((== n) . length)
  $ subsequences [0 .. numStates - 1]

newtype MaskedDigit = MaskedDigit [State]
  deriving (Show, Eq)

applyMask :: Mask -> Digit -> MaskedDigit
applyMask (Mask indexes) (Digit states) = MaskedDigit $
  (\index -> states !! index) <$> IS.toAscList indexes

canMaskDistinguishAllDigits :: Mask -> [Digit] -> Bool
canMaskDistinguishAllDigits mask digits =
  not . hasDuplicates $ applyMask mask <$> digits

hasDuplicates :: Eq a => [a] -> Bool
hasDuplicates xs = length (nub xs) /= length xs

uniqueMasksToDistinguishAllDigits :: [Mask] -> [Digit] -> [Mask]
uniqueMasksToDistinguishAllDigits masks digits =
  filter (\mask -> canMaskDistinguishAllDigits mask digits) masks

numberOfUniqueMasksToDistinguishAllDigits :: [Mask] -> [Digit] -> Int
numberOfUniqueMasksToDistinguishAllDigits masks =
  length . uniqueMasksToDistinguishAllDigits masks

main :: IO ()
main = print $ numberOfUniqueMasksToDistinguishAllDigits allMasks digits
