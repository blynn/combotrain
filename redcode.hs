import Data.Char
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

type Arg = (Char, Int)
data Op = Op String Arg Arg deriving (Show, Eq)
type Core = Map Int Op
data Game = Game Core Int deriving Show

sz = 8000

initCore = M.fromList $ zip [0..sz - 1] $ repeat $ Op "DAT" ('$', 0) ('$', 0)

go g@(Game c ip) = Game (foldl' f c deltas) ip1 where
  (deltas, ip1) = exe (c!ip) g
  f c (k, v) = M.insert k v c

exe (Op op (ma, a) (mb, b)) (Game c ip) = f op ma mb where
  f "NOP" _   _ = ([], adv ip)
  f "MOV" '#' _ = ([(rb, putB aa $ c!rb)], adv ip)
  f "MOV" _ '#' = ([(rb, putB (getB $ c!ra) $ c!rb)], adv ip)
  f "MOV" _   _ = ([(rb, c!ra)], adv ip)
  f "SEQ" '#' _ = skipIf $ aa == bb
  f "SEQ" _ '#' = skipIf $ getB (c!ra) == bb
  f "SEQ" _   _ = skipIf $ ra == rb
  f "SNE" '#' _ = skipIf $ aa /= bb
  f "SNE" _ '#' = skipIf $ getB (c!ra) /= bb
  f "SNE" _   _ = skipIf $ ra /= rb
  f "ADD" '#' _ = ([(rb, putB (add a $ bb) $ c!rb)], adv ip)
  f "JMP" _   _ = ([], ra)
  f op _ _ = error $ "huh " ++ op
  skipIf True  = ([], adv $ adv ip)
  skipIf False = ([], adv ip)
  ra = resolve c ip (ma, a)
  rb = resolve c ip (mb, b)
  aa = getA $ c!ra
  bb = getB $ c!rb

resolve c ip ('#', i) = ip
resolve c ip ('$', i) = add ip i
resolve c ip ('@', i) = let j = add ip i in add j $ getB $ c!j

getA (Op _ (_, a) _) = a
getB (Op _ _ (_, b)) = b
putB b (Op op ma (m, _)) = Op op ma (m, b)

add x y = (x + y) `mod` sz

adv ip = add ip 1

num :: Parser Int
num = do
  s <- option id $ const negate <$> char '-'
  s . read <$> many1 digit

arg = do
  spaces
  m <- option '$' $ oneOf "@#$"
  n <- num
  return (m, n)

known = S.fromList $ words "MOV ADD SUB JMP JMZ JMG DJZ SEQ SNE DAT"

unalias "CMP" = "SEQ"
unalias s     = s

asm :: Parser Op
asm = do
  spaces
  op <- unalias . map toUpper <$> many1 letter
  if op `S.notMember` known then
    fail $ "unknown: " ++ op
  else do
    a <- arg
    m <- optionMaybe $ optional (try $ spaces >> char ',') >> arg
    spaces
    eof
    case m of
      Just b -> return $ Op op a b
      Nothing -> case op of
        "JMP" -> return $ Op op a ('#', 0)
        "DAT" -> return $ Op op ('#', 0) a
        _ -> fail $ "needs 2 args: " ++ op

load ops a c = foldl' f c $ zip [a..] ops where f c (k, v) = M.insert k v c

demo = load ops 0 where
  Right ops = mapM (parse asm "")
    ["jmp 3", "dat 0", "dat 99",
     "mov @-2, @-1", "sne -3, #9", "jmp 4",
     "add #1, -5", "add #1, -5", "jmp -5",
     "mov #99, 93", "jmp 93"]
    --["add #4, 3", "mov 2, @2", "jmp -2", "dat 0"]

main = print $ iterate go (Game (demo initCore) 0)!!100
