= Core Wars =

++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="redcode.js"></script>
<canvas id="canvas" style="border: 1px solid black;" width="300" height="240">
</canvas>
<p><textarea id="con" rows="5" cols="80" readonly>
</textarea></p>
<p><button id="goB">Go!</button></p>
<p><textarea id="player1" rows="25" cols="20" spellcheck="false">
jmp 3
dat 0
dat 99
mov @-2, @-1
sne -3, #9
jmp 4
add #1, -5
add #1, -5
jmp -5
mov #99, 93
jmp 93
</textarea></p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

After installing Haste, run:

------------------------------------------------------------------------------
haste-cabal install parsec
------------------------------------------------------------------------------

\begin{code}
import Control.Concurrent.MVar
import Control.Monad
import Data.Char
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Text.ParserCombinators.Parsec

import Haste
import Haste.DOM
import Haste.Events
import Haste.Graphics.Canvas

type Arg = (Char, Int)
data Op = Op String Arg Arg deriving (Show, Eq)
type Core = Map Int Op
data Game = Game Core Int deriving Show

sz = 8000

initCore = M.fromList $ zip [0..sz - 1] $ repeat $ Op "DAT" ('$', 0) ('$', 0)

exe (Op op (ma, a) (mb, b)) (Game c ip) = f op ma mb where
  f "NOP" _   _ = ([], adv ip)
  f "MOV" '#' _ = ([(rb, putB aa ib)], adv ip)
  f "MOV" _ '#' = ([(rb, putB ba ib)], adv ip)
  f "MOV" _   _ = ([(rb, c!ra)], adv ip)
  f "SEQ" '#' _ = skipIf $ aa == bb
  f "SEQ" _ '#' = skipIf $ ba == bb
  f "SEQ" _   _ = skipIf $ ia == ib
  f "SNE" '#' _ = skipIf $ aa /= bb
  f "SNE" _ '#' = skipIf $ ba /= bb
  f "SNE" _   _ = skipIf $ ia /= ib
  f "ADD" '#' _ = ([(rb, putB (add a $ bb) ib)], adv ip)
  f "ADD" _ '#' = ([(rb, putB (add ba $ bb) ib)], adv ip)
  f "ADD" _   _ = ([(rb, putA (add aa $ ab) $ putB (add ba $ bb) ib)], adv ip)
  f "JMP" _   _ = ([], ra)
  f "JMN" _   _ = jumpIf (bb /= 0) ra
  f "JMZ" _   _ = jumpIf (bb == 0) ra
  f "DJN" _   _ = effect [(rb, putB (sub bb 1) ib)] $ jumpIf (bb /= 1) ra
  f "DJZ" _   _ = effect [(rb, putB (sub bb 1) ib)] $ jumpIf (bb == 1) ra
  f op _ _ = error $ "huh " ++ op
  jumpIf True  a = ([], a)
  jumpIf False _ = ([], adv ip)
  skipIf True  = ([], adv $ adv ip)
  skipIf False = ([], adv ip)
  effect es (ds, a) = (ds ++ es, a)
  ra = resolve c ip (ma, a)
  rb = resolve c ip (mb, b)
  ia = c!ra
  ib = c!rb
  aa = getA ia
  ba = getB ia
  ab = getA ib
  bb = getB ib

resolve c ip ('#', i) = ip
resolve c ip ('$', i) = add ip i
resolve c ip ('@', i) = let j = add ip i in add j $ getB $ c!j

getA (Op _ (_, a) _) = a
getB (Op _ _ (_, b)) = b
putA a (Op op (m, _) mb) = Op op (m, a) mb
putB b (Op op ma (m, _)) = Op op ma (m, b)

add x y = (x + y) `mod` sz
sub x y = (x + sz - y) `mod` sz

adv ip = add ip 1

num :: Parser Int
num = do
  s <- option id $ const negate <$> char '-'
  s . read <$> many1 digit

arg = do
  spaces
  m <- option '$' $ oneOf "@#$"
  n <- num
  return (m, standardize n)

standardize n | m < 0     = sz - m
              | otherwise = m
              where m = mod n sz

known = S.fromList $ words "MOV ADD SUB JMP JMZ JMN DJZ DJN SEQ SNE DAT"

unalias "CMP" = "SNE"
unalias "JMG" = "JMN"
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

passive1 = RGB 191 63 63
active1 = RGB 255 127 127

main = withElems ["canvas", "player1", "con", "goB"] $
     \[canvasE, player1E, conE, goB] -> do
  Just canvas <- fromElem canvasE
  gv <- newMVar Nothing
  let
    mark c a = renderOnTop canvas $ color c $ box x y
      where (y, x) = divMod a 100
    box x y = fill $ rect (xf, yf) (xf + 3, yf + 3) where
      xf = fromIntegral x * 3
      yf = fromIntegral y * 3
    tryStep = do
      jg <- takeMVar gv
      case jg of
        Just g -> step g
        Nothing -> putMVar gv Nothing

    step g@(Game c ip) = do
      let
        (deltas, ip1) = exe (c!ip) g
        g1 = Game (foldl' ins c deltas) ip1
        ins c (k, v) = M.insert k v c
      mark passive1 ip
      mapM (mark passive1 . fst) deltas
      mark active1 ip1
      putMVar gv $ Just g1

    newMatch = do
      render canvas $ color (RGB 0 0 0) $ fill $ rect (0, 0) (300, 240)
      s <- getProp player1E "value"
      case mapM (parse asm "") $ lines s of
        Left err -> do
          swapMVar gv Nothing
          setProp conE "value" $ show err
        Right ops -> do
          mapM_ (mark passive1) [0..length ops - 1]
          let
            g = Game (load ops 0 initCore) 0
          mark active1 0
          void $ swapMVar gv $ Just g

  void $ goB `onEvent` Click $ \_ -> newMatch

  newMatch
  void $ setTimer (Repeat 16) tryStep

\end{code}
