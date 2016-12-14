= Core War =


++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
<script src="redcode.js"></script>
<canvas id="canvas" style="border: 1px solid black;" width="300" height="240">
</canvas>
<p><textarea id="con" rows="5" cols="80" readonly></textarea></p>
<p><button id="goB">Restart</button>
<button id="stopB">Halt</button></p>
<p><textarea id="player1" rows="25" cols="16" spellcheck="false">
jmp 4
mov 2, -1
jmp -1
dat 9
spl -2
spl 4
add #-16, -3
mov -4, @-4
jmp -4
spl 2
jmp -1
mov 0 1
</textarea>
<textarea id="player2" rows="25" cols="16" spellcheck="false">
jmp 2
dat 0
mov #12, -1
mov @-2, <5
djn -1, -3
spl @3
add #653, 2
jmz -5, -6
dat 833
</textarea></p>
++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

//////////////////////////////////////////////////////////////////////////////
Dwarf

  add #4, 3
  mov 2, @2
  jmp -2
  dat 0

Gemini

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
//////////////////////////////////////////////////////////////////////////////

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
data Game = Game Core [(Int, [Int])] deriving Show

sz = 8000

inskvs = foldl' (\c (k, v) -> M.insert k v c)

initCore = M.fromList $ zip [0..sz - 1] $ repeat $ Op "DAT" ('#', 0) ('#', 0)

exe c ip = (preb ++ prea ++ deltas, ip1) where
  Op _ (ma, a) (mb, b) = c!ip
  preb | mb == '<' = [(rb, putB (sub (getB $ c!rb) 1) $ c!rb)]
       | otherwise = []
  cb = inskvs c preb
  rb = resolve c ip ('$', b)
  prea | ma == '<' = [(ra, putB (sub (getB $ cb!ra) 1) $ cb!ra)]
       | otherwise = []
  ca = inskvs cb prea
  ra = resolve cb ip ('$', a)
  (deltas, ip1) = exeRedcode ca ip

exeRedcode c ip = f op ma mb where
  Op op (ma, a) (mb, b) = c!ip
  f "DAT" _   _ = ([], [])
  f "NOP" _   _ = ([], adv)
  f "MOV" '#' _ = ([(rb, putB aa ib)], adv)
  f "MOV" _ '#' = ([(rb, putB ba ib)], adv)
  f "MOV" _   _ = ([(rb, c!ra)], adv)
  f "SEQ" '#' _ = skipIf $ aa == bb
  f "SEQ" _ '#' = skipIf $ ba == bb
  f "SEQ" _   _ = skipIf $ ia == ib
  f "SNE" '#' _ = skipIf $ aa /= bb
  f "SNE" _ '#' = skipIf $ ba /= bb
  f "SNE" _   _ = skipIf $ ia /= ib
  f "ADD" '#' _ = ([(rb, putB (add a $ bb) ib)], adv)
  f "ADD" _ '#' = ([(rb, putB (add ba $ bb) ib)], adv)
  f "ADD" _   _ = ([(rb, putA (add aa $ ab) $ putB (add ba $ bb) ib)], adv)
  f "SPL" _   _ = ([], adv ++ [ra])
  f "JMP" _   _ = jumpIf True      ra
  f "JMN" _   _ = jumpIf (bb /= 0) ra
  f "JMZ" _   _ = jumpIf (bb == 0) ra
  f "DJN" _   _ = effect [(rb, putB (sub bb 1) ib)] $ jumpIf (bb /= 1) ra
  f "DJZ" _   _ = effect [(rb, putB (sub bb 1) ib)] $ jumpIf (bb == 1) ra
  f op _ _ = error $ "huh " ++ op
  jumpIf True  a = ([], [a])
  jumpIf False _ = ([], adv)
  skipIf True  = ([], map (add 1) $ adv)
  skipIf False = ([], adv)
  effect es (ds, a) = (ds ++ es, a)
  ra = resolve c ip (ma, a)
  rb = resolve c ip (mb, b)
  ia = c!ra
  ib = c!rb
  aa = getA ia
  ba = getB ia
  ab = getA ib
  bb = getB ib
  adv = [add ip 1]

resolve c ip ('#', i) = ip
resolve c ip ('$', i) = add ip i
resolve c ip ('@', i) = let j = add ip i in add j $ getB $ c!j
resolve c ip ('<', i) = resolve c ip ('@', i)

getA (Op _ (_, a) _) = a
getB (Op _ _ (_, b)) = b
putA a (Op op (m, _) mb) = Op op (m, a) mb
putB b (Op op ma (m, _)) = Op op ma (m, b)

add x y = (x + y) `mod` sz
sub x y = (x + sz - y) `mod` sz

num :: Parser Int
num = do
  s <- option id $ const negate <$> char '-'
  s . read <$> many1 digit

arg = do
  spaces
  m <- option '$' $ oneOf "@#$<"
  n <- num
  return (m, standardize n)

standardize n | m < 0     = sz - m
              | otherwise = m
              where m = mod n sz

jumps = words "JMP JMZ JMN DJZ DJN SPL"
known = flip S.member $ S.fromList $ words "MOV ADD SUB SEQ SNE DAT " ++ jumps
isJump = flip S.member $ S.fromList jumps

unalias "CMP" = "SNE"
unalias "JMG" = "JMN"
unalias s     = s

asm :: Parser Op
asm = do
  spaces
  op <- unalias . map toUpper <$> many1 letter
  if not $ known op then fail $ "unknown: " ++ op else do
    a <- arg
    m <- optionMaybe $ optional (try $ spaces >> char ',') >> arg
    spaces
    eof
    case m of
      Just b -> return $ Op op a b
      Nothing -> if isJump op then return $ Op op a ('#', 0)
        else if op == "DAT"  then return $ Op op ('#', 0) a
        else fail $ "needs 2 args: " ++ op

load ops a c = inskvs c $ zip [a..] ops

passive = [RGB 191 63 63, RGB 63 63 191]
active = [RGB 255 127 127, RGB 127 127 255]

main = withElems ["canvas", "player1", "player2", "con", "goB", "stopB"] $
     \[canvasE, player1E, player2E, conE, goB, stopB] -> do
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

    step g@(Game c []) = do
      putMVar gv $ Nothing
      v0 <- getProp conE "value"
      setProp conE "value" $ v0 ++ "all programs halted\n"

    step g@(Game c ((id, ip:rest):players)) = do
      let
        (deltas, next) = exe c ip
        truncNext = take (8000 - length rest) $ next
        ipq = rest ++ truncNext
        c1 = inskvs c deltas
      mapM (mark (passive!!id) . fst) deltas
      mark (passive!!id) ip
      mapM (mark (active!!id)) truncNext
      case ipq of
        (h:_) -> do
          putMVar gv $ Just $ Game c1 $ players ++ [(id, ipq)]
        [] -> do
          v0 <- getProp conE "value"
          setProp conE "value" $ v0 ++ "program " ++ show id ++ " halted\n"
          putMVar gv $ Just $ Game c1 players

    newMatch = do
      render canvas $ color (RGB 0 0 0) $ fill $ rect (0, 0) (300, 240)
      s <- getProp player1E "value"
      case mapM (parse asm "") $ lines s of
        Left err -> do
          swapMVar gv Nothing
          setProp conE "value" $ show err
        Right p1 -> do
          s <- getProp player2E "value"
          case mapM (parse asm "") $ lines s of
            Left err -> do
              swapMVar gv Nothing
              setProp conE "value" $ show err
            Right p2 -> gameOn p1 p2

    gameOn p1 p2 = do
      mapM_ (mark $ passive!!0) [0..length p1 - 1]
      mark (active!!0) 0
      mapM_ (mark $ passive!!1) [4000..4000 + length p2 - 1]
      mark (active!!1) 4000
      void $ swapMVar gv $ Just $ Game
        (load p2 4000 $ load p1 0 initCore) [(0, [0]), (1, [4000])]
      setProp conE "value" $ "running programs: 0 vs 1\n"

  void $ goB `onEvent` Click $ \_ -> newMatch

  void $ stopB `onEvent` Click $ \_ -> do
    jg <- takeMVar gv
    case jg of
      Just _ -> do
        v0 <- getProp conE "value"
        setProp conE "value" $ v0 ++ "match halted\n"
      Nothing -> pure ()
    putMVar gv Nothing

  newMatch
  void $ setTimer (Repeat 16) tryStep

\end{code}
