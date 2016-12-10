import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

type Arg = (Char, Int)
data Op = Op String Arg Arg deriving (Show, Eq)
type Core = Map Int Op
data Game = Game Core Int deriving Show

sz = 8000

initCore = M.fromList $ zip [0..sz - 1] $ repeat $ Op "DAT" ('$', 0) ('$', 0)

--demo c = M.insert 0 (Op "MOV" ('$', 0) ('$', 1)) c
demo c = M.insert 0 (Op "ADD" ('#', 4) ('$', 3)) $
  M.insert 1 (Op "MOV" ('$', 2) ('@', 2)) $
  M.insert 2 (Op "JMP" ('$', sz - 2) ('#', 0)) $
  M.insert 3 (Op "DAT" ('#', 0) ('#', 0)) c
main = print $ go $ Game (demo initCore) 0

go g@(Game c ip) = Game (foldl' f c deltas) ip1 where
  (deltas, ip1) = exe (c!ip) g
  f c (k, v) = M.insert k v c

exe (Op "NOP" _ _) (Game c ip) = ([], adv ip)

exe (Op "MOV" ma mb) (Game c ip) =
  ([(resolve c ip mb, c!resolve c ip ma)], adv ip)

exe (Op "ADD" ('#', a) mb) (Game c ip) = let
  rb = resolve c ip mb in ([(rb, putB (add a $ getB $ c!rb) $ c!rb)], adv ip)

exe (Op "JMP" ma _) (Game c ip) = ([], resolve c ip ma)

resolve c ip ('#', i) = ip
resolve c ip ('$', i) = add ip i
resolve c ip ('@', i) = add ip $ getB (c!add ip i)

getB (Op _ _ (_, b)) = b
putB b (Op op ma (m, _)) = Op op ma (m, b)

add x y = (x + y) `mod` sz

adv ip = add ip 1
