{-
   This queries a single DTM50 chess endgame tablebase:
   White having five queens versus Black having a king.
   It requires the file <fivequeens.gt1>, which is
   currently available at http://www.filedropper.com/fivequeens

   Galen Huntington, 2015
-}


--  Standard platform modules.
import System.IO
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Word
import Data.Array.Unboxed
import System.Exit
import System.Posix.Env.ByteString (getArgs)
import Data.List
import Control.Monad
import Control.Applicative
import Data.Bits

--  Not so standard.
import Control.Monad.Trans.Resource (runResourceT)
import Data.Conduit
import Data.Conduit.Lzma
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Lazy as CLL


--  Mixing word sizes more than needed?
type Square = Word8
type Depth = Word16
type OutcomeCode = Word8

outcomes :: Array OutcomeCode [Depth]
outcomes = listArray (0, 16) $
   [] :
   [1] :
   [0,0,2] :
   [0,3,2] :
   [0,4,2] :
   [0,2] :
   [0,0,0,0,3] :
   [0,0,0,4,3] :
   [0,0,4,4,3] :
   [0,4,4,4,3] :
   [0,0,0,5,3] :
   [0,0,0,3] :
   [0,0,4,3] :
   [0,0,3] :
   [0,4,3] :
   [0,3] :
   [0,4,4,3] :   -- only for "the nineteen"
   []

--  The 19 exceptional positions for 4 queens.
--  (An ending outcome that doesn't occur with 5 queens.)
theNineteen :: [(Int, Int, Square)]
theNineteen =
   (1,481719,21) : (1,481845,21) : (1,481726,21) : (1,481852,21) :
   (4,776,45) : (4,49330,45) : (4,729979,43) : (4,739275,43) :
   (5,1220993,42) : (5,1230653,42) : (5,1426967,42) : (5,1433603,42) :
   (6,1309784,45) : (6,1365271,45) : (6,79509,43) : (6,1739469,43) :
   (6,88805,43) : (6,1748765,43) : (9,99109,44) : []

--  Diagonal flip; row/column swap.
dflip :: Square -> Square
dflip  sq = (sq.&.0o7)`shiftL`3 .|. sq`shiftR`3

--  Places a king might go, not considering rest of board.
kingMoves :: Square -> [Square]
kingMoves sq = let
   n1 = sq .&. 0o7; n2 = (sq .&. 0o70) `shiftR` 3
   f n = case n of
      0 -> 0 : 1 : []
      7 -> 6 : 7 : []
      _ -> n : n-1 : n+1 : []
   in
   [ sq' | a <- f n1, b <- f n2, let sq' = a + b*8, sq /= sq' ]

--  Use symmetries to get into "normal" position, lexicographically least in
--  D_4 orbit.
normalize :: [Square] -> [Square]
normalize sqs@(whk:_) = llq `min` map dflip llq where
   mask = (if whk > 31 then 0o70 else 0) .|. (if whk .&. 0o7 > 3 then 0o7 else 0)
   --  Lower-left quadrant.
   llq = map (xor mask) sqs

--  Easier to work with this than an Ord wrapper.
--  (Other option: have 0xff or whatever code draw.)
max0 :: (Ord a, Num a) => a -> a -> a
max0 0 x = 0
max0 x 0 = 0
max0 x y = max x y

--  Like zipWith max0 but if one list is shorter it continues
--  with that list's last element.
pcMerge :: [Depth] -> [Depth] -> [Depth]
pcMerge l1@(h1:_) l2@(h2:_) = go h1 h2 l1 l2 where
   go _ _  [] [] = []
   go _ h2 l1 [] = map (max0 h2) l1
   go h1 _ [] l2 = map (max0 h1) l2
   go _ _ (a:l1) (b:l2) = max0 a b : go a b l1 l2
pcMerge _ _ = error "pcMerge: empty list"

--  Human-readable DTM50.
--  First argument indicates whether winning or not ("mate" vs. "mated").
showDTM50 :: Bool -> [Depth] -> String
showDTM50 win pcl = concat $ intersperse ", " $ reverse $ go' 99 $ group pcl where
   go' pc (g:rest@(_:_)) =
      let pc' = pc - length g in go pc (pc'+1) g : go' pc' rest
   go' pc [g] = go pc 0 g : []
   go' pc [] = "illegal position" : []
   go pc2 pc1 (d:_) =
      (if d==0 then "draw" else "mate" ++ (guard (not win) >> "d") ++ " in " ++ show d)
         ++ " for PC=" ++ show pc1 ++ (guard (pc2>pc1) >> ".." ++ show pc2)

--  These are what they say on the tin.
--  Could do this with a real parser but doesn't seem worth it.

readSquare :: BS.ByteString -> Square
readSquare sq =
   let Just (r, x) = BS.uncons sq; Just (c, _) = BS.uncons x
   in fromIntegral $ fromIntegral r * 8 + fromIntegral c - (fromEnum 'a' * 8 + fromEnum '1')

readSquares :: BS.ByteString -> [Square]
readSquares bs =
   map readSquare $ take (BS.length bs `div` 2) $ unfoldr (Just . BS.splitAt 2) bs

--  Compress list of increasing Squares into unique Int.
triangle, triangle' :: [Square] -> Int
triangle l =
   sum [ product [x-i .. x] `div` product [1..i+1]
         | (i, x') <- zip [0..] l, let x = fromIntegral x' ]

--  Tables were generated in above order; this flips it around to be in
--  lexicographic order, to make life harder for myself for no reason.
--  The constant is 64 choose 5 - 1
triangle' l = 7624511 - triangle (reverse $ map (0o77-) l)

--  I use an array in the generator for speed, but here it's used once.
kingcon :: Square -> Int
kingcon x' = x .&. 0o7 + [0, 3, 5, 6] !! (x `shiftR` 3)
   where x = fromIntegral x'

--  Get the numbered block from the given (seekable) filehandle.
getBlock :: Handle -> Int -> IO (UArray Int OutcomeCode)
getBlock fh block = do
   let (chunk, within) = block `divMod` 8928
   hSeek fh AbsoluteSeek (fromIntegral chunk * 4)
   offs' <- decode <$> BL.hGet fh 4
   let offs = fromIntegral (offs' :: Word32)
   len <- fromIntegral <$> (offs' `subtract`) <$> decode <$> BL.hGet fh 4
   hSeek fh AbsoluteSeek $ fromIntegral offs
   runResourceT $ do
      arr <- listArray (0, 639) . concatMap denibbler .
         --  Cast through BL for efficient dropping.
         BL.unpack . BL.drop (fromIntegral within * 320) . BL.fromChunks <$>
         CLL.lazyConsume (CB.sourceHandle fh =$= CB.isolate len =$= decompress Nothing)
      --  Need to force evaluation inside T... don't know a better way.
      (arr ! 0) `seq` return arr
   where denibbler c = c .&. 0xf : c `shiftR` 4 : []

--  Includes wrapper for handling rare symmetries... integrate with getBlock?
--  Or put into Black move logic since that's only time it's used?
queryBlock :: UArray Int OutcomeCode -> Int -> Bool -> Square -> [Depth]
queryBlock block whkc sym blk =
   let blk' = if sym then blk `min` dflip blk else blk
   in outcomes ! (block ! (10 * fromIntegral blk' + whkc))

--  Mega-function for dispatching all possible inputs.
process :: Handle -> BS.ByteString -> IO String
process fh bs = do
   let squares@(whk : rest) = normalize $ readSquares bs
   let whkc = kingcon whk
   let col = BSC.last bs
   --  Wonky but functional indentation.
   --  Not using MultiWayIf since too new....
   if even (BS.length bs) || any (\x -> x<0 || x>0o77) squares
   then return "parse failure"
   else if nub squares /= squares
   then return "illegal position (duplicate square)"
   else case length squares of
      6 | col == 'b' ->
         return "sorry, four queens with Black to move not supported"
        | col == 'w' -> do
         let (tri, blk) = (triangle' $ sort rest, last rest)
         if (whkc, tri, blk) `elem` theNineteen
            then return $ showDTM50 True $ outcomes ! 16
            else do
               block <- getBlock fh tri
               return $ showDTM50 True $ queryBlock block whkc False blk
      7 | col `elem` "bw" -> do
         let (whqs', blk:_) = splitAt 5 rest
         let whqs = sort whqs'
         block <- getBlock fh (triangle' whqs)
         let query' = queryBlock block whkc
         if col == 'w'
            then return $ showDTM50 True $ query' False blk
            else let
               sym = whk == dflip whk && whqs == sort (map dflip whqs)
               query = query' sym
               blks = kingMoves blk
               moves =
                  [ (blk', v) | blk' <- blks, let v = query blk', v /= [] ]
               in case (query blk, moves) of
                  _  | whk `elem` blks
                           -> return "illegal position"
                  ([], []) -> return "checkmated"
                  (_,  []) -> return "draw (stalemate)"
                  _        ->
                     return $ showDTM50 False $
                        foldl1 pcMerge [
                           if blk' `elem` whqs  -- capture?
                              then last v : []
                              else 0 : v
                           | (blk', v) <- moves ]
      _ -> return "invalid input"

main = do
   args <- getArgs
   when (null args) $ do
      hPutStrLn stderr "Usage:  query5q [position]"
      exitFailure
   --  Some tolerance for extraneous characters, spaces, etc.
   --  (Not Unicode though.)
   let pos = BS.take 15 $ BSC.filter (`elem`"abcdefgh12345678w") $ BS.concat args
   fh <- openFile "fivequeens.gt1" ReadMode
   putStrLn =<< process fh pos
   hClose fh

