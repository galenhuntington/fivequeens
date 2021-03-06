{-
   This queries a single DTM50 chess endgame tablebase:
   White having five queens versus Black having a king.
   It requires the file <fivequeens.gt1>.
   Currently available at http://www.filedropper.com/fivequeens

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
import Data.Char (isDigit)

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
   [d,d,2] :
   [d,3,2] :
   [d,4,2] :
   [d,2] :
   [d,d,d,d,3] :
   [d,d,d,4,3] :
   [d,d,4,4,3] :
   [d,4,4,4,3] :
   [d,d,d,5,3] :
   [d,d,d,3] :
   [d,d,4,3] :
   [d,d,3] :
   [d,4,3] :
   [d,3] :
   [d,4,4,3] :   -- only for "the nineteen"
   []
   where d = drawCode

--  This allows comparisons with max.
drawCode :: Depth
drawCode = maxBound

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
dflip sq = (sq.&.0o7)`shiftL`3 .|. sq`shiftR`3

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
--  D_4 orbit, with queens in increasing order.
normalize :: [[Square]] -> [[Square]]
normalize sqs@((whk:_):_) = map (sort . map dflip) llq `min` map sort llq where
   mask = (if whk > 31 then 0o70 else 0) .|. (if whk .&. 0o7 > 3 then 0o7 else 0)
   llq = map (map (xor mask)) sqs  -- lower-left quadrant

--  Like zipWith max but if one list is shorter it continues
--  with that list's last element.
pcMerge :: [Depth] -> [Depth] -> [Depth]
pcMerge l1@(h1:_) l2@(h2:_) = go h1 h2 l1 l2 where
   go _ _  [] [] = []
   go _ h2 l1 [] = map (max h2) l1
   go h1 _ [] l2 = map (max h1) l2
   go _ _ (a:l1) (b:l2) = max a b : go a b l1 l2
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
      showValue win d ++ " for PC="
         ++ show pc1 ++ (guard (pc2>pc1) >> ".." ++ show pc2)

--  Show outcome at a particular PC.
showValue :: Bool -> Depth -> String
showValue win dp =
   if dp==drawCode
      then "draw" 
      else "mate" ++ (guard (not win) >> "d") ++ " in " ++ show dp

--  Dispatch based on maybe PC.
--  These names could be improved.
showOutcome :: Maybe Int -> Bool -> [Depth] -> String
showOutcome Nothing   = showDTM50
showOutcome (Just 100) = \ _ outc ->
      if null outc then "illegal position" else "drawn (by fifty-move rule)"
showOutcome (Just pc)  = \ win outc ->
      showValue win $ head $ drop (99-pc) outc ++ last outc : []

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
triangle , triangle' :: [Square] -> Int
triangle l =
   sum [ product [x-i .. x] `div` product [1..i+1]
         | (i, x') <- zip [0..] l, let x = fromIntegral x' ]

--  Tables were generated in above order; this flips it around to be in
--  lexicographic order, to make life harder for myself for no reason.
--  The constant is 64 choose 5 - 1
triangle' l = 7624511 - triangle (reverse $ map (63-) l)

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
   let (mpc, squares) = (fst <$> r, readSquares $ maybe bs snd r)
         where r = BSC.readInt bs
   let shower = showOutcome mpc
   let col = BSC.last bs
   --  Wonky but functional indentation.
   --  Not using MultiWayIf since too new....
   if any (\x -> x<0 || x>63) squares
   then return "parse failure"
   else if maybe False (\pc -> pc<0 || pc>100) mpc
   then return "invalid PC"
   else if nub squares /= squares
   then return "illegal position (duplicate square)"
   else case length squares of
      6 | col == 'b' ->
         return "sorry, four queens with Black to move not available"
        | col == 'w' -> do
         let (whk:_) : whqs : (blk:_) : [] =
               normalize $ let h:t = squares in [h] : t : [last t] : []
         let whkc = kingcon whk
         let tri = triangle' whqs
         if (whkc, tri, blk) `elem` theNineteen
            then return $ shower True $ outcomes ! 16
            else do
               block <- getBlock fh tri
               return $ shower True $ queryBlock block whkc False blk
      7 | col `elem` "bw" -> do
         let (whk:_) : whqs : (blk:_) : [] =
               normalize $ let h:t = squares in [h] : init t : [last t] : []
         let whkc = kingcon whk
         block <- getBlock fh (triangle' whqs)
         let query' = queryBlock block whkc
         if col == 'w'
            then return $ shower True $ query' False blk
            else let
               sym = whk == dflip whk && whqs == sort (map dflip whqs)
               query = query' sym
               blks = kingMoves blk
               moves = [ (blk', v) | blk' <- blks, v@(_:_) <- [query blk'] ]
               in case (query blk, moves) of
                  _  | whk `elem` blks
                           -> return "illegal position"
                  ([], []) -> return "checkmated"
                  (_,  []) -> return "drawn (stalemate)"
                  _        ->
                     return $ shower False $
                        foldl1 pcMerge [
                           if blk' `elem` whqs  -- capture?
                              then last v : []
                              else drawCode : v
                           | (blk', v) <- moves ]
      _ -> return "invalid input"

main = do
   args <- getArgs
   when (null args) $ do
      hPutStrLn stderr "Usage:  query5q [position]"
      exitFailure
   --  Some tolerance for extraneous characters, spaces, etc.
   --  (Not Unicode though.)
   let pos = BSC.filter (`elem`"abcdefgh1234567890w-") $ BS.concat args
   fh <- openFile "fivequeens.gt1" ReadMode
   putStrLn =<< process fh pos
   hClose fh

