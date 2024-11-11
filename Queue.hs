module Queue where

-- Queue Sample
-- https://stackoverflow.com/questions/1739675/efficient-queue-in-haskell

data Queue a = Queue Int Int Int [a] [a] 

skipI _ [] = []
skipI 0 (x:xs) = xs
skipI i (x:xs) = x: skipI (i-1) xs

pushIn e (Queue totLen qLen stackLen q stack) = 
  Queue (totLen+1) qLen (stackLen+1) q (e:stack)

popHelper 0 q dequeued = (reverse dequeued, q)
popHelper n q@(Queue 0 _ _ _ _) dequeued = (reverse dequeued, q)
popHelper n (Queue _ _ stackLen [] stack) dequeued =
  popHelper n (Queue stackLen stackLen 0 (reverse stack) []) dequeued
popHelper n (Queue totLen qLen stackLen (x:q) stack) dequeued =
  popHelper (n-1) (Queue (totLen-1) (qLen-1) stackLen q stack) (x:dequeued)

instance Show a => Show (Queue a) where
  show q = "qFromList " ++ show (qToList q)

instance Functor Queue where
  fmap f (Queue a b c l1 l2) = Queue a b c (fmap f l1) (fmap f l2)

instance Foldable Queue where
  foldr f e q = foldr f e $ qToList q

class Functor q => Queueable q where
  qFromList :: [a] -> q a
  qToList :: q a -> [a]
  qLength :: q a -> Int
  qPopHelper :: Int -> q a -> [a] -> ([a], q a)
  qPush :: a -> q a -> q a
  qPushL :: [a] -> q a -> q a
  qMerge :: q a -> q a -> q a  -- seq like `++` for queue
  qAddTo :: [a] -> q a -> q a    -- seq like `++` for queue ++ list
  qTransfer :: Int -> q a -> q a -> (q a, q a)
  qPop :: q a -> (a,q a)
  qPopN :: Int -> q a -> ([a],q a)
  qHead :: q a -> a
  qTail :: q a -> q a
  qTake :: Int -> q a -> q a
  qDrop :: Int -> q a -> q a
  qSkipI :: Int -> q a -> q a
  qEmpty :: q a -> Bool
  qElem :: (Eq a) => a -> q a -> Bool
  qFilter :: (a -> Bool) -> q a -> q a
  qMapI :: Int -> (a -> a) -> q a -> q a
  qMap :: (a -> b) -> q a -> q b
  qIndex :: q a -> Int -> a
  qMap = fmap
  qIndex q a = qToList q !! a
  qSkipI i q = qFromList $ skipI i q'
    where q' = qToList q
  qMapI i f q = qFromList $ mapI i f $ qToList q
    where 
      mapI _ _ [] = []
      mapI 0 f (x:xs) = f x:xs
      mapI i f (x:xs) = x:mapI (i-1) f xs
  qPushL l q = foldr qPush q $ reverse l
  qMerge q q' = qAddTo (qToList q') q
  qAddTo l q = foldr qPush q $ reverse l
  qTransfer n q q' = 
    case qPopN n q of
      (ns, qbar) -> (qbar, qAddTo ns q)
  qPop q = 
    case qPopHelper 1 q [] of
      ([],q') -> error "The queue is empty"
      (x:xs,q') -> (x,q')
  qHead q = 
    case qPop q of
      (x,_) -> x
  qTail q = 
    case qPop q of
      (_,q') -> q'
  qTake n q = 
    case qPopHelper n q [] of
      (x,_) -> qFromList x
  qDrop n q = 
    case qPopHelper n q [] of
      (_,q') -> q'
  qPopN n q = qPopHelper n q []
  qPopHelper n q _ = (take n $ qToList q, qDrop n q)
  qEmpty q = null $ qToList q
  qElem x q = elem x $ qToList q
  qFilter f q = qFromList $ filter f $ qToList q

instance Queueable Queue where
  qLength (Queue n _ _ _ _) = n 
  qFromList l = Queue len len 0 l []
    where len = length l
  qToList (Queue _ _ _ q s) = q ++ reverse s
  qPopHelper = popHelper
  qPush = pushIn

main = do
  let q = (qFromList [1,2,3,4,5] :: Queue Int)
  let q7 = qPush 7 $ qPush 6 q
  print $ qPop q
  print $ qTake 3 q
  print q7
  print $ qLength $ qDrop 5 q7
  print $ qPush 5 $ qDrop 8 q7
  print $ qLength $qPush 5 $ qDrop 8 q7
  print $ qTransfer 3 q q7
  print $ qLength $ qMerge q q7
  print $ ("PushL",qPushL [1,2,3] q)
  print $ ("PopN",qPopN 3 q)
  print $ qMapI 1 (+1) q
  print $ qSkipI 2 q
  print $ qSkipI 0 q
  print $ skipI 2 [1,2..10]
  putStrLn "Auto Test finished"
