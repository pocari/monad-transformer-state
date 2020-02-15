module Sample where

import           Control.Monad.State

type Stack = [Int]

-- Stateのサンプル
testStateManip :: State Stack Int
testStateManip = do
  _ <- push 1
  _ <- push 2
  _ <- push 3
  peek

testState :: IO ()
testState = do
  -- evalStateの結果はIOモナドでないので、結果をletでrに束縛して使う
  let r = evalState testStateManip [7, 8, 9]
  putStrLn $ "r is " ++ show r

-- StateTのサンプル
testStateTManip :: StateT Stack IO Int
testStateTManip = do
  _ <- push 1
  _ <- push 2
  s <- get
  liftIO $ print s
  _ <- push 3
  peek

testStateT :: IO ()
testStateT = do
  -- evalStateの結果はIOモナドなので、その中身をbindでrに束縛して使う
  r <- evalStateT testStateTManip [7, 8, 9]
  putStrLn $ "r is " ++ show r

-- Stateが StateTのモナドがIdentityMonadのものとして定義されてるので
--   Monad m => StateT 状態の型 m 値の型
-- を返すように各種関数を作っておくと State にも StateT にもつかる関数になる
push :: Monad m => Int -> StateT Stack m Int
push x = state $ \s -> (x, x : s)

pop :: Monad m => StateT Stack m Int
pop = state $ \(x : xs) -> (x, xs)

peek :: Monad m => StateT Stack m Int
peek = state $ \xs -> (head xs, xs)

