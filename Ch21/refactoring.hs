module Refactoring where

-- example entiresly from the book; none of the code here
-- is mine

import Control.Monad (join)

data Query     = Query
data SomeObj   = SomeObj
data IoOnlyObj = IoOnlyObj
data Err       = Err

decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

fetchFn :: Query -> IO [String]
fetchFn = undefined

makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
  a <- fetchFn query
  traverse makeIoOnlyObj (traverse decodeFn a)
--  a <- fetchFn query
--  case sequence (map decodeFn a) of
--    (Left err) -> return $ Left $ err
--    (Right res) -> do 
--      a <- makeIoOnlyObj res
--      return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' =
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
