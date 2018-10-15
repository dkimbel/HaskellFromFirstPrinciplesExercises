import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Lazy hiding (get)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Functor.Identity

type RM r a = ReaderT r Maybe a
type MR r a = MaybeT (Reader r) a

myRm :: RM Bool Int
myRm = ReaderT $ \r -> Just 5

myMr :: MR Bool Int
myMr = MaybeT $ ReaderT $ \r -> Identity (Just 5)

extractedMyRm :: Maybe Int
extractedMyRm = (runReaderT myRm) True

extractedMyMr :: Maybe Int
extractedMyMr = runIdentity $ (runReaderT (runMaybeT myMr)) True

-- The above look pretty equivalent to me, but the version
-- where MaybeT is the outermost transformer is structurally
-- much more convoluted.
-- Note: it may be possible to make `extractedMyMr` less
-- convoluted by using runReader instead of runIdentity
-- and runReaderT.
