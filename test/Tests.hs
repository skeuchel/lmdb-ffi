
import Database.LMDB.C

import Control.Monad
import Data.Foldable
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.Random
import System.IO.Temp


umask :: MDB_mode
umask = 432 -- == 0660

chk :: MDB_return_code -> IO ()
chk rc = unless (rc == MDB_SUCCESS) (error $ "Test failed: " ++ show rc)

e :: IO MDB_return_code -> IO ()
e m = m >>= chk

p :: String -> IO ()
p = putStrLn

withEnv :: (MDB_env u -> IO a) -> IO a
withEnv f = do
  p "Creating environment"
  env <- alloca $ \penv -> do
           e $ mdb_env_create penv
           peek penv
  f env

main :: IO ()
main =
  withSystemTempDirectory "lmdb-ffi" $ \dir ->
  withEnv $ \env -> do

    p "Seeding global random generator"
    seed <- randomIO
    setStdGen (mkStdGen seed)
    p $ "Seed: " ++ show seed

    p "Creating test data"
    count  <- randomRIO (10000,20000)
    keys   <- replicateM count (randomRIO (100,999))
    values <- replicateM count (randomRIO (100,999))

    p "Setting environment up"
    e $ mdb_env_set_maxreaders env 1
    e $ mdb_env_set_mapsize env (1024*1024)
    e $ mdb_env_set_maxdbs env 1

    p "Opening environment"
    e $ withCString dir $ \cdir ->
      mdb_env_open env cdir MDB_NOSYNC umask

    p "Creating test txn"
    txn <- alloca $ \ptxn -> do
             e $ mdb_txn_begin env (MDB_txn nullPtr) 0 ptxn
             peek ptxn

    p "Opening null db"
    dbi <- alloca $ \pdbi -> do
             e $ mdb_dbi_open txn nullPtr 0 pdbi
             peek pdbi :: IO (MDB_dbi () Int Int)

    p $ "Adding " ++ show count ++ " values"
    for_ (zip keys values) $ \(k, d) ->
      alloca $ \pKey -> alloca $ \pData ->
        alloca $ \pValKey -> alloca $ \pValData -> do
          poke pKey k
          poke pData d
          poke pValKey (MDB_val (fromIntegral $ sizeOf k) pKey)
          poke pValData (MDB_val (fromIntegral $ sizeOf d) pData)
          p $ "Putting " ++ show k ++ " ↦ " ++ show d
          e $ mdb_put txn dbi pValKey pValData 0
    e $ mdb_txn_commit txn

    p "Getting env stat"
    stat <- alloca $ \pstat -> do
      e $ mdb_env_stat env pstat
      peek pstat

    p $ "Stat: " ++ show stat

    p "Closing environment"
    mdb_env_close env
