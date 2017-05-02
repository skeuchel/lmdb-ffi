{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
module Database.LMDB.C
  ( -- * Version
    mdb_VERSION_MAJOR
  , mdb_VERSION_MINOR
  , mdb_VERSION_PATCH
  , mdb_VERSION_STRING

    -- * Types
  , MDB_mode
  , MDB_filehandle
  , MDB_env
  , MDB_txn(..)
  , MDB_txnid
  , MDB_dbi
  , MDB_cursor
  , MDB_val(..)
  , MDB_cmp_func
#ifdef LMDB_FIXEDMAP
  , MDB_rel_func
#endif
  , MDB_msg_func
  , MDB_assert_func
  , MDB_stat(..)
  , MDB_envinfo(..)
  , MDB_env_flags
  , pattern MDB_FIXEDMAP
  , pattern MDB_NOSUBDIR
  , pattern MDB_NOSYNC
  , pattern MDB_RDONLY
  , pattern MDB_NOMETASYNC
  , pattern MDB_WRITEMAP
  , pattern MDB_MAPASYNC
  , pattern MDB_NOTLS
  , pattern MDB_NOLOCK
  , pattern MDB_NORDAHEAD
  , pattern MDB_NOMEMINIT
  , MDB_db_flags
  , pattern MDB_REVERSEKEY
  , pattern MDB_DUPSORT
  , pattern MDB_INTEGERKEY
  , pattern MDB_DUPFIXED
  , pattern MDB_INTEGERDUP
  , pattern MDB_REVERSEDUP
  , pattern MDB_CREATE
  , MDB_write_flags
  , pattern MDB_NOOVERWRITE
  , pattern MDB_NODUPDATA
  , pattern MDB_CURRENT
  , pattern MDB_RESERVE
  , pattern MDB_APPEND
  , pattern MDB_APPENDDUP
  , pattern MDB_MULTIPLE
  , MDB_copy_flags
  , pattern MDB_CP_COMPACT
  , MDB_txn_flags
  , pattern MDB_RDONLY
  , MDB_cursor_op
  , pattern MDB_FIRST
  , pattern MDB_FIRST_DUP
  , pattern MDB_GET_BOTH
  , pattern MDB_GET_BOTH_RANGE
  , pattern MDB_GET_CURRENT
  , pattern MDB_GET_MULTIPLE
  , pattern MDB_LAST
  , pattern MDB_LAST_DUP
  , pattern MDB_NEXT
  , pattern MDB_NEXT_DUP
  , pattern MDB_NEXT_MULTIPLE
  , pattern MDB_NEXT_NODUP
  , pattern MDB_PREV
  , pattern MDB_PREV_DUP
  , pattern MDB_PREV_NODUP
  , pattern MDB_SET
  , pattern MDB_SET_KEY
  , pattern MDB_SET_RANGE
  , pattern MDB_PREV_MULTIPLE
  , MDB_return_code
  , pattern MDB_SUCCESS
  , pattern MDB_KEYEXIST
  , pattern MDB_NOTFOUND
  , pattern MDB_PAGE_NOTFOUND
  , pattern MDB_CORRUPTED
  , pattern MDB_PANIC
  , pattern MDB_VERSION_MISMATCH
  , pattern MDB_INVALID
  , pattern MDB_MAP_FULL
  , pattern MDB_DBS_FULL
  , pattern MDB_READERS_FULL
  , pattern MDB_TLS_FULL
  , pattern MDB_TXN_FULL
  , pattern MDB_CURSOR_FULL
  , pattern MDB_PAGE_FULL
  , pattern MDB_MAP_RESIZED
  , pattern MDB_INCOMPATIBLE
  , pattern MDB_BAD_RSLOT
  , pattern MDB_BAD_TXN
  , pattern MDB_BAD_VALSIZE
  , pattern MDB_BAD_DBI

    -- * Imports
  , mdb_version
  , mdb_strerror
  , mdb_env_create
  , mdb_env_open
  , mdb_env_copy
  , mdb_env_copyfd
  , mdb_env_copy2
  , mdb_env_copyfd2
  , mdb_env_stat
  , mdb_env_info
  , mdb_env_sync
  , mdb_env_close
  , mdb_env_set_flags
  , mdb_env_get_flags
  , mdb_env_get_path
  , mdb_env_get_fd
  , mdb_env_set_mapsize
  , mdb_env_set_maxreaders
  , mdb_env_get_maxreaders
  , mdb_env_set_maxdbs
  , mdb_env_get_maxkeysize
  , mdb_env_set_userctx
  , mdb_env_get_userctx
  , mdb_env_set_assert
  , mdb_txn_begin
  , mdb_txn_env
  , mdb_txn_id
  , mdb_txn_commit
  , mdb_txn_abort
  , mdb_txn_reset
  , mdb_txn_renew
  , mdb_dbi_open
  , mdb_stat
  , mdb_dbi_flags
  , mdb_dbi_close
  , mdb_drop
  , mdb_set_compare
  , mdb_set_dupsort
#ifdef LMDB_FIXEDMAP
  , mdb_set_relfunc
  , mdb_set_relctx
#endif
  , mdb_get
  , mdb_put
  , mdb_del
  , mdb_cursor_open
  , mdb_cursor_close
  , mdb_cursor_renew
  , mdb_cursor_txn
  , mdb_cursor_dbi
  , mdb_cursor_get
  , mdb_cursor_put
  , mdb_cursor_del
  , mdb_cursor_count
  , mdb_cmp
  , mdb_dcmp
  , mdb_reader_list
  , mdb_reader_check

    -- * Unsafe imports
  , unsafe_mdb_get
  , unsafe_mdb_put
  , unsafe_mdb_del
  , unsafe_mdb_cursor_get
  , unsafe_mdb_cursor_put
  , unsafe_mdb_cursor_del
  , unsafe_mdb_cursor_count
  , unsafe_mdb_cmp
  , unsafe_mdb_dcmp

    -- * Wrappers
  , wrapCmpFunc
  , wrapMsgFunc
  ) where

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

#ifdef LMDB_SYSTEM
#include <lmdb.h>
#else
#include "../cbits/lmdb.h"
#endif

import Control.Applicative   ((<$>), (<*>))
import Data.Bits             (Bits(..), FiniteBits(..))
import Data.List             (foldl')
import Data.Monoid           (Monoid(..))
import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr
import Foreign.Storable      (Storable(..))
import Numeric               (showHex)

--------------------------------------------------------------------------------

mdb_VERSION_MAJOR, mdb_VERSION_MINOR, mdb_VERSION_PATCH :: Int
mdb_VERSION_MAJOR  = #const MDB_VERSION_MAJOR
mdb_VERSION_MINOR  = #const MDB_VERSION_MINOR
mdb_VERSION_PATCH  = #const MDB_VERSION_PATCH

mdb_VERSION_STRING :: String
mdb_VERSION_STRING = #const_str MDB_VERSION_STRING

--------------------------------------------------------------------------------

-- = Type Definitions
type MDB_mode       = #type mdb_mode_t
type MDB_filehandle = #type mdb_filehandle_t
-- Just for mdb_env_set_maxdbs
type MDB_dbi_t      = #type MDB_dbi
{-| Opaque structure for LMDB environment.

    A DB environment supports multiple databases, all residing in the same
    shared-memory map.
-}
newtype MDB_env u = MDB_env (Ptr (MDB_env u))
  deriving (Eq, Storable)

{-| Opaque structure for a transaction handle.

    All database operations require a transaction handle. Transactions may be
    read-only or read-write.

    A transaction and its cursors must only be used by a single thread, and a
    thread may only have a single transaction at a time.  If 'MDB_NOTLS' is in
    use, this does not apply to read-only transactions.
-}
newtype MDB_txn u = MDB_txn (Ptr (MDB_txn u))
  deriving (Eq, Storable)

newtype MDB_txnid = MDB_txnid { from_mdb_txnid :: CSize }
  deriving (Eq, Ord, Storable)

instance Show MDB_txnid where
  showsPrec _ txid = showString "0x" . showHex (from_mdb_txnid txid)

{-| Handle for an individual database in the environment. -}
newtype MDB_dbi u k v = MDB_dbi #type MDB_dbi
  deriving (Eq, Storable)

{-| Opaque structure for navigating through a database. -}
newtype MDB_cursor u k v = MDB_cursor (Ptr (MDB_cursor u k v))
  deriving (Eq, Storable)

{-| Generic structure used for passing keys and data in and out of the database.

    Values returned from the database are valid only until a subsequent update
    operation, or the end of the transaction. Do not modify or free them, they
    commonly point into the database itself.

    Key sizes must be between 1 and 'mdb_env_get_maxkeysize' inclusive. The same
    applies to data sizes in databases with the 'MDB_DUPSORT' flag. Other data
    items can in theory be from 0 to 0xffffffff bytes long.

    Be cautious; committing the transaction that obtained a value should also
    invalidate it; avoid capturing 'MDB_val' in a lazy value. A safe interface
    similar to STRef could be provided by another module.
-}
data MDB_val a = MDB_val
  { mv_size :: {-# UNPACK #-} !CSize
  , mv_data :: {-# UNPACK #-} !(Ptr a)
  }

{-| Type of callback functions used to compare two keys in a database. -}
type MDB_cmp_func a = Ptr (MDB_val a) -> Ptr (MDB_val a) -> IO CInt

#ifdef LMDB_FIXEDMAP
{-| A callback function used to relocate a position-dependent data item
    in a fixed-address database.
-}
type MDB_rel_func a =
     Ptr (MDB_val a)  -- ^ (in/out) The item that is to be relocated.
  -> Ptr ()           -- ^ (in)     The previous address.
  -> Ptr ()           -- ^ (in)     The new address to relocate to.
  -> Ptr ()           -- ^ (in)     An application-provided context, set by
                      --            'mdb_set_relctx'.
  -> IO ()
#endif

{-| A callback function used to print a message from the library. -}
type MDB_msg_func a =
     CString  -- ^ The string to be printed.
  -> Ptr a    -- ^ An arbitrary context pointer for the callback.
  -> IO CInt  -- ^ < 0 on failure, >= 0 on success.

type MDB_assert_func u =
     Ptr (MDB_env u)
  -> CString
  -> IO ()

-- | Statistics for a database in the environment
data MDB_stat = MDB_stat
  { -- | Size of a database page. This is currently the same for all databases.
    ms_psize          :: {-# UNPACK #-} !CUInt
  , -- | Depth (height) of the B-tree.
    ms_depth          :: {-# UNPACK #-} !CUInt
  , -- | Number of internal (non-leaf) pages.
    ms_branch_pages   :: {-# UNPACK #-} !CSize
  , -- | Number of leaf pages.
    ms_leaf_pages     :: {-# UNPACK #-} !CSize
  , -- | Number of overflow pages.
    ms_overflow_pages :: {-# UNPACK #-} !CSize
  , -- | Number of data items.
    ms_entries        :: {-# UNPACK #-} !CSize
  } deriving (Eq, Show)

-- | Information about the environment
data MDB_envinfo = MDB_envinfo
  { -- | Address of map, if fixed.
    me_mapaddr        :: {-# UNPACK #-} !(Ptr ())
  , -- | Size of the data memory map.
    me_mapsize        :: {-# UNPACK #-} !CSize
  , -- | ID of the last used page.
    me_last_pgno      :: {-# UNPACK #-} !CSize
  , -- | ID of the last committed transaction.
    me_last_txnid     :: {-# UNPACK #-} !MDB_txnid
  , -- | max reader slots in the environment.
    me_maxreaders     :: {-# UNPACK #-} !CUInt
  , -- | max reader slots used in the environment.
    me_numreaders     :: {-# UNPACK #-} !CUInt
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

decompileFlags :: (Bits a, FiniteBits a) => a -> [a]
decompileFlags a =
  [ bit i
  | i <- [0..finiteBitSize a - 1]
  , testBit a i
  ]

{-| Helper function used to produce monoid like 'Show' results. E.g.

    show (zeroBits :: MDB_env_flags)
      ==>   "mempty"
    show MDB_FIXEDMAP
      ==>   "MDB_FIXEDMAP"
    show (MDB_FIXEDMAP <> MDB_NOSUBDIR)
      ==>   "MDB_FIXEDMAP <> MDB_NOSUBDIR"
-}
showsPrecFlags :: (Bits a, FiniteBits a)
  => Int                   -- ^ The show precedence.
  -> (Int -> a -> ShowS)   -- ^ 'showsPrec' for a single flag.
  -> a                     -- ^ The flags to show.
  -> ShowS
showsPrecFlags p0 showsPrecFlag f0 = go p0 (decompileFlags f0)
  where
    go _ []     = showString "mempty"
    go p [f]    = showsPrecFlag p f
    go p (f:fs) = showsPrecFlag (mappendPrec+1) f .
                  showString " <> " .
                  go p fs
    mappendPrec = 6

-- |
-- == Environment flags

newtype MDB_env_flags = MDB_ENV_FLAG CUInt
  deriving (Eq, Num, Bits, FiniteBits, Storable)

pattern MDB_FIXEDMAP   :: MDB_env_flags
pattern MDB_FIXEDMAP   = #const MDB_FIXEDMAP
pattern MDB_NOSUBDIR   :: MDB_env_flags
pattern MDB_NOSUBDIR   = #const MDB_NOSUBDIR
pattern MDB_NOSYNC     :: MDB_env_flags
pattern MDB_NOSYNC     = #const MDB_NOSYNC
pattern MDB_RDONLY     :: (Eq a, Num a) => a
pattern MDB_RDONLY     = #const MDB_RDONLY
pattern MDB_NOMETASYNC :: MDB_env_flags
pattern MDB_NOMETASYNC = #const MDB_NOMETASYNC
pattern MDB_WRITEMAP   :: MDB_env_flags
pattern MDB_WRITEMAP   = #const MDB_WRITEMAP
pattern MDB_MAPASYNC   :: MDB_env_flags
pattern MDB_MAPASYNC   = #const MDB_MAPASYNC
pattern MDB_NOTLS      :: MDB_env_flags
pattern MDB_NOTLS      = #const MDB_NOTLS
pattern MDB_NOLOCK     :: MDB_env_flags
pattern MDB_NOLOCK     = #const MDB_NOLOCK
pattern MDB_NORDAHEAD  :: MDB_env_flags
pattern MDB_NORDAHEAD  = #const MDB_NORDAHEAD
pattern MDB_NOMEMINIT  :: MDB_env_flags
pattern MDB_NOMEMINIT  = #const MDB_NOMEMINIT

instance Show MDB_env_flags where
  showsPrec p0 = showsPrecFlags p0 $ \p f -> case f of
    MDB_FIXEDMAP    -> showString "MDB_FIXEDMAP"
    MDB_NOSUBDIR    -> showString "MDB_NOSUBDIR"
    MDB_NOSYNC      -> showString "MDB_NOSYNC"
    MDB_RDONLY      -> showString "MDB_RDONLY"
    MDB_NOMETASYNC  -> showString "MDB_NOMETASYNC"
    MDB_WRITEMAP    -> showString "MDB_WRITEMAP"
    MDB_MAPASYNC    -> showString "MDB_MAPASYNC"
    MDB_NOTLS       -> showString "MDB_NOTLS"
    MDB_NOLOCK      -> showString "MDB_NOLOCK"
    MDB_NORDAHEAD   -> showString "MDB_NORDAHEAD"
    MDB_NOMEMINIT   -> showString "MDB_NOMEMINIT"
    MDB_ENV_FLAG f' -> showParen (p > 10) $
                         showString "MDB_ENV_FLAG 0x" .
                         showHex f'


-- |
-- == Database flags

newtype MDB_db_flags = MDB_DB_FLAG CUInt
  deriving (Eq, Num, Bits, FiniteBits, Storable)

pattern MDB_REVERSEKEY  :: MDB_db_flags
pattern MDB_REVERSEKEY  = #const MDB_REVERSEKEY
pattern MDB_DUPSORT     :: MDB_db_flags
pattern MDB_DUPSORT     = #const MDB_DUPSORT
pattern MDB_INTEGERKEY  :: MDB_db_flags
pattern MDB_INTEGERKEY  = #const MDB_INTEGERKEY
pattern MDB_DUPFIXED    :: MDB_db_flags
pattern MDB_DUPFIXED    = #const MDB_DUPFIXED
pattern MDB_INTEGERDUP  :: MDB_db_flags
pattern MDB_INTEGERDUP  = #const MDB_INTEGERDUP
pattern MDB_REVERSEDUP  :: MDB_db_flags
pattern MDB_REVERSEDUP  = #const MDB_REVERSEDUP
pattern MDB_CREATE      :: MDB_db_flags
pattern MDB_CREATE      = #const MDB_CREATE

instance Show MDB_db_flags where
  showsPrec p0 = showsPrecFlags p0 $ \p f -> case f of
    MDB_REVERSEKEY -> showString "MDB_REVERSEKEY"
    MDB_DUPSORT    -> showString "MDB_DUPSORT"
    MDB_INTEGERKEY -> showString "MDB_INTEGERKEY"
    MDB_DUPFIXED   -> showString "MDB_DUPFIXED"
    MDB_INTEGERDUP -> showString "MDB_INTEGERDUP"
    MDB_REVERSEDUP -> showString "MDB_REVERSEDUP"
    MDB_CREATE     -> showString "MDB_CREATE"
    MDB_DB_FLAG f' -> showParen (p > 10) $
                        showString "MDB_DB_FLAG 0x" .
                        showHex f'


-- |
-- == Write flags

newtype MDB_write_flags = MDB_WR_FLAG CUInt
  deriving (Eq, Num, Bits, FiniteBits, Storable)

pattern MDB_NOOVERWRITE :: MDB_write_flags
pattern MDB_NOOVERWRITE = #const MDB_NOOVERWRITE
pattern MDB_NODUPDATA   :: MDB_write_flags
pattern MDB_NODUPDATA   = #const MDB_NODUPDATA
pattern MDB_CURRENT     :: MDB_write_flags
pattern MDB_CURRENT     = #const MDB_CURRENT
pattern MDB_RESERVE     :: MDB_write_flags
pattern MDB_RESERVE     = #const MDB_RESERVE
pattern MDB_APPEND      :: MDB_write_flags
pattern MDB_APPEND      = #const MDB_APPEND
pattern MDB_APPENDDUP   :: MDB_write_flags
pattern MDB_APPENDDUP   = #const MDB_APPENDDUP
pattern MDB_MULTIPLE    :: MDB_write_flags
pattern MDB_MULTIPLE    = #const MDB_MULTIPLE

instance Show MDB_write_flags where
  showsPrec p0 = showsPrecFlags p0 $ \p f -> case f of
    MDB_NOOVERWRITE -> showString "MDB_NOOVERWRITE"
    MDB_NODUPDATA   -> showString "MDB_NODUPDATA"
    MDB_CURRENT     -> showString "MDB_CURRENT"
    MDB_RESERVE     -> showString "MDB_RESERVE"
    MDB_APPEND      -> showString "MDB_APPEND"
    MDB_APPENDDUP   -> showString "MDB_APPENDDUP"
    MDB_MULTIPLE    -> showString "MDB_MULTIPLE"
    MDB_WR_FLAG f'  -> showParen (p > 10) $
                         showString "MDB_WR_FLAG 0x" .
                         showHex f'


-- |
-- == Copy flags

newtype MDB_copy_flags = MDB_CP_FLAG CUInt
  deriving (Eq, Num, Bits, FiniteBits, Storable)

pattern MDB_CP_COMPACT :: MDB_copy_flags
pattern MDB_CP_COMPACT = #const MDB_CP_COMPACT

instance Show MDB_copy_flags where
  showsPrec p0 = showsPrecFlags p0 $ \p f -> case f of
    MDB_CP_COMPACT -> showString "MDB_CP_COMPACT"
    MDB_CP_FLAG f' -> showParen (p > 10) $
                      showString "MDB_CP_FLAG 0x" .
                      showHex f'


-- |
-- == Transaction flags
newtype MDB_txn_flags = MDB_TXN_FLAG CUInt
  deriving (Eq, Num, Bits, FiniteBits, Storable)

instance Show MDB_txn_flags where
  showsPrec p0 = showsPrecFlags p0 $ \p f -> case f of
    MDB_RDONLY      -> showString "MDB_RDONLY"
    MDB_TXN_FLAG f' -> showParen (p > 10) $
                       showString "MDB_TXN_FLAG 0x" .
                       showHex f'

--------------------------------------------------------------------------------

instance Monoid MDB_env_flags where
  mempty  = zeroBits
  mappend = (.|.)
  mconcat = foldl' mappend mempty

instance Monoid MDB_db_flags where
  mempty  = zeroBits
  mappend = (.|.)
  mconcat = foldl' mappend mempty

instance Monoid MDB_write_flags where
  mempty  = zeroBits
  mappend = (.|.)
  mconcat = foldl' mappend mempty

instance Monoid MDB_copy_flags where
  mempty  = zeroBits
  mappend = (.|.)
  mconcat = foldl' mappend mempty

instance Monoid MDB_txn_flags where
  mempty  = zeroBits
  mappend = (.|.)
  mconcat = foldl' mappend mempty

--------------------------------------------------------------------------------

-- |
-- = Cursor get operations

newtype MDB_cursor_op = MDB_cursor_op (#type MDB_cursor_op)
  deriving (Eq, Num)

pattern MDB_FIRST          :: MDB_cursor_op
pattern MDB_FIRST          = #const MDB_FIRST
pattern MDB_FIRST_DUP      :: MDB_cursor_op
pattern MDB_FIRST_DUP      = #const MDB_FIRST_DUP
pattern MDB_GET_BOTH       :: MDB_cursor_op
pattern MDB_GET_BOTH       = #const MDB_GET_BOTH
pattern MDB_GET_BOTH_RANGE :: MDB_cursor_op
pattern MDB_GET_BOTH_RANGE = #const MDB_GET_BOTH_RANGE
pattern MDB_GET_CURRENT    :: MDB_cursor_op
pattern MDB_GET_CURRENT    = #const MDB_GET_CURRENT
pattern MDB_GET_MULTIPLE   :: MDB_cursor_op
pattern MDB_GET_MULTIPLE   = #const MDB_GET_MULTIPLE
pattern MDB_LAST           :: MDB_cursor_op
pattern MDB_LAST           = #const MDB_LAST
pattern MDB_LAST_DUP       :: MDB_cursor_op
pattern MDB_LAST_DUP       = #const MDB_LAST_DUP
pattern MDB_NEXT           :: MDB_cursor_op
pattern MDB_NEXT           = #const MDB_NEXT
pattern MDB_NEXT_DUP       :: MDB_cursor_op
pattern MDB_NEXT_DUP       = #const MDB_NEXT_DUP
pattern MDB_NEXT_MULTIPLE  :: MDB_cursor_op
pattern MDB_NEXT_MULTIPLE  = #const MDB_NEXT_MULTIPLE
pattern MDB_NEXT_NODUP     :: MDB_cursor_op
pattern MDB_NEXT_NODUP     = #const MDB_NEXT_NODUP
pattern MDB_PREV           :: MDB_cursor_op
pattern MDB_PREV           = #const MDB_PREV
pattern MDB_PREV_DUP       :: MDB_cursor_op
pattern MDB_PREV_DUP       = #const MDB_PREV_DUP
pattern MDB_PREV_NODUP     :: MDB_cursor_op
pattern MDB_PREV_NODUP     = #const MDB_PREV_NODUP
pattern MDB_SET            :: MDB_cursor_op
pattern MDB_SET            = #const MDB_SET
pattern MDB_SET_KEY        :: MDB_cursor_op
pattern MDB_SET_KEY        = #const MDB_SET_KEY
pattern MDB_SET_RANGE      :: MDB_cursor_op
pattern MDB_SET_RANGE      = #const MDB_SET_RANGE
pattern MDB_PREV_MULTIPLE  :: MDB_cursor_op
pattern MDB_PREV_MULTIPLE  = #const MDB_PREV_MULTIPLE


-- |
-- == Returncodes / errors

newtype MDB_return_code = MDB_RET_CODE CInt
  deriving (Eq, Num)

pattern MDB_SUCCESS          :: MDB_return_code
pattern MDB_SUCCESS          = #const MDB_SUCCESS
pattern MDB_KEYEXIST         :: MDB_return_code
pattern MDB_KEYEXIST         = #const MDB_KEYEXIST
pattern MDB_NOTFOUND         :: MDB_return_code
pattern MDB_NOTFOUND         = #const MDB_NOTFOUND
pattern MDB_PAGE_NOTFOUND    :: MDB_return_code
pattern MDB_PAGE_NOTFOUND    = #const MDB_PAGE_NOTFOUND
pattern MDB_CORRUPTED        :: MDB_return_code
pattern MDB_CORRUPTED        = #const MDB_CORRUPTED
pattern MDB_PANIC            :: MDB_return_code
pattern MDB_PANIC            = #const MDB_PANIC
pattern MDB_VERSION_MISMATCH :: MDB_return_code
pattern MDB_VERSION_MISMATCH = #const MDB_VERSION_MISMATCH
pattern MDB_INVALID          :: MDB_return_code
pattern MDB_INVALID          = #const MDB_INVALID
pattern MDB_MAP_FULL         :: MDB_return_code
pattern MDB_MAP_FULL         = #const MDB_MAP_FULL
pattern MDB_DBS_FULL         :: MDB_return_code
pattern MDB_DBS_FULL         = #const MDB_DBS_FULL
pattern MDB_READERS_FULL     :: MDB_return_code
pattern MDB_READERS_FULL     = #const MDB_READERS_FULL
pattern MDB_TLS_FULL         :: MDB_return_code
pattern MDB_TLS_FULL         = #const MDB_TLS_FULL
pattern MDB_TXN_FULL         :: MDB_return_code
pattern MDB_TXN_FULL         = #const MDB_TXN_FULL
pattern MDB_CURSOR_FULL      :: MDB_return_code
pattern MDB_CURSOR_FULL      = #const MDB_CURSOR_FULL
pattern MDB_PAGE_FULL        :: MDB_return_code
pattern MDB_PAGE_FULL        = #const MDB_PAGE_FULL
pattern MDB_MAP_RESIZED      :: MDB_return_code
pattern MDB_MAP_RESIZED      = #const MDB_MAP_RESIZED
pattern MDB_INCOMPATIBLE     :: MDB_return_code
pattern MDB_INCOMPATIBLE     = #const MDB_INCOMPATIBLE
pattern MDB_BAD_RSLOT        :: MDB_return_code
pattern MDB_BAD_RSLOT        = #const MDB_BAD_RSLOT
pattern MDB_BAD_TXN          :: MDB_return_code
pattern MDB_BAD_TXN          = #const MDB_BAD_TXN
pattern MDB_BAD_VALSIZE      :: MDB_return_code
pattern MDB_BAD_VALSIZE      = #const MDB_BAD_VALSIZE
pattern MDB_BAD_DBI          :: MDB_return_code
pattern MDB_BAD_DBI          = #const MDB_BAD_DBI

instance Show MDB_return_code where
  showsPrec p rc = case rc of
    MDB_SUCCESS          -> showString "MDB_SUCCESS"
    MDB_KEYEXIST         -> showString "MDB_KEYEXIST"
    MDB_NOTFOUND         -> showString "MDB_NOTFOUND"
    MDB_PAGE_NOTFOUND    -> showString "MDB_PAGE_NOTFOUND"
    MDB_CORRUPTED        -> showString "MDB_CORRUPTED"
    MDB_PANIC            -> showString "MDB_PANIC"
    MDB_VERSION_MISMATCH -> showString "MDB_VERSION_MISMATCH"
    MDB_INVALID          -> showString "MDB_INVALID"
    MDB_MAP_FULL         -> showString "MDB_MAP_FULL"
    MDB_DBS_FULL         -> showString "MDB_DBS_FULL"
    MDB_READERS_FULL     -> showString "MDB_READERS_FULL"
    MDB_TLS_FULL         -> showString "MDB_TLS_FULL"
    MDB_TXN_FULL         -> showString "MDB_TXN_FULL"
    MDB_CURSOR_FULL      -> showString "MDB_CURSOR_FULL"
    MDB_PAGE_FULL        -> showString "MDB_PAGE_FULL"
    MDB_MAP_RESIZED      -> showString "MDB_MAP_RESIZED"
    MDB_INCOMPATIBLE     -> showString "MDB_INCOMPATIBLE"
    MDB_BAD_RSLOT        -> showString "MDB_BAD_RSLOT"
    MDB_BAD_TXN          -> showString "MDB_BAD_TXN"
    MDB_BAD_VALSIZE      -> showString "MDB_BAD_VALSIZE"
    MDB_BAD_DBI          -> showString "MDB_BAD_DBI"
    MDB_RET_CODE i       -> showParen (p > 10) $
                            showString "MDB_RET_CODE 0x" .
                            showHex i

--------------------------------------------------------------------------------

foreign import ccall unsafe mdb_version            :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO CString
foreign import ccall unsafe mdb_strerror           :: MDB_return_code -> CString
foreign import ccall        mdb_env_create         :: Ptr (MDB_env u) -> IO MDB_return_code
foreign import ccall        mdb_env_open           :: MDB_env u -> CString -> MDB_env_flags -> MDB_mode -> IO MDB_return_code
foreign import ccall        mdb_env_copy           :: MDB_env u -> CString -> IO MDB_return_code
foreign import ccall        mdb_env_copyfd         :: MDB_env u -> MDB_filehandle -> IO MDB_return_code
foreign import ccall        mdb_env_copy2          :: MDB_env u -> CString -> MDB_copy_flags -> IO MDB_return_code
foreign import ccall        mdb_env_copyfd2        :: MDB_env u -> MDB_filehandle -> MDB_copy_flags -> IO MDB_return_code
foreign import ccall unsafe mdb_env_stat           :: MDB_env u -> Ptr MDB_stat -> IO MDB_return_code
foreign import ccall unsafe mdb_env_info           :: MDB_env u -> Ptr MDB_envinfo -> IO MDB_return_code
foreign import ccall        mdb_env_sync           :: MDB_env u -> CInt -> IO MDB_return_code
foreign import ccall        mdb_env_close          :: MDB_env u -> IO ()
foreign import ccall unsafe mdb_env_set_flags      :: MDB_env u -> MDB_env_flags -> CInt -> IO MDB_return_code
foreign import ccall unsafe mdb_env_get_flags      :: MDB_env u -> Ptr MDB_env_flags -> IO MDB_return_code
foreign import ccall unsafe mdb_env_get_path       :: MDB_env u -> Ptr CString -> IO MDB_return_code
foreign import ccall unsafe mdb_env_get_fd         :: MDB_env u -> Ptr MDB_filehandle -> IO MDB_return_code
foreign import ccall        mdb_env_set_mapsize    :: MDB_env u -> CSize -> IO MDB_return_code
foreign import ccall unsafe mdb_env_set_maxreaders :: MDB_env u -> CUInt -> IO MDB_return_code
foreign import ccall unsafe mdb_env_get_maxreaders :: MDB_env u -> Ptr CUInt -> IO MDB_return_code
foreign import ccall unsafe mdb_env_set_maxdbs     :: MDB_env u -> MDB_dbi_t -> IO MDB_return_code
foreign import ccall unsafe mdb_env_get_maxkeysize :: MDB_env u -> IO CInt
foreign import ccall unsafe mdb_env_set_userctx    :: MDB_env u -> Ptr u -> IO MDB_return_code
foreign import ccall unsafe mdb_env_get_userctx    :: MDB_env u -> IO (Ptr u)
foreign import ccall unsafe mdb_env_set_assert     :: MDB_env u -> FunPtr (MDB_assert_func u) -> IO MDB_return_code
foreign import ccall        mdb_txn_begin          :: MDB_env u -> MDB_txn u -> MDB_txn_flags -> Ptr (MDB_txn u) -> IO MDB_return_code
foreign import ccall unsafe mdb_txn_env            :: MDB_txn u -> IO (MDB_env u)
foreign import ccall unsafe mdb_txn_id             :: MDB_txn u -> IO MDB_txnid
foreign import ccall        mdb_txn_commit         :: MDB_txn u -> IO MDB_return_code
foreign import ccall        mdb_txn_abort          :: MDB_txn u -> IO ()
foreign import ccall        mdb_txn_reset          :: MDB_txn u -> IO ()
foreign import ccall        mdb_txn_renew          :: MDB_txn u -> IO MDB_return_code
foreign import ccall        mdb_dbi_open           :: MDB_txn u -> CString -> MDB_db_flags -> Ptr (MDB_dbi u k v) -> IO MDB_return_code
foreign import ccall unsafe mdb_stat               :: MDB_txn u -> MDB_dbi u k v -> Ptr MDB_stat -> IO MDB_return_code
foreign import ccall unsafe mdb_dbi_flags          :: MDB_txn u -> MDB_dbi u k v -> Ptr MDB_db_flags -> IO MDB_return_code
foreign import ccall        mdb_dbi_close          :: MDB_env u -> MDB_dbi u k v -> IO ()
foreign import ccall        mdb_drop               :: MDB_txn u -> MDB_dbi u k v -> CInt -> IO MDB_return_code
foreign import ccall unsafe mdb_set_compare        :: MDB_txn u -> MDB_dbi u k v -> FunPtr (MDB_cmp_func k) -> IO MDB_return_code
foreign import ccall unsafe mdb_set_dupsort        :: MDB_txn u -> MDB_dbi u k v -> FunPtr (MDB_cmp_func k) -> IO MDB_return_code
#ifdef LMDB_FIXEDMAP
foreign import ccall unsafe mdb_set_relfunc        :: MDB_txn u -> MDB_dbi u k v -> FunPtr (MDB_rel_func v) -> IO MDB_return_code
foreign import ccall unsafe mdb_set_relctx         :: MDB_txn u -> MDB_dbi u k v -> Ptr () -> IO MDB_return_code
#endif
foreign import ccall        mdb_get                :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> IO MDB_return_code
foreign import ccall        mdb_put                :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> MDB_write_flags -> IO MDB_return_code
foreign import ccall        mdb_del                :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> IO MDB_return_code
foreign import ccall        mdb_cursor_open        :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_cursor u k v)  -> IO MDB_return_code
foreign import ccall        mdb_cursor_close       :: MDB_cursor u k v -> IO ()
foreign import ccall        mdb_cursor_renew       :: MDB_txn u -> MDB_cursor u k v -> IO MDB_return_code
foreign import ccall unsafe mdb_cursor_txn         :: MDB_cursor u k v -> IO (MDB_txn u)
foreign import ccall unsafe mdb_cursor_dbi         :: MDB_cursor u k v -> IO (MDB_dbi u k v)
foreign import ccall        mdb_cursor_get         :: MDB_cursor u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> MDB_cursor_op -> IO MDB_return_code
foreign import ccall        mdb_cursor_put         :: MDB_cursor u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> MDB_write_flags -> IO MDB_return_code
foreign import ccall        mdb_cursor_del         :: MDB_cursor u k v -> MDB_write_flags -> IO MDB_return_code
foreign import ccall        mdb_cursor_count       :: MDB_cursor u k v -> Ptr CSize -> IO MDB_return_code
foreign import ccall        mdb_cmp                :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val k) -> IO CInt
foreign import ccall        mdb_dcmp               :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val k) -> IO CInt
foreign import ccall        mdb_reader_list        :: MDB_env u -> FunPtr (MDB_msg_func a) -> Ptr a -> IO CInt
foreign import ccall        mdb_reader_check       :: MDB_env u -> Ptr CInt -> IO MDB_return_code

-- | User-defined comparison functions for keys.
foreign import ccall "wrapper" wrapCmpFunc ::
  MDB_cmp_func k -> IO (FunPtr (MDB_cmp_func k))

-- | Callback function for reader list.
foreign import ccall "wrapper" wrapMsgFunc ::
  MDB_msg_func a -> IO (FunPtr (MDB_msg_func a))

foreign import ccall unsafe "mdb_get"          unsafe_mdb_get          :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> IO MDB_return_code
foreign import ccall unsafe "mdb_put"          unsafe_mdb_put          :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> MDB_write_flags -> IO MDB_return_code
foreign import ccall unsafe "mdb_del"          unsafe_mdb_del          :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> IO MDB_return_code
foreign import ccall unsafe "mdb_cursor_get"   unsafe_mdb_cursor_get   :: MDB_cursor u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> MDB_cursor_op -> IO MDB_return_code
foreign import ccall unsafe "mdb_cursor_put"   unsafe_mdb_cursor_put   :: MDB_cursor u k v -> Ptr (MDB_val k) -> Ptr (MDB_val v) -> MDB_write_flags -> IO MDB_return_code
foreign import ccall unsafe "mdb_cursor_del"   unsafe_mdb_cursor_del   :: MDB_cursor u k v -> MDB_write_flags -> IO MDB_return_code
foreign import ccall unsafe "mdb_cursor_count" unsafe_mdb_cursor_count :: MDB_cursor u k v -> Ptr CSize -> IO MDB_return_code
foreign import ccall unsafe "mdb_cmp"          unsafe_mdb_cmp          :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val k) -> IO CInt
foreign import ccall unsafe "mdb_dcmp"         unsafe_mdb_dcmp         :: MDB_txn u -> MDB_dbi u k v -> Ptr (MDB_val k) -> Ptr (MDB_val k) -> IO CInt

--------------------------------------------------------------------------------

instance Storable (MDB_val a) where
  alignment _ = #{alignment MDB_val}
  sizeOf _    = #{size MDB_val}
  peek ptr =
    MDB_val
    <$> #{peek MDB_val, mv_size} ptr
    <*> #{peek MDB_val, mv_data} ptr
  poke ptr (MDB_val sz pd) = do
    #{poke MDB_val, mv_size} ptr sz
    #{poke MDB_val, mv_data} ptr pd

instance Storable MDB_stat where
  alignment _ = #{alignment MDB_stat}
  sizeOf _ = #{size MDB_stat}
  peek ptr =
    MDB_stat
    <$> #{peek MDB_stat, ms_psize}          ptr
    <*> #{peek MDB_stat, ms_depth}          ptr
    <*> #{peek MDB_stat, ms_branch_pages}   ptr
    <*> #{peek MDB_stat, ms_leaf_pages}     ptr
    <*> #{peek MDB_stat, ms_overflow_pages} ptr
    <*> #{peek MDB_stat, ms_entries}        ptr
  poke ptr val = do
    #{poke MDB_stat, ms_psize}          ptr (ms_psize val)
    #{poke MDB_stat, ms_depth}          ptr (ms_depth val)
    #{poke MDB_stat, ms_branch_pages}   ptr (ms_branch_pages val)
    #{poke MDB_stat, ms_leaf_pages}     ptr (ms_leaf_pages val)
    #{poke MDB_stat, ms_overflow_pages} ptr (ms_overflow_pages val)
    #{poke MDB_stat, ms_entries}        ptr (ms_entries val)

instance Storable MDB_envinfo where
  alignment _ = #{alignment MDB_envinfo}
  sizeOf _    = #{size MDB_envinfo}
  peek ptr =
    MDB_envinfo
    <$> #{peek MDB_envinfo, me_mapaddr}    ptr
    <*> #{peek MDB_envinfo, me_mapsize}    ptr
    <*> #{peek MDB_envinfo, me_last_pgno}  ptr
    <*> #{peek MDB_envinfo, me_last_txnid} ptr
    <*> #{peek MDB_envinfo, me_maxreaders} ptr
    <*> #{peek MDB_envinfo, me_numreaders} ptr
  poke ptr val = do
    #{poke MDB_envinfo, me_mapaddr}    ptr (me_mapaddr val)
    #{poke MDB_envinfo, me_mapsize}    ptr (me_mapsize val)
    #{poke MDB_envinfo, me_last_pgno}  ptr (me_last_pgno val)
    #{poke MDB_envinfo, me_last_txnid} ptr (me_last_txnid val)
    #{poke MDB_envinfo, me_maxreaders} ptr (me_maxreaders val)
    #{poke MDB_envinfo, me_numreaders} ptr (me_numreaders val)
