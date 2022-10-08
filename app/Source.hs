module Source 
  ( SourcePos
  , Pos
  , Source(..)
  , readSource
  , SrcSpan(..)
  , mkSrcSpan
  , SrcAnn(..)
  ) where

import  Text.Megaparsec    (SourcePos(SourcePos), Pos)
import  Control.Exception  (assert)

-- | Representation of a source file.
data Source = MkSource
  { sourcePath    :: FilePath
  , sourceContent :: Text
  } deriving (Show)

-- | Attemps to read a source file from the specified file path.
readSource :: MonadIO m => FilePath -> m Source
readSource filePath = do
  fileContent <- liftIO $ readFile filePath
  pure MkSource
    { sourcePath = filePath
    , sourceContent = fileContent
    }

-- | A specific portion within a source file.
data SrcSpan = MkSrcSpan
  { srcName   :: FilePath
  , srcBegin  :: !(Pos, Pos) -- ^ (line, col)
  , srcEnd    :: !(Pos, Pos) -- ^ (line, col)
  }

-- | Constructs a 'SrcSpan' from two 'SourcePos'.
-- 
-- This will fail if the source names of the provided 'SourcePos'
-- are not the same or if the beginning 'SourcePos' is greater than
-- the ending 'SourcePos'.
mkSrcSpan 
  :: SourcePos -- ^ Beginning
  -> SourcePos -- ^ Ending 
  -> SrcSpan
mkSrcSpan (SourcePos p bl bc) (SourcePos p' el ec) =
  assert (p == p' && ((bl == el && bc <= ec) || (bl < el))) 
    $ MkSrcSpan p (bl, bc) (el, ec)  

-- | A 'SrcAnn' wraps a 'SrcSpan' with an extra annotation type.
data SrcAnn a = MkSrcAnn
  { ann     :: a
  , annSrc  :: SrcSpan 
  }