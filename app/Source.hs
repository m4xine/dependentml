module Source 
  ( Source(..)
  , readSource
  ) where

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