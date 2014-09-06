{-# LANGUAGE TemplateHaskell #-}
import Control.Exception (IOException, catch)
import Data.Time.LocalTime (LocalTime(..), TimeOfDay(..))
import System.Environment (getArgs)
import System.FilePath (takeFileName)
import System.IO (hPutStrLn, stderr)
import System.Posix (fileExist, rename)
import qualified Graphics.HsExif as Exif
import Text.Printf.Mauke.TH (printf)

errMsg :: String -> IO ()
errMsg = hPutStrLn stderr

safeRename :: FilePath -> FilePath -> IO ()
safeRename oldPath newPath = do
  e <- fileExist newPath
  if e
    then
      errMsg $
      "File " ++ show newPath ++ " already exists, not renaming " ++ show oldPath
    else do
      putStrLn $ "Rename " ++ oldPath ++ " to " ++ newPath
      rename oldPath newPath

mkExifFileName :: LocalTime -> FilePath -> FilePath
mkExifFileName (LocalTime day (TimeOfDay hour minute sec)) oldPath =
  $(printf "%s_%02d-%02d-%02d_%s") (show day) hour minute secs baseName
  where
    baseName = takeFileName oldPath
    secs :: Integer
    secs = truncate sec

ioErrorHandler :: (IOException -> IO a) -> (IOException -> IO a)
ioErrorHandler = id

exifRename :: FilePath -> IO ()
exifRename path = do
  result <- Exif.parseFileExif path `catch` ioErrorHandler (\e -> return (Left (show e)))
  case result of
    Left err -> errMsg $ "Failed to parse EXIF data from " ++ show path ++ ": " ++ show err
    Right exifTags ->
      case Exif.getDateTimeOriginal exifTags of
      Nothing -> putStrLn $ "No original time in photo: " ++ show path
      Just localTime -> safeRename path $ mkExifFileName localTime path

main :: IO ()
main = do
  filenames <- getArgs
  mapM_ exifRename filenames
