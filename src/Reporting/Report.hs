module Reporting.Report where

import           System.IO


class Report a where
  report :: a -> String

printReport :: Report a => a -> IO ()
printReport = hPutStrLn stderr . report
