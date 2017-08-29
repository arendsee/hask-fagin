import Fagin
import Fagin.Prelude
import Fagin.IO

gfffile :: String
gfffile = "sample-data/big.gff3"

main :: IO ()
main = do
  text <- readFile gfffile
  writeResultAndExit
    $   readGff text
    >>= sequence . map model2gff
