module Main where

import           Data.Semigroup      ((<>))
import           DotGen              (generateDot)
import           HTMLGen             (generateHTML)
import           Options.Applicative
import           Parser
import           Scanner
import           System.Directory    (doesFileExist)


data Command = ShowTokens
             | ShowAST
             | GenerateHTML
             | GenerateDot
  deriving (Eq)

data Config = Config
  { optCommand :: Command
  , outfile    :: Maybe FilePath
  , infile     :: FilePath
  }

config = Config
  <$> hsubparser
    ( command "tokens"   (info (pure ShowTokens)
                               (progDesc "gescannte Tokens ausgeben"))
   <> command "ast"      (info (pure ShowAST)
                               (progDesc "durch den Parser erzeugten AST ausgeben"))
   <> command "html"     (info (pure GenerateHTML)
                               (progDesc "HTML erzeugen"))
   <> command "dot"      (info (pure GenerateDot)
                               (progDesc "AST in dot-Syntax ausgeben"))
    )
  <*> optional (strOption
    ( long "output"
   <> short 'o'
   <> metavar "OUTFILE"
   <> help "Ausgabe in Datei umleiten"
    ))
  <*> argument str
    ( metavar "INFILE"
   <> help "Quell-Datei im Markdown-Format"
    )

main :: IO ()
main = main' =<< execParser opts
  where
    opts = info (config <**> helper)
      ( fullDesc
     <> progDesc "Markdown-To-HTML-Compiler"
     <> header "md2html"
      )

main' config = do
  fileExist <- doesFileExist $ infile config
  if not fileExist
    then putStrLn $ "error: Die Datei " ++ infile config ++ " existiert nicht!"
    else do
    -- lese den Inhalt der Datei "test.md" als einen kompletten String ein
      input <- readFile $ infile config
      -- versuche den String zu scannen
      let maybeTokens = scan input
      let maybeAST = maybe Nothing parse maybeTokens
      let html = maybe "" generateHTML maybeAST
      let dot = maybe "" generateDot maybeAST
      case optCommand config of
        ShowTokens -> maybe
          (putStrLn "error: scanner failed")
          (stdoutOrFile config . show)
          maybeTokens
        ShowAST -> maybe
          (putStrLn "error: parser or scanner failed")
          (stdoutOrFile config . show)
          maybeAST
        GenerateHTML -> stdoutOrFile config html
        GenerateDot -> stdoutOrFile config dot

stdoutOrFile :: Config -> String -> IO ()
stdoutOrFile config output =
    maybe (putStrLn output) (`writeFile` output) $ outfile config
