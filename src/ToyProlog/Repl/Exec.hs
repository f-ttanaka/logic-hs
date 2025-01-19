module ToyProlog.Repl.Exec where

import Control.Exception.Safe
import System.Console.Repline hiding (ReplOpts (..))
import ToyProlog.Common
import ToyProlog.Core.Solve

type Repl = HaskelineT (Solve IO)

-- discard error and continue to run repl
continue :: (MonadIO m, MonadCatch m) => m () -> m ()
continue m = m `catch` \(SomeException e) -> print e

exec :: String -> Repl ()
exec source = continue $ do
  return ()

-- evalFileContent ::
--   (MonadTrans t, MonadIO m, MonadIO (t (Exec m)), MonadCatch m) =>
--   FilePath ->
--   t (Exec m) ()
-- evalFileContent fp = do
--   stmts <- liftIO $ withFile fp ReadMode $ \hdl -> do
--     c <- hGetContents hdl
--     doParseFile parseFileContents fp (pack c)
--   lift $ mapM_ evalStmt stmts

-------------------------------------------------------------------------------
-- Command Options
-------------------------------------------------------------------------------

-- :q
quit :: String -> Repl ()
quit _ = exitSuccess

-- :l
-- loadFile :: String -> Repl ()
-- loadFile fName = continue $ evalFileContent fName

options :: Options Repl
options =
  [ ("q", quit)
  ]

-------------------------------------------------------------------------------
-- REPL details
-------------------------------------------------------------------------------

defaultMatcher :: [(String, CompletionFunc m)]
defaultMatcher = []

-- Default tab completer
comp :: (Monad m) => WordCompleter m
comp n = do
  let cmds = [":q", ":t", ":l"]
  return $ filter (isPrefixOf n) cmds

completer :: CompleterStyle (Solve IO)
completer = Prefix (wordCompleter comp) defaultMatcher

ini :: Repl ()
ini = return ()

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

-------------------------------------------------------------------------------
-- Entry Point
-------------------------------------------------------------------------------

runRepl :: IO ()
runRepl =
  runExec $
    evalRepl
      (const . pure $ "repl> ")
      exec
      options
      (Just ':')
      (Just "paste")
      completer
      ini
      final
