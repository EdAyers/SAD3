
    {-# LANGUAGE CPP                 #-}
    {-# LANGUAGE FlexibleContexts    #-}
    {-# LANGUAGE LambdaCase          #-}
    {-# LANGUAGE MultiWayIf          #-}
    {-# LANGUAGE OverloadedStrings   #-}
    {-# LANGUAGE ScopedTypeVariables #-}
module Main where

    
    import           System.Directory
    import           Control.Concurrent
    import           Control.Concurrent.STM.TChan     -- transactional memory FIFO [https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Monad-STM.html#t:STM]
    import qualified Control.Exception                     as E
    import           Control.Lens  -- lenses let you have `^.` and let you easily modify stuff inside deeply nested data.
    import           Control.Monad
    import           Control.Monad.IO.Class
    import           Control.Monad.Reader
    import           Control.Monad.STM -- the STM monad
    import qualified Data.Aeson                            as J -- JSON parser. 
    import           Data.Default
    import qualified Data.HashMap.Strict                   as H -- package `unordered-containers`
    import qualified Data.Text                             as T -- yet another implementation of strings.
    import qualified Language.Haskell.LSP.Control          as CTRL
    import qualified Language.Haskell.LSP.Core             as Core
    import           Language.Haskell.LSP.Diagnostics
    import           Language.Haskell.LSP.Messages   
    import qualified Language.Haskell.LSP.Types            as J -- Special JSON object types
    import qualified Language.Haskell.LSP.Types.Lens       as J -- ditto
    import qualified Language.Haskell.LSP.Utility          as U
    import           Language.Haskell.LSP.VFS
    import           System.Exit
    import qualified System.Log.Logger                     as L
    import qualified Yi.Rope                               as Yi -- Yi is a text editor written in Haskell and apparently we are using their implementation of strings.

    import           Control.Monad.Trans.Except
    
    import Alice.Data.Instr
    import Alice.Import.Reader
    import Alice.Data.Text.Block
    import Alice.Core.Position
    
    
    -- ---------------------------------------------------------------------
    {-# ANN module ("HLint: ignore Eta reduce"         :: String) #-}
    {-# ANN module ("HLint: ignore Redundant do"       :: String) #-}
    {-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}
    -- ---------------------------------------------------------------------
    --
    
    main :: IO ()
    main = do
      run (return ()) >>= \case
        0 -> exitSuccess
        c -> exitWith . ExitFailure $ c
    
    -- ---------------------------------------------------------------------
    
    -- | The argument is the thing to run after the reactor has been started.
    run :: IO () -> IO Int
    run dispatcherProc = flip E.catches handlers $ do
      -- make a new transactional memory channel. This is a buffer with all of the incoming messages.
      rin  <- atomically newTChan :: IO (TChan ReactorInput)
    
      let
        dp lf = do
          U.logs "main.run:dp entered"
          _rpid  <- forkIO $ reactor lf rin
          U.logs "main.run:dp tchan"
          dispatcherProc
          U.logs "main.run:dp after dispatcherProc"
          return Nothing
    
      flip E.finally finalProc $ do
        Core.setupLogger (Just "/tmp/lsp-hello.log") [] L.DEBUG
        CTRL.run (return (Right ()), dp) (lspHandlers rin) lspOptions (Just "/tmp/lsp-hello-session.log")
    
      where
        handlers = [ E.Handler ioExcept
                   , E.Handler someExcept
                   ]
        finalProc = L.removeAllHandlers
        ioExcept   (e :: E.IOException)       = print e >> return 1
        someExcept (e :: E.SomeException)     = print e >> return 1
    
    -- ---------------------------------------------------------------------
    
    -- The reactor is a process that serialises and buffers all requests from the
    -- LSP client, so they can be sent to the backend compiler one at a time, and a
    -- reply sent.
    
    data ReactorInput
      = HandlerRequest FromClientMessage
          -- ^ injected into the reactor input by each of the individual callback handlers
    
    -- ---------------------------------------------------------------------
    
    -- | The monad used in the reactor
    type R c a = ReaderT (Core.LspFuncs c) IO a
    
    -- ---------------------------------------------------------------------
    -- reactor monad functions
    -- ---------------------------------------------------------------------
    
    -- ---------------------------------------------------------------------
    
    reactorSend :: FromServerMessage -> R () ()
    reactorSend msg = do
      lf <- ask
      liftIO $ Core.sendFunc lf msg
    
    -- ---------------------------------------------------------------------
    
    publishDiagnostics :: Int -> J.Uri -> J.TextDocumentVersion -> DiagnosticsBySource -> R () ()
    publishDiagnostics maxToPublish uri v diags = do
      lf <- ask
      liftIO $ (Core.publishDiagnosticsFunc lf) maxToPublish uri v diags
    
    -- ---------------------------------------------------------------------
    
    nextLspReqId :: R () J.LspId
    nextLspReqId = do
      f <- asks Core.getNextReqId
      liftIO f
    
    -- ---------------------------------------------------------------------
  
    onSave fileName = do
      result <- runExceptT $ parse fileName
      U.logs $ "made it after parse"
      return result

    -- | The single point that all events flow through, allowing management of state
    -- to stitch replies and requests together from the two asynchronous sides: lsp
    -- server and backend compiler
    reactor :: Core.LspFuncs () -> TChan ReactorInput -> IO ()
    reactor lf inp = do
      U.logs "reactor:entered"
      flip runReaderT lf $ forever $ do
        -- pull the next message from the message queue. Note that if it is empty this thread blocks.
        inval <- liftIO $ atomically $ readTChan inp
        case inval of
    
          -- Handle any response from a message originating at the server, such as
          -- "workspace/applyEdit"
          HandlerRequest (RspFromClient rm) -> do
            liftIO $ U.logs $ "reactor:got RspFromClient:" ++ show rm
    
          -- -------------------------------
    
          HandlerRequest (NotInitialized _notification) -> do
            liftIO $ U.logm "****** reactor: processing Initialized Notification"
            -- Server is ready, register any specific capabilities we need
    
             {-
             Example:
             {
                     "method": "client/registerCapability",
                     "params": {
                             "registrations": [
                                     {
                                             "id": "79eee87c-c409-4664-8102-e03263673f6f",
                                             "method": "textDocument/willSaveWaitUntil",
                                             "registerOptions": {
                                                     "documentSelector": [
                                                             { "language": "javascript" }
                                                     ]
                                             }
                                     }
                             ]
                     }
             }
            -}
            let
              registration = J.Registration "lsp-hello-registered" J.WorkspaceExecuteCommand Nothing
            let registrations = J.RegistrationParams (J.List [registration])
            rid <- nextLspReqId
    
            reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations
    
            -- example of showMessageRequest
            let
              params = J.ShowMessageRequestParams J.MtWarning "choose an option for YYY"
                               (Just [J.MessageActionItem "option a", J.MessageActionItem "option b"])
            -- make a new reference id.
            rid1 <- nextLspReqId 
    
            reactorSend $ ReqShowMessage $ fmServerShowMessageRequest rid1 params
            -- [TODO] how would I retreive the information about which option was clicked?
    
          -- ------------------------------
          HandlerRequest (NotDidOpenTextDocument notification) -> do
            liftIO $ U.logm "****** reactor: processing NotDidOpenTextDocument"
            let
                doc  = notification ^. J.params
                                     . J.textDocument
                                     . J.uri
                version  = notification ^. J.params
                                     . J.textDocument
                                     . J.version
                fileName =  J.uriToFilePath doc
            liftIO $ U.logs $ "********* fileName=" ++ show fileName
            sendDiagnostics doc (Just 0) (T.pack $ show version)
    
          -- -------------------------------
    
          HandlerRequest (NotDidChangeTextDocument notification) -> do
            let doc :: J.Uri
                doc  = notification ^. J.params
                                     . J.textDocument
                                     . J.uri
            mdoc <- liftIO $ Core.getVirtualFileFunc lf doc
            case mdoc of
              Just (VirtualFile _version str) -> do
                liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: vf got:" ++ (show $ Yi.toString str)
              Nothing -> do
                liftIO $ U.logs "reactor:processing NotDidChangeTextDocument: vf returned Nothing"
    
            liftIO $ U.logs $ "reactor:processing NotDidChangeTextDocument: uri=" ++ (show doc)
    
          -- -------------------------------
    
          HandlerRequest (NotDidSaveTextDocument notification) -> do
            liftIO $ U.logm "****** reactor: processing NotDidSaveTextDocument"
            let
                doc  = notification ^. J.params
                                     . J.textDocument
                                     . J.uri
                --version = notification ^. J.params . J.textDocument . J.version
                fileName = J.uriToFilePath doc
            liftIO $ U.logs $ "********* fileName=" ++ show fileName
            -- run parser here.
            case fileName of
                Just fileName ->  do
                      --blocks <- liftIO $ onSave fileName
                      --let text = T.pack $ show $ head blocks
                  --case text of
                    --Right result -> do
                      --pwd <- liftIO $ getCurrentDirectory
                      --let r = T.pack $ pwd
                      ---- make an info message that sends back the filename
                      -- let ps = J.ShowMessageRequestParams J.MtInfo r Nothing
                      -- rid1 <- nextLspReqId
                      --reactorSend $ ReqShowMessage $ fmServerShowMessageRequest rid1 $ ps
                      sendDiag2 doc Nothing "hello" J.DsError (3, 0, 4, 0)
                    --Left e -> return ()
                Nothing -> return ()
            return ()
    
          -- -------------------------------
    
          HandlerRequest (ReqRename req) -> do
            liftIO $ U.logs $ "reactor:got RenameRequest:" ++ show req
            let
                _params = req ^. J.params
                _doc  = _params ^. J.textDocument . J.uri
                J.Position _l _c' = _params ^. J.position
                _newName  = _params ^. J.newName
    
            let we = J.WorkspaceEdit
                        Nothing -- "changes" field is deprecated
                        (Just (J.List [])) -- populate with actual changes from the rename
            let rspMsg = Core.makeResponseMessage req we
            reactorSend $ RspRename rspMsg
    
          -- ------------------------------- hover request.
    
          HandlerRequest (ReqHover req) -> do
            liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
            let J.TextDocumentPositionParams _doc pos = req ^. J.params
                J.Position _l _c' = pos
    
            let
              ht = Just $ J.Hover ms (Just range)
              ms = J.List [J.CodeString $ J.LanguageString "lsp-hello" (T.pack $ show req) ]
              range = J.Range pos pos
            reactorSend $ RspHover $ Core.makeResponseMessage req ht
    
          -- -------------------------------
    
          HandlerRequest (ReqCodeAction req) -> do
            liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
            let params = req ^. J.params
                doc = params ^. J.textDocument
                -- fileName = drop (length ("file://"::String)) doc
                -- J.Range from to = J._range (params :: J.CodeActionParams)
                (J.List diags) = params ^. J.context . J.diagnostics
    
            let
              -- makeCommand only generates commands for diagnostics whose source is us
              makeCommand (J.Diagnostic (J.Range start _) _s _c (Just "lsp-hello") _m _l) = [J.Command title cmd cmdparams]
                where
                  title = "Apply LSP hello command:" <> head (T.lines _m)
                  -- NOTE: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
                  cmd = "lsp-hello-command"
                  -- need 'file' and 'start_pos'
                  args = J.List
                          [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
                          , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
                          ]
                  cmdparams = Just args
              makeCommand (J.Diagnostic _r _s _c _source _m _l) = []
            let body = J.List $ map J.CACommand $ concatMap makeCommand diags
                rsp = Core.makeResponseMessage req body
            reactorSend $ RspCodeAction rsp
    
          -- -------------------------------
    
          HandlerRequest (ReqExecuteCommand req) -> do
            liftIO $ U.logs "reactor:got ExecuteCommandRequest:" -- ++ show req
            let params = req ^. J.params
                margs = params ^. J.arguments
    
            liftIO $ U.logs $ "reactor:ExecuteCommandRequest:margs=" ++ show margs
    
            let
              reply v = reactorSend $ RspExecuteCommand $ Core.makeResponseMessage req v
            -- When we get a RefactorResult or HieDiff, we need to send a
            -- separate WorkspaceEdit Notification
              r = J.List [] :: J.List Int
            liftIO $ U.logs $ "ExecuteCommand response got:r=" ++ show r
            case toWorkspaceEdit r of
              Just we -> do
                reply (J.Object mempty)
                lid <- nextLspReqId
                -- reactorSend $ J.RequestMessage "2.0" lid "workspace/applyEdit" (Just we)
                reactorSend $ ReqApplyWorkspaceEdit $ fmServerApplyWorkspaceEditRequest lid we
              Nothing ->
                reply (J.Object mempty)
    
          -- -------------------------------
    
          HandlerRequest om -> do
            liftIO $ U.logs $ "reactor:got HandlerRequest:" ++ show om
    
    -- ---------------------------------------------------------------------
    
    toWorkspaceEdit :: t -> Maybe J.ApplyWorkspaceEditParams
    toWorkspaceEdit _ = Nothing
    
    -- ---------------------------------------------------------------------
    
    -- | Analyze the file and send any diagnostics to the client in a
    -- "textDocument/publishDiagnostics" notification
    -- @deprecated
    sendDiagnostics :: J.Uri -> Maybe Int -> T.Text -> R () ()
    sendDiagnostics fileUri version msg = do
      let
        diags = [J.Diagnostic
                  (J.Range (J.Position 0 1) (J.Position 0 5))
                  (Just J.DsWarning)  -- severity
                  Nothing  -- code
                  (Just "lsp-hello") -- source
                  msg
                  (Just (J.List []))
                ]
      publishDiagnostics 100 fileUri version (partitionBySource diags)
    
    sendDiag2 :: J.Uri -> Maybe Int -> T.Text -> J.DiagnosticSeverity -> (Int,Int,Int,Int) -> R () ()
    sendDiag2 fileUri version msg sev (sl,sc,fl,fc) = do
        let
          diags = [
            J.Diagnostic
                  (J.Range (J.Position sl sc) (J.Position fl fc))
                  (Just sev)  -- severity
                  Nothing  -- code
                  (Just "forthel-sendDiagnostics") -- source
                  msg
                  (Just (J.List []))
            ]
        publishDiagnostics 100 fileUri version (partitionBySource diags)

    -- ---------------------------------------------------------------------
    
    syncOptions :: J.TextDocumentSyncOptions
    syncOptions = J.TextDocumentSyncOptions
      { J._openClose         = Just True
      , J._change            = Just J.TdSyncIncremental
      , J._willSave          = Just False
      , J._willSaveWaitUntil = Just False
      , J._save              = Just $ J.SaveOptions $ Just False
      }
    
    lspOptions :: Core.Options
    lspOptions = def { Core.textDocumentSync = Just syncOptions
                     , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["lsp-hello-command"]))
                     }
    
    lspHandlers :: TChan ReactorInput -> Core.Handlers
    lspHandlers rin
      = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
            , Core.renameHandler                            = Just $ passHandler rin ReqRename
            , Core.hoverHandler                             = Just $ passHandler rin ReqHover
            , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
            , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
            , Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
            , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
            , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
            , Core.responseHandler                          = Just $ responseHandlerCb rin
            , Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
            , Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
            }
    
    -- ---------------------------------------------------------------------
    
    passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
    passHandler rin c notification = do
      atomically $ writeTChan rin (HandlerRequest (c notification))
    
    -- ---------------------------------------------------------------------
    
    responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
    responseHandlerCb _rin resp = do
      U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp
    
      