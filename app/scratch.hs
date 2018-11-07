
    {-# LANGUAGE CPP                 #-}
    {-# LANGUAGE FlexibleContexts    #-}
    {-# LANGUAGE LambdaCase          #-}
    {-# LANGUAGE MultiWayIf          #-}
    {-# LANGUAGE OverloadedStrings   #-}
    {-# LANGUAGE ScopedTypeVariables #-}
module Main where

    import           System.Directory
    import           Control.Concurrent
    import           Control.Concurrent.STM.TChan               -- transactional memory FIFO. Read the paper link at: https://hackage.haskell.org/package/stm-2.5.0.0/docs/Control-Monad-STM.html#t:STM
    import qualified Control.Exception                     as E
    import           Control.Lens
    import           Control.Monad
    import           Control.Monad.IO.Class
    import           Control.Monad.Reader
    import           Control.Monad.STM                          -- Shared Transactional Memory Monad.
    import qualified Data.Aeson                            as J -- JSON parser. 
    import           Data.Default
    import qualified Data.HashMap.Strict                   as H -- package `unordered-containers`
    import qualified Data.Text                             as T -- yet another implementation of strings.
    import qualified Language.Haskell.LSP.Control          as CTRL
    import qualified Language.Haskell.LSP.Core             as Core
    import           Language.Haskell.LSP.Diagnostics
    import           Language.Haskell.LSP.Messages   
    import qualified Language.Haskell.LSP.Types            as J -- Special JSON object types
    import qualified Language.Haskell.LSP.Types.Lens       as J -- Special JSON object types
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
    
    servername = "SAD"

    main :: IO ()
    main = do
      run (return ()) >>= \case
        0 -> exitSuccess
        c -> exitWith . ExitFailure $ c
    
    -- ---------------------------------------------------------------------
    
    -- | The argument is a callback to run after the reactor has been started.
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
        Core.setupLogger (Just "/tmp/SAD.log") [] L.DEBUG
        CTRL.run (return (Right ()), dp) (lspHandlers rin) lspOptions (Just "/tmp/SAD-session.log")
    
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
  
    -- | [HACK] This is our quick method to run the parser and get some results that can be used to give diagnostic information.
    onSave :: String -> IO (Either (String, SourcePos) [Text])
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
              registration = J.Registration "SAD-registered" J.WorkspaceExecuteCommand Nothing
            let registrations = J.RegistrationParams (J.List [registration])
            rid <- nextLspReqId
    
            reactorSend $ ReqRegisterCapability $ fmServerRegisterCapabilityRequest rid registrations
    
            -- -- example of showMessageRequest
            -- let
            --   params = J.ShowMessageRequestParams J.MtWarning "choose an option for YYY"
            --                    (Just [J.MessageActionItem "option a", J.MessageActionItem "option b"])
            -- -- make a new reference id.
            -- rid1 <- nextLspReqId 
    
            -- reactorSend $ ReqShowMessage $ fmServerShowMessageRequest rid1 params
            -- -- [TODO] how would I retreive the information about which option was clicked?
    
          -- ------------------------------
          {- This is called when a text document opens, it is a good place to initialise state for a document. -}
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
    
          -- -------------------------------
          {- [NOTE] in order for this to fire you need to uncomment the relevant line in `lspHandlers`. -}
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
                fileName = J.uriToFilePath doc
            liftIO $ U.logs $ "********* fileName=" ++ show fileName
            case fileName of
                Just fileName ->  do
                      --sendMessage J.MtInfo "save detected"
                      result <- liftIO $ onSave fileName
                      case result of
                        Left (msg,pos) -> do -- error case
                          liftIO $ U.logs $ show pos
                          sendADiagnostic doc (Just 1) (T.pack msg) J.DsError (sourceLine pos - 1, sourceColumn pos - 1, sourceLine pos - 1, sourceColumn pos + 1)
                        Right happy -> do -- parse worked
                          clearDiagnostics doc Nothing
                          sendMessage J.MtInfo "parse successful"
                          -- [TODO] now verify!
                Nothing -> return ()
            return ()
    
          -- ------------------------------- hover request.
    
          {- This is when you hover over a symbol. [NOTE] in order for this to fire you need to uncomment the relevant line in `lspHandlers`. -}
          HandlerRequest (ReqHover req) -> do
            liftIO $ U.logs $ "reactor:got HoverRequest:" ++ show req
            let J.TextDocumentPositionParams _doc pos = req ^. J.params
                J.Position _l _c' = pos
    
            let
              ht = Just $ J.Hover ms (Just range)
              text = "TYPE INFO GOES HERE" -- (T.pack $ show req) -- just echo the request.
              ms = J.List [J.CodeString $ J.LanguageString servername text ]
              range = J.Range pos pos
            reactorSend $ RspHover $ Core.makeResponseMessage req ht
    
          -- -------------------------------

          {- A CodeAction is a change that the language server offers to make to the source. For example something like "add the missing `import`".
              You can get vscode to list these using `Cmd+.`.
              This just lets the system know what the possible changes are.
              [NOTE] in order for this to fire you need to uncomment the relevant line in `lspHandlers`. 
          -}
    
          HandlerRequest (ReqCodeAction req) -> do
            liftIO $ U.logs $ "reactor:got CodeActionRequest:" ++ show req
            let params = req ^. J.params
                doc = params ^. J.textDocument
                -- fileName = drop (length ("file://"::String)) doc
                -- J.Range from to = J._range (params :: J.CodeActionParams)
                (J.List diags) = params ^. J.context . J.diagnostics
            -- let
            --   -- makeCommand only generates commands for diagnostics whose source is us
            --   makeCommand (J.Diagnostic (J.Range start _) _s _c (Just _) _m _l) = [J.Command title cmd cmdparams]
            --     where
            --       title = "Apply LSP hello command:" <> head (T.lines _m)
            --       -- [NOTE]: the cmd needs to be registered via the InitializeResponse message. See lspOptions above
            --       cmd = "SAD-command"
            --       -- need 'file' and 'start_pos'
            --       args = J.List
            --               [ J.Object $ H.fromList [("file",     J.Object $ H.fromList [("textDocument",J.toJSON doc)])]
            --               , J.Object $ H.fromList [("start_pos",J.Object $ H.fromList [("position",    J.toJSON start)])]
            --               ]
            --       cmdparams = Just args
            --   makeCommand (J.Diagnostic _r _s _c _source _m _l) = []
            -- [NOTE] for now, don't return any code actions.
            let body = J.List [] -- $ map J.CACommand $ concatMap makeCommand diags
                rsp = Core.makeResponseMessage req body
            reactorSend $ RspCodeAction rsp
    
          -- -------------------------------
    
          {- Once the user has clicked on the command, it sends back an execute command request and we in turn send back a suggested edit.
          [NOTE] in order for this to fire you need to uncomment the relevant line in `lspHandlers`. 
          -}
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
    
    sendMessage :: J.MessageType -> T.Text -> R () ()
    sendMessage severity text = do
      let ps = J.ShowMessageRequestParams severity text Nothing
      rid1 <- nextLspReqId
      reactorSend $ ReqShowMessage $ fmServerShowMessageRequest rid1 $ ps

    {-| Remove all diagnostics. -}
    clearDiagnostics :: J.Uri -> J.TextDocumentVersion -> R () ()
    clearDiagnostics fileUri version = publishDiagnostics 100 fileUri version (partitionBySource [])
    {-|  Sends a single diagnostic message.
         Remember that the client doesn't do any diagnostic merging so you have to send the entire list for a particular file every time.
    -}
    sendADiagnostic :: J.Uri -> Maybe Int -> T.Text -> J.DiagnosticSeverity -> (Int,Int,Int,Int) -> R () ()
    sendADiagnostic fileUri version msg sev (sl,sc,fl,fc) = do
        let
          diags = [
            J.Diagnostic
                  (J.Range (J.Position sl sc) (J.Position fl fc))
                  (Just sev)  -- severity
                  Nothing  -- code
                  (Just servername) -- source
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
                     , Core.executeCommandProvider = Just (J.ExecuteCommandOptions (J.List ["SAD-command"]))
                     }
    {-| This is where you say what the capabilities are of the language server. -}
    lspHandlers :: TChan ReactorInput -> Core.Handlers
    lspHandlers rin
      = def { Core.initializedHandler                       = Just $ passHandler rin NotInitialized
            --, Core.hoverHandler                             = Just $ passHandler rin ReqHover
            , Core.didOpenTextDocumentNotificationHandler   = Just $ passHandler rin NotDidOpenTextDocument
            , Core.didSaveTextDocumentNotificationHandler   = Just $ passHandler rin NotDidSaveTextDocument
            --, Core.didChangeTextDocumentNotificationHandler = Just $ passHandler rin NotDidChangeTextDocument
            , Core.didCloseTextDocumentNotificationHandler  = Just $ passHandler rin NotDidCloseTextDocument
            , Core.cancelNotificationHandler                = Just $ passHandler rin NotCancelRequestFromClient
            , Core.responseHandler                          = Just $ responseHandlerCb rin
            --, Core.codeActionHandler                        = Just $ passHandler rin ReqCodeAction
            --, Core.executeCommandHandler                    = Just $ passHandler rin ReqExecuteCommand
            }
    
    -- ---------------------------------------------------------------------
    
    passHandler :: TChan ReactorInput -> (a -> FromClientMessage) -> Core.Handler a
    passHandler rin c notification = do
      atomically $ writeTChan rin (HandlerRequest (c notification))
    
    -- ---------------------------------------------------------------------
    
    responseHandlerCb :: TChan ReactorInput -> Core.Handler J.BareResponseMessage
    responseHandlerCb _rin resp = do
      U.logs $ "******** got ResponseMessage, ignoring:" ++ show resp
    
      