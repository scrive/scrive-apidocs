{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Data.Bifunctor
import Data.List (isSuffixOf)
import Data.Maybe
import Debug.Trace
import Language.Haskell.Exts
import System.Directory
import System.Exit
import System.IO
import Text.StringTemplate
import Text.StringTemplates.Files
import qualified Data.Map as Map
import qualified Data.Set as S

import AppDir (setupAppPaths)
import ScriptsPrelude
import Transifex.Synch
import Transifex.Utils

whiteList :: S.Set String
whiteList = S.fromList
  [ "newTemplateTitle"
  , "morethenonelist"
  , "nomorethanonelist"
  , "dumpAllEvidenceTexts"
  , "javascriptLocalisation"
                       -- Old evidence events. Calls for rendering are
                       -- convoluted, so it tries to autodetect that
                       -- they are needed.
  , "CancelDocumenElegEvidenceText"
  , "SignatoryLinkVisitedArchive"
                       -- Seems like templates defined as parameters
                       -- are not detected.  See $nicemail() uses.
  , "mailForwardSigningForAuthorMail"
  , "mailForwardSigningForNewSignatory"
  , "mailForwardSigningForNewSignatoryButton"
  ]

kontraExtensions :: [Extension]
kontraExtensions = map
  EnableExtension
  [ BangPatterns
  , ConstraintKinds
  , DeriveDataTypeable
  , DeriveFunctor
  , DeriveGeneric
  , DerivingStrategies
  , FlexibleContexts
  , FlexibleInstances
  , FunctionalDependencies
  , GeneralizedNewtypeDeriving
  , LambdaCase
  , MultiParamTypeClasses
  , MultiWayIf
  , NamedFieldPuns
  , OverloadedLabels
  , OverloadedStrings
  , PatternGuards
  , RankNTypes
  , RecordWildCards
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TupleSections
  , TypeApplications
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  ]

------------------------------
-- code for extracting expressions from haskell source files

-- returns list of [recursive] exps (e.g. if a file contains "1+2",
-- it will return [1, 2, 1+2]
fileExps :: FilePath -> IO (Maybe (S.Set (Exp SrcSpanInfo)))
fileExps path = do
  parseResult <- parseFileWithMode mode' path
  case parseResult of
    ParseOk module'                 -> return . Just . moduleExps $ module'
    ParseFailed (SrcLoc _ line _) e -> do
      hPutStrLn stderr $ path ++ ":" ++ show line ++ ": " ++ e
      return Nothing
  where mode' = defaultParseMode { fixities = Just [], extensions = kontraExtensions }

moduleExps :: Module SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
moduleExps (Module _ _ _ _ decls) = S.unions $ map declExps decls
moduleExps XmlPage{}              = S.empty
moduleExps XmlHybrid{}            = S.empty

-- TODO: declExps should cover more patterns
declExps :: Decl SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
declExps (PatBind _ _ rhs binds') = rhsExps rhs `S.union` bindsExps binds'
declExps (FunBind _ matches) = S.unions $ map matchExps matches
declExps _ = S.empty

matchExps :: Match SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
matchExps (Match _ _ _ rhs binds'       ) = rhsExps rhs `S.union` bindsExps binds'
matchExps (InfixMatch _ _ _ _ rhs binds') = rhsExps rhs `S.union` bindsExps binds'

rhsExps :: Rhs SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
rhsExps (UnGuardedRhs _ e          ) = expExps e
rhsExps (GuardedRhss  _ guardedRhss) = S.unions $ map guardedRhsExps guardedRhss

guardedRhsExps :: GuardedRhs SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
guardedRhsExps (GuardedRhs _ stmts e) = S.unions (map stmtExps stmts) `S.union` expExps e

expExps :: Exp SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
expExps e = e `S.insert` case e of
  Var             _ _            -> S.empty
  OverloadedLabel _ _            -> S.empty
  IPVar           _ _            -> S.empty
  Con             _ _            -> S.empty
  Lit             _ _            -> S.empty
  InfixApp _ e1 _ e2             -> expExps e1 `S.union` expExps e2
  App _ e1 e2                    -> expExps e1 `S.union` expExps e2
  NegApp _ e'                    -> expExps e'
  Lambda _ _      e'             -> expExps e'
  Let    _ binds' e'             -> bindsExps (Just binds') `S.union` expExps e'
  If _ e1 e2 e3                  -> expExps e1 `S.union` expExps e2 `S.union` expExps e3
  Case _ e' alts                 -> expExps e' `S.union` S.unions (map altExps alts)
  Do  _ stmts                    -> S.unions $ map stmtExps stmts
  MDo _ stmts                    -> S.unions $ map stmtExps stmts
  Tuple        _ _ exps          -> S.unions $ map expExps exps
  TupleSection _ _ mexps         -> S.unions . map expExps $ catMaybes mexps
  List  _ exps                   -> S.unions $ map expExps exps
  Paren _ e'                     -> expExps e'
  LeftSection  _ e' _            -> expExps e'
  RightSection _ _  e'           -> expExps e'
  RecConstr    _ _  fieldUpdates -> S.unions $ map fieldUpdateExps fieldUpdates
  RecUpdate _ e' fieldUpdates ->
    expExps e' `S.union` S.unions (map fieldUpdateExps fieldUpdates)
  EnumFrom _ e'             -> expExps e'
  EnumFromTo   _ e1 e2      -> expExps e1 `S.union` expExps e2
  EnumFromThen _ e1 e2      -> expExps e1 `S.union` expExps e2
  EnumFromThenTo _ e1 e2 e3 -> expExps e1 `S.union` expExps e2 `S.union` expExps e3
  ListComp _ e' qualStmts   -> expExps e' `S.union` S.unions (map qualStmtExps qualStmts)
  ParComp _ e' qualStmts ->
    expExps e' `S.union` S.unions (map (S.unions . map qualStmtExps) qualStmts)
  ExpTypeSig _ e' _   -> expExps e'
  VarQuote   _ _      -> S.empty
  TypQuote   _ _      -> S.empty
  BracketExp _ _      -> S.empty
  SpliceExp  _ _      -> S.empty
  QuasiQuote{}        -> S.empty
  TypeApp _ _         -> S.empty
  XETag _ _ _ me      -> S.unions . map expExps $ catMaybes [me]
  XTag _ _ _ me es    -> S.unions . map expExps $ (catMaybes [me] ++ es)
  XPcdata   _ _       -> S.empty
  XExpTag   _ e'      -> expExps e'
  XChildTag _ es      -> S.unions $ map expExps es
  CorePragma{}        -> S.empty
  SCCPragma{}         -> S.empty
  GenPragma{}         -> S.empty
  Proc _ _ e'         -> expExps e'
  LeftArrApp _ e1 e2  -> expExps e1 `S.union` expExps e2
  RightArrApp _ e1 e2 -> expExps e1 `S.union` expExps e2
  LeftArrHighApp _ e1 e2 -> expExps e1 `S.union` expExps e2
  RightArrHighApp _ e1 e2 -> expExps e1 `S.union` expExps e2
  MultiIf _ ifs       -> S.unions $ map guardedRhsExps ifs
  LCase   _ alts      -> S.unions $ map altExps alts
  UnboxedSum _ _ _ e' -> expExps e'
  _                   -> error $ "Unknown AST: " <> show e

bindsExps :: Maybe (Binds SrcSpanInfo) -> S.Set (Exp SrcSpanInfo)
bindsExps Nothing                    = S.empty
bindsExps (Just (BDecls  _ decls  )) = S.unions $ map declExps decls
bindsExps (Just (IPBinds _ ipbinds)) = S.unions $ map ipBindExp ipbinds

altExps :: Alt SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
altExps (Alt _ _ rhs binds') = rhsExps rhs `S.union` bindsExps binds'

ipBindExp :: IPBind SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
ipBindExp (IPBind _ _ e) = S.singleton e

stmtExps :: Stmt SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
stmtExps (Generator _ _ e   ) = expExps e
stmtExps (Qualifier _ e     ) = expExps e
stmtExps (LetStmt   _ binds') = bindsExps (Just binds')
stmtExps (RecStmt   _ stmts ) = S.unions $ map stmtExps stmts

fieldUpdateExps :: FieldUpdate SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
fieldUpdateExps (FieldUpdate _ _ e) = expExps e
fieldUpdateExps _                   = S.empty

qualStmtExps :: QualStmt SrcSpanInfo -> S.Set (Exp SrcSpanInfo)
qualStmtExps _ = S.empty
--------------------------------------------------
-- returns list of constructors of EvidenceEventType datatype
-- (parsed from EvidenceLog.Model module)
-- these are needed, because every event types has associated template
elogEvents :: IO (S.Set String)
elogEvents = do
  ParseOk (Module _ _ _ _ decls) <- parseFileWithExts kontraExtensions
                                                      "backend/lib/EvidenceLog/Model.hs"
  let
    documentCurrentEvidenceEventDeclCtors (DataDecl _ (DataType _) _ (DHead _ (Ident _ "CurrentEvidenceEventType")) ctors _)
      = Just ctors
    documentCurrentEvidenceEventDeclCtors _ = Nothing
    documentObsoleteEvidenceEventDeclCtors (DataDecl _ (DataType _) _ (DHead _ (Ident _ "ObsoleteEvidenceEventType")) ctors _)
      = Just ctors
    documentObsoleteEvidenceEventDeclCtors _ = Nothing
    currentEventCtorDecls =
      head . catMaybes $ map documentCurrentEvidenceEventDeclCtors decls
    obsoleteEventCtorDecls =
      head . catMaybes $ map documentObsoleteEvidenceEventDeclCtors decls

    nameToString (Ident  _ x) = x
    nameToString (Symbol _ x) = x

    eventCtor (QualConDecl _ _ _ (ConDecl _ name' _       )) = nameToString name'
    eventCtor (QualConDecl _ _ _ (InfixConDecl _ _ name' _)) = nameToString name'
    eventCtor (QualConDecl _ _ _ (RecDecl _ name' _       )) = nameToString name'

  return . S.fromList $ map eventCtor (currentEventCtorDecls ++ obsoleteEventCtorDecls)

--------------------------------------------------
-- returns template name from expression of certain forms
-- e.g. renderTemplate "foo" returns Just "foo"
-- for expressions that don't look like funcalls to template rendering functions
-- returns nothing
-- TODO: support qualified fun names
expTemplateName :: Exp SrcSpanInfo -> Maybe String
expTemplateName (App _ (Var _ (UnQual _ (Ident _ funName))) (Lit _ (String _ template _)))
  | funName
    `elem` [ "renderTemplate"
           , "renderTemplate_"
           , "renderTextTemplate"
           , "renderTextTemplate_"
           , "renderTemplateI"
           , "flashMessage"
           , "flashMessageWithFieldName"
           , "templateName"
           , "render"
           ]
  = Just template
  | otherwise
  = Nothing
expTemplateName (App _ (App _ (Var _ (UnQual _ (Ident _ funName))) _) (Lit _ (String _ template _)))
  | funName
    `elem` [ "renderLocalTemplate"
           , "renderLocalTemplate_"
           , "flashMessageWithFieldName"
           , "renderTemplateAsPage"
           ]
  = Just template
  | otherwise
  = Nothing
expTemplateName (App _ (App _ (App _ (Var _ (UnQual _ (Ident _ funName))) _) _) (Lit _ (String _ template _)))
  | funName == "documentMailWithDocLang"
  = Just template
  | otherwise
  = Nothing
expTemplateName (App _ (App _ (App _ (App _ (Var _ (UnQual _ (Ident _ funName))) _) _) _) (Lit _ (String _ template _)))
  | funName `elem` ["documentMail", "kontramail"]
  = Just template
  | otherwise
  = Nothing
expTemplateName (App _ (App _ (App _ (App _ (App _ (Var _ (UnQual _ (Ident _ funName))) _) _) _) _) (Lit _ (String _ template _)))
  | funName == "kontramaillocal"
  = Just template
  | otherwise
  = Nothing
expTemplateName _ = Nothing

-- takes a string template and returns names of (immediately) dependent templates
templateDeps :: String -> S.Set String
templateDeps tmpl = maybe S.empty S.fromList deps
  where
    parsedTmpl      = newSTMP tmpl :: StringTemplate String
    (_, _, immDeps) = checkTemplate parsedTmpl
    deps            = filter (/= "noescape") <$> immDeps

-- Recursive template dependency scanner.
--
-- Takes a map from template names to template strings (all known
-- templates in the system), set of already seen templates (empty in
-- the beginning), and names of templates that we wish to recursively
-- scan for dependencies. Returns a list of all (indirectly) dependent
-- template names (of that ^^ set).
go
  :: S.Set String -> Map.Map String String -> S.Set String -> S.Set String -> S.Set String
go elogTemplates allTmpls seenTmpls tmpls
  | S.null tmpls = seenTmpls
  | otherwise    = go elogTemplates allTmpls seenTmpls' $ newDeps `S.union` tmpls'
  where
    (tmpl, tmpls') = S.deleteFindMin tmpls
    tmplDef        = fromMaybe traceNotFound $ Map.lookup tmpl allTmpls
    traceNotFound  = if tmpl `S.member` elogTemplates
      then ""
      else trace ("Missing template definition: " ++ tmpl) ""
    deps       = templateDeps tmplDef
    seenTmpls' = tmpl `S.insert` seenTmpls
    newDeps    = deps S.\\ seenTmpls

setCatMaybes :: Ord a => S.Set (Maybe a) -> S.Set a
setCatMaybes = S.fromList . catMaybes . S.toList

main :: IO ()
main = do
  _     <- setupAppPaths

  files <- filter (".hs" `isSuffixOf`) <$> directoryFilesRecursive "backend"
  mexps <- mapM fileExps files
  when (Nothing `elem` mexps) exitFailure
  let exps = S.unions . catMaybes $ mexps
  let topLevelTemplatesFromSources = setCatMaybes $ S.map expTemplateName exps
  events <- elogEvents
  let elogTemplates = S.unions
        [ S.map (++ "Log") events
        , S.map (++ "Archive") events
        , S.map (++ "VerificationPages") events
        ]
  let topLevelTemplates =
        S.unions [elogTemplates, topLevelTemplatesFromSources, whiteList]
  translations       <- concat <$> mapM (fetchLocal "en") allResources
  templatesFilesPath <- filter (".st" `isSuffixOf`)
    <$> directoryFilesRecursive "templates"
  templates <- concat <$> mapM getTemplates templatesFilesPath
  let templatesMap    = Map.fromList $ templates ++ translations
      allTemplates    = S.fromList $ Map.keys templatesMap
      knownTemplates  = go elogTemplates templatesMap S.empty topLevelTemplates
      unusedTemplates = allTemplates S.\\ knownTemplates
  let couldBeRemoved = filter (not . null) . S.toList $ unusedTemplates
  if null couldBeRemoved
    then do
      putStrLn "Template check returned no unused templates."
      exitSuccess
    else do
      putStrLn "Templates that could be removed. Please double check them:"
      putStr . unlines $ couldBeRemoved
      exitFailure



-- UTILS

directoryEntriesRecursive
  :: FilePath                    -- ^ dir path to be searched for recursively
  -> IO ([FilePath], [FilePath]) -- ^ (list of all subdirs, list of all files)
directoryEntriesRecursive path
  | "." `isSuffixOf` path = return ([], [])
  | otherwise = do
    isDir <- doesDirectoryExist path
    if isDir
      then do
        entries <- getDirectoryContents path
        let properEntries = map ((path ++ "/") ++) entries
        results <- mapM directoryEntriesRecursive properEntries
        let (dirs, files) = biConcat results
        return (path : dirs, files)
      else return ([], [path])
  where biConcat = bimap concat concat . unzip

directoryFilesRecursive
  :: FilePath      -- ^ dir path to be searched for recursively
  -> IO [FilePath] -- ^ list of all files in that dir
directoryFilesRecursive path = snd `fmap` directoryEntriesRecursive path
