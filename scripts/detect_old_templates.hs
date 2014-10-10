--- run with: runhaskell -isrc scripts/detect_old_templates.hs
{-# OPTIONS_GHC -Wall -Werror #-}
{- needed packages:
    MissingH
    HStringTemplate
    filemanip
    haskell-src-exts
    kontrakcja-templates
-}
import qualified Data.Set as S
import Data.Maybe
import Control.Applicative
import System.IO
import Language.Haskell.Exts
import Text.StringTemplates.Files
import qualified Data.Map as Map
import Text.StringTemplate
import System.Directory
import Data.List (isSuffixOf)
import Transifex.Synch
import Transifex.Utils
import Debug.Trace

whiteList :: S.Set String
whiteList = S.fromList [ "newTemplateTitle"
                       , "morethenonelist"
                       , "morethenonelistnormal"
                       , "nomorethanonelist"
                       , "nomorethanonelistnormal"
                       , "dumpAllEvidenceTexts"
                       ]

kontraExtensions :: [Extension]
kontraExtensions = map EnableExtension [
    BangPatterns
  , DeriveDataTypeable
  , FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , OverloadedStrings
  , PatternGuards
  , RankNTypes
  , RecordWildCards
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TupleSections
  , TypeFamilies
  , TypeOperators
  , TypeSynonymInstances
  , UndecidableInstances
  , LambdaCase
  ]

------------------------------
-- code for extracting expressions from haskell source files

-- returns list of [recursive] exps (e.g. if a file contains "1+2",
-- it will return [1, 2, 1+2]
fileExps :: FilePath -> IO (S.Set Exp)
fileExps path = do
  parseResult <- parseFileWithMode mode' path
  case parseResult of
    ParseOk module' -> return $ moduleExps module'
    ParseFailed (SrcLoc _ line _) e -> do
      hPutStrLn stderr $ path ++ ":" ++ show line ++ ": " ++ e
      return S.empty
  where mode' = defaultParseMode{ fixities   = Just []
                                , extensions = kontraExtensions
                                }

moduleExps :: Module -> S.Set Exp
moduleExps (Module _ _ _ _ _ _ decls) = S.unions $ map declExps decls

-- TODO: declExps should cover more patterns
declExps :: Decl -> S.Set Exp
declExps (PatBind _ _ _ rhs _) = rhsExps rhs
declExps (FunBind matches) = S.unions $ map matchExps matches
declExps _ = S.empty

matchExps :: Match -> S.Set Exp
matchExps (Match _ _ _ _ rhs binds') = rhsExps rhs `S.union` bindsExps binds'

rhsExps :: Rhs -> S.Set Exp
rhsExps (UnGuardedRhs e) = expExps e
rhsExps (GuardedRhss guardedRhss) = S.unions $ map guardedRhsExps guardedRhss

guardedRhsExps :: GuardedRhs -> S.Set Exp
guardedRhsExps (GuardedRhs _ stmts e) = S.unions (map stmtExps stmts) `S.union` expExps e

expExps :: Exp -> S.Set Exp
expExps e = e `S.insert`
    case e of
      Var _ -> S.empty
      IPVar _ -> S.empty
      Con _ -> S.empty
      Lit _ -> S.empty
      InfixApp e1 _ e2 -> expExps e1 `S.union` expExps e2
      App e1 e2 -> expExps e1 `S.union` expExps e2
      NegApp e' -> expExps e'
      Lambda _ _ e' -> expExps e'
      Let binds' e' -> bindsExps binds' `S.union` expExps e'
      If e1 e2 e3 -> expExps e1 `S.union` expExps e2 `S.union` expExps e3
      Case e' alts -> expExps e' `S.union` S.unions (map altExps alts)
      Do stmts -> S.unions $ map stmtExps stmts
      MDo stmts -> S.unions $ map stmtExps stmts
      Tuple _ exps -> S.unions $ map expExps exps
      TupleSection _ mexps -> S.unions $ map expExps $ catMaybes mexps
      List exps -> S.unions $ map expExps exps
      Paren e' -> expExps e'
      LeftSection e' _ -> expExps e'
      RightSection _ e' -> expExps e'
      RecConstr _ fieldUpdates -> S.unions $ map fieldUpdateExps fieldUpdates
      RecUpdate e' fieldUpdates -> expExps e' `S.union` (S.unions $ map fieldUpdateExps fieldUpdates)
      EnumFrom e' -> expExps e'
      EnumFromTo e1 e2 -> expExps e1 `S.union` expExps e2
      EnumFromThen e1 e2 -> expExps e1 `S.union` expExps e2
      EnumFromThenTo e1 e2 e3 -> expExps e1 `S.union` expExps e2 `S.union` expExps e3
      ListComp e' qualStmts -> expExps e' `S.union` (S.unions $ map qualStmtExps qualStmts)
      ParComp e' qualStmts -> expExps e' `S.union` (S.unions $ map (S.unions . map qualStmtExps) qualStmts)
      ExpTypeSig _ e' _ -> expExps e'
      VarQuote _ -> S.empty
      TypQuote _ -> S.empty
      BracketExp _ -> S.empty
      SpliceExp _ -> S.empty
      QuasiQuote _ _ -> S.empty
      XETag _ _ _ me -> S.unions $ map expExps $ catMaybes [me]
      XTag _ _ _ me es -> S.unions $ map expExps $ catMaybes [me] ++ es
      XPcdata _ -> S.empty
      XExpTag e' -> expExps e'
      XChildTag _ es -> S.unions $ map expExps es
      CorePragma _ _ -> S.empty
      SCCPragma _ _ -> S.empty
      GenPragma _ _ _ _ -> S.empty
      Proc _ _ e' -> expExps e'
      LeftArrApp e1 e2 -> expExps e1 `S.union` expExps e2
      RightArrApp e1 e2 -> expExps e1 `S.union` expExps e2
      LeftArrHighApp e1 e2 -> expExps e1 `S.union` expExps e2
      RightArrHighApp e1 e2 -> expExps e1 `S.union` expExps e2
      MultiIf ifs -> S.unions $ map (\(IfAlt e1 e2) -> expExps e1 `S.union` expExps e2) ifs
      LCase alts -> S.unions $ map altExps alts


bindsExps :: Binds -> S.Set Exp
bindsExps (BDecls decls) = S.unions $ map declExps decls
bindsExps (IPBinds _) = S.empty

altExps :: Alt -> S.Set Exp
altExps (Alt _ _ guardedAlts binds') = guardedAltsExps guardedAlts `S.union` bindsExps binds'

guardedAltsExps :: GuardedAlts -> S.Set Exp
guardedAltsExps (UnGuardedAlt e) = expExps e
guardedAltsExps (GuardedAlts guardedAlts) = S.unions $ map guardedAltExps guardedAlts

guardedAltExps :: GuardedAlt -> S.Set Exp
guardedAltExps (GuardedAlt _ stmts e) = (S.unions $ map stmtExps stmts) `S.union` expExps e

stmtExps :: Stmt -> S.Set Exp
stmtExps (Generator _ _ e) = expExps e
stmtExps (Qualifier e) = expExps e
stmtExps (LetStmt binds') = bindsExps binds'
stmtExps (RecStmt stmts) = S.unions $ map stmtExps stmts

fieldUpdateExps :: FieldUpdate -> S.Set Exp
fieldUpdateExps (FieldUpdate _ e) = expExps e
fieldUpdateExps _ = S.empty

qualStmtExps :: QualStmt -> S.Set Exp
qualStmtExps _ = S.empty
--------------------------------------------------
-- returns list of constructors of EvidenceEventType datatype
-- (parsed from EvidenceLog.Model module)
-- these are needed, because every event types has associated template
elogEvents :: IO (S.Set String)
elogEvents = do
  ParseOk (Module _ _ _ _ _ _ decls) <- parseFileWithExts kontraExtensions "src/EvidenceLog/Model.hs"
  let documentCurrentEvidenceEventDeclCtors (DataDecl _ DataType _ (Ident "CurrentEvidenceEventType") _ ctors _) = Just ctors
      documentCurrentEvidenceEventDeclCtors _ = Nothing
      documentObsoleteEvidenceEventDeclCtors (DataDecl _ DataType _ (Ident "ObsoleteEvidenceEventType") _ ctors _) = Just ctors
      documentObsoleteEvidenceEventDeclCtors _ = Nothing
      currentEventCtorDecls = head $ catMaybes $ map documentCurrentEvidenceEventDeclCtors decls
      obsoleteEventCtorDecls = head $ catMaybes $ map documentObsoleteEvidenceEventDeclCtors decls

      nameToString (Ident x) = x
      nameToString (Symbol x) = x

      eventCtor (QualConDecl _ _ _ (ConDecl name' _)) = nameToString name'
      eventCtor (QualConDecl _ _ _ (InfixConDecl _ name' _)) = nameToString name'
      eventCtor (QualConDecl _ _ _ (RecDecl name' _)) = nameToString name'

  return $ S.fromList $ map eventCtor (currentEventCtorDecls ++ obsoleteEventCtorDecls)

--------------------------------------------------
-- returns template name from expression of certain forms
-- e.g. renderTemplate "foo" returns Just "foo"
-- for expressions that don't look like funcalls to template rendering functions
-- returns nothing
-- TODO: support qualified fun names
expTemplateName :: Exp -> Maybe String
expTemplateName (App (Var (UnQual (Ident funName))) (Lit (String template)))
    | funName `elem` [ "renderTemplate"
                     , "renderTemplate_"
                     , "renderTemplateI"
                     , "flashMessage"
                     , "flashMessageWithFieldName"
                     , "templateName"
                     ] = Just template
    | otherwise = Nothing
expTemplateName (App (App (Var (UnQual (Ident funName))) _) (Lit (String template)))
    | funName `elem` [ "renderLocalTemplate"
                     , "renderLocalTemplate_"
                     , "flashMessageWithFieldName"
                     , "renderTemplateAsPage"] = Just template
    | otherwise = Nothing
expTemplateName (App (App (App (Var (UnQual (Ident funName))) _) _) (Lit (String template)))
    | funName `elem` [ "documentMailWithDocLang"
                     , "kontramail"
                     ] = Just template
    | otherwise = Nothing
expTemplateName (App (App (App (App (Var (UnQual (Ident funName))) _) _) _) (Lit (String template)))
    | funName `elem` [ "documentMail"
                     , "kontramaillocal"
                     ] = Just template
    | otherwise = Nothing
expTemplateName _ = Nothing

-- takes a string template and returns names of (immediately) dependent templates
templateDeps :: String -> S.Set String
templateDeps tmpl = fromMaybe S.empty $ S.fromList <$> immDeps
    where parsedTmpl = newSTMP tmpl :: StringTemplate String
          (_, _, immDeps) = checkTemplate parsedTmpl

-- recursive template dependency scanner
-- takes a map from template names to template strings (all known templates in the system)
-- set of already seen templates (empty in the beginning)
-- names of templates that we wish to recursively scan for dependencies
-- returns list of all (indirectly) dependent template names (of that ^^ set)
go :: Map.Map String String -> S.Set String -> S.Set String -> S.Set String
go allTmpls seenTmpls tmpls | S.null tmpls = seenTmpls
                            | otherwise = go allTmpls seenTmpls' $ newDeps `S.union` tmpls'
    where (tmpl, tmpls') = S.deleteFindMin tmpls
          tmplDef = fromMaybe (trace ("Missing template definition: " ++ tmpl) "") $ Map.lookup tmpl allTmpls
          deps = templateDeps tmplDef
          seenTmpls' = tmpl `S.insert` seenTmpls
          newDeps = deps S.\\ seenTmpls

setCatMaybes :: Ord a => S.Set (Maybe a) -> S.Set a
setCatMaybes = S.fromList . catMaybes . S.toList

main :: IO ()
main = do
  files <- filter (".hs" `isSuffixOf`) <$> directoryFilesRecursive "src"
  exps <- S.unions <$> mapM fileExps files
  let topLevelTemplatesFromSources = setCatMaybes $ S.map expTemplateName exps
  events <- elogEvents
  let elogTemplates = S.map (++"Text") events
  let topLevelTemplates = S.unions [elogTemplates, topLevelTemplatesFromSources, whiteList]
  translations <- fmap concat $ mapM (fetchLocal "en") allResources
  templatesFilesPath <-filter (".st" `isSuffixOf`) <$>  directoryFilesRecursive "templates"
  templates <- concat <$> mapM getTemplates templatesFilesPath
  let templatesMap = Map.fromList $ templates ++ translations
      allTemplates = S.fromList $ Map.keys templatesMap
      knownTemplates = go templatesMap S.empty topLevelTemplates
      unusedTemplates = allTemplates S.\\ knownTemplates
  putStrLn "Templates that could be removed. Please double check them:"
  putStr $ unlines $ filter (not.null) $ S.toList $ unusedTemplates


-- UTILS

directoryEntriesRecursive :: FilePath -- ^ dir path to be searched for recursively
                          -> IO ([FilePath], [FilePath]) -- ^ (list of all subdirs, list of all files)
directoryEntriesRecursive path | "." `isSuffixOf` path = return ([], [])
                               | otherwise = do
  isDir <- doesDirectoryExist path
  if isDir then do
      entries <- getDirectoryContents path
      let properEntries = map ((path ++ "/")++) entries
      results <- mapM directoryEntriesRecursive properEntries
      let (dirs, files) = biConcat results
      return (path:dirs, files)
   else
      return ([], [path])
 where biConcat = (\(x, y) -> (concat x, concat y)) . unzip

directoryFilesRecursive :: FilePath -- ^ dir path to be searched for recursively
                        -> IO [FilePath] -- ^ list of all files in that dir
directoryFilesRecursive path = snd `fmap` directoryEntriesRecursive path

