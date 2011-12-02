module Archive.Invariants (
    listPersonalDocInvariantProblems
  , listSupervisedDocInvariantProblems
  ) where

import Data.List
import Data.Maybe

import Doc.DocStateData
import User.Model
import Util.SignatoryLinkUtils

listPersonalDocInvariantProblems :: [(User, [Document])] -> [String]
listPersonalDocInvariantProblems =
  listInvariantProblems archiveInvariantsByUser . mkPairs

listSupervisedDocInvariantProblems :: [(User, [Document])] -> [String]
listSupervisedDocInvariantProblems =
  listInvariantProblems archiveInvariantsByCompany . mkPairs

mkPairs :: [(a, [b])] -> [(a, b)]
mkPairs = concatMap (\(x, ys) -> map (\y -> (x, y)) ys)

listInvariantProblems :: [(User, Document) -> Maybe String] -> [(User, Document)] -> [String]
listInvariantProblems invariants = catMaybes . map (invariantProblems invariants)

invariantProblems :: [(User, Document) -> Maybe String] -> (User, Document) -> Maybe String
invariantProblems invariants val =
  case catMaybes $ map (\inv -> inv val) invariants of
    [] -> Nothing
    problems -> Just $ mkLabel val ++ " : " ++ intercalate ";" problems
  where
    mkLabel (user, doc) = "userid = " ++ (show $ userid user) ++ " docid = " ++ (show $ documentid doc)

archiveInvariantsByUser :: [(User, Document) -> Maybe String]
archiveInvariantsByUser = [ userIsSigLinkInDocument
                          , userSigLinkHasMatchingCompany
                          ]

archiveInvariantsByCompany :: [(User, Document) -> Maybe String]
archiveInvariantsByCompany = [ userHasACompany
                             , usersCompanyIsSigLinkInDocument
                             ]

userIsSigLinkInDocument :: (User, Document) -> Maybe String
userIsSigLinkInDocument (user, document) =
  let msiglink = getSigLinkFor document (userid user)
      msiglinksigid = msiglink >>= maybesignatory in
  assertInvariant ("users must have siglink in the document")
                  (Just (userid user) == msiglinksigid)

userSigLinkHasMatchingCompany :: (User, Document) -> Maybe String
userSigLinkHasMatchingCompany (user, document) =
  let msiglink = getSigLinkFor document (userid user)
      msiglinkcompany = msiglink >>= maybecompany in
  assertInvariant ("users sig link must have same company as user " ++
                   "(expected " ++ show (usercompany user) ++ " got " ++ show msiglinkcompany ++ ")")
                   (usercompany user == msiglinkcompany)

userHasACompany :: (User, Document) -> Maybe String
userHasACompany (user, _document) =
  assertInvariant ("user should have a company to see supervised docs")
                  (isJust $ usercompany user)

usersCompanyIsSigLinkInDocument :: (User, Document) -> Maybe String
usersCompanyIsSigLinkInDocument (user, document) =
  let msiglink = (usercompany user) >>= getSigLinkFor document
      msiglinkcompanyid = msiglink >>= maybecompany in
  assertInvariant ("companies must have siglink in the document")
                  (usercompany user == msiglinkcompanyid)

assertInvariant :: String -> Bool -> Maybe String
assertInvariant _ True = Nothing
assertInvariant s False  = Just s
