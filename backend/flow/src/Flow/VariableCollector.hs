{-# LANGUAGE StrictData #-}
module Flow.VariableCollector
    ( Role(..)
    , DocRoleFor(..)
    , CollectorDocUserAssociation
    , FlowVariables(..)
    , collectVariables
    )
  where

import Data.Set hiding (fold, foldl')

import Flow.HighTongue
import Flow.Model.Types (DocRoleFor(..), Role(..))

type CollectorDocUserAssociation = DocRoleFor UserName DocumentName

data FlowVariables = FlowVariables
    { users :: Set UserName
    , documents :: Set DocumentName
    , messages :: Set MessageName
    , documentUserAssociation :: Set CollectorDocUserAssociation
    }
  deriving (Eq, Ord, Show)

instance Semigroup FlowVariables where
  a <> b = FlowVariables
    { users                   = users a <> users b
    , documents               = documents a <> documents b
    , messages                = messages a <> messages b
    , documentUserAssociation = documentUserAssociation a <> documentUserAssociation b
    }

instance Monoid FlowVariables where
  mempty = FlowVariables { users                   = mempty
                         , documents               = mempty
                         , messages                = mempty
                         , documentUserAssociation = mempty
                         }

collectVariables :: HighTongue -> FlowVariables
collectVariables HighTongue {..} = foldl' toVariables mempty stages
  where
    toVariables :: FlowVariables -> Stage -> FlowVariables
    toVariables variables Stage {..} =
      variables <> actionToVariables stageActions <> expectToVariables stageExpect

    expectToVariables :: Set Expect -> FlowVariables
    expectToVariables stageExpect = foldMap unexpect $ toList stageExpect

    actionToVariables :: [SystemAction] -> FlowVariables
    actionToVariables = foldMap unaction

    unaction :: SystemAction -> FlowVariables
    unaction Notify {..} =
      FlowVariables (fromList actionUsers) mempty (singleton actionMessage) mempty
    unexpect :: Expect -> FlowVariables
    unexpect ReceivedData {..} = mempty
    unexpect ApprovedBy {..} =
      FlowVariables (fromList expectUsers) (fromList expectDocuments) mempty
        $ associate Approver expectUsers expectDocuments
    unexpect SignedBy {..} =
      FlowVariables (fromList expectUsers) (fromList expectDocuments) mempty
        $ associate SigningParty expectUsers expectDocuments
    unexpect ViewedBy {..} =
      FlowVariables (fromList expectUsers) (fromList expectDocuments) mempty
        $ associate Viewer expectUsers expectDocuments

    associate :: Role -> [UserName] -> [DocumentName] -> Set CollectorDocUserAssociation
    associate role users documents = fromList $ DocRoleFor role <$> users <*> documents
