module Nixec where

-- base
import System.Exit

-- text
import qualified Data.Text as Text

-- | The `Rule` is the basic item in `Nixec` this is what we are going to write
-- the whole program out of.
data Rule = Rule 
  { ruleName :: Text.Text
  -- ^ Every rule needs a name
  , ruleNeeds :: [ Requirement ]
  -- ^ The rule then have some requirements
  , ruleCommands :: [ Command ]
  -- ^ The the rule has some commands to be executed
  , ruleChecks :: [ Condition ]
  -- ^ And finally a rule has post-conditions. If these are not meet, the rule
  -- have failed to run.
  }

type NixPackage = Text.Text

data Input 
  = RuleInput Rule
  | PackageInput NixPackage
  | FileInput FilePath

-- | The requirement are currently just depending on the output of other 
-- rules to be put in some directories.
data Requirement
  = LinkTo FilePath Input
  | OnPath Input

-- | A convinient wrapper around `LinkTo`
(~>) :: Requirment
fp ~> rule = 
  LinkTo fp rule

-- | Condition
data Condition 
  = FileExist FilePath
  -- ^ Checks that a file exist after running the program
  | ExitCodeWas ExitCode
  -- ^ Check that the exit-code is `ExitCode`.
  | Terminated
  -- ^ Check that the program terminated.
