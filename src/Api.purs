module Api where

import Prelude

import Control.Monad.Reader (Reader, ask)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Effect.Aff (Aff)

-- INTERNAL

newtype BaseUrl = BaseUrl String

newtype Token = Token String

headers :: Maybe Token -> Array String
headers maybeToken = []

url :: BaseUrl -> String -> String
url (BaseUrl baseUrl) endpoint =
  baseUrl <> endpoint

type AppContext =
  { baseUrl :: BaseUrl
  , envId :: EnvironmentId
  , token :: Maybe Token
  -- , logLevel :: Debug | ImportantOnly -- https://sourcegraph.com/github.com/purescript/spago/-/blob/spaghetto/core/src/Log.purs
  -- , logger :: Logger
  }

-- PUBLIC

newtype SecretKey = SecretKey String

newtype ClientId = ClientId String

newtype UserId = UserId String

derive instance newtypeUserId :: Newtype UserId _
derive newtype instance eqUserId :: Eq UserId
derive newtype instance ordUserId :: Ord UserId

auth :: ClientId -> SecretKey -> Reader AppContext Token
auth clientId secretKey = do
  appEnv <- ask
  let url' = url appEnv.baseUrl "/auth/access-token"
  let headers' = headers appEnv.token
  let body = { clientId, secret: secretKey }
  pure $ Token "some_jwt"

newtype Environment = Environment
  { id :: EnvironmentId
  , accountId :: Maybe AccountId
  , name :: String
  , isProd :: Boolean
  , guestAuthentication :: Array String
  }

-- | Example: us_env_hVXkXs0b
newtype EnvironmentId = EnvironmentId String

derive instance newtypeEnvironmentId :: Newtype EnvironmentId _
derive newtype instance eqEnvironmentId :: Eq EnvironmentId
derive newtype instance ordEnvironmentId :: Ord EnvironmentId

-- | Example: us_acc_uj6s91wc
newtype AccountId = AccountId String

derive instance newtypeAccountId :: Newtype AccountId _
derive newtype instance eqAccountId :: Eq AccountId
derive newtype instance ordAccountId :: Ord AccountId

-- | Get all environments for an account.
listEnvironments :: Reader AppContext (Array Environment)
listEnvironments = do
  appEnv <- ask
  let url' = url appEnv.baseUrl "/environments"
  let headers' = headers appEnv.token
  -- envsRes <- get json url
  pure []

newtype Agent = Agent
  { id :: AgentId
  , topics :: Array EventTopic
  , compiler :: String
  , source :: String
  }

-- | Example: us_ag_qGZbKwDW
newtype AgentId = AgentId String

derive instance newtypeAgentId :: Newtype AgentId _
derive newtype instance eqAgentId :: Eq AgentId
derive newtype instance ordAgentId :: Ord AgentId

data EventTopic
  = ActionTriggered
  | ClientInit
  | FileDeleted
  | JobCompleted
  | JobDeleted
  | JobFailed
  | JobStarted
  | JobUpdated
  | JobWaiting
  | RecordsCreated
  | RecordsDeleted
  | RecordsUpdated
  | SheetValidated
  | SpaceAdded
  | SpaceRemoved
  | UploadCompleted
  | UploadFailed
  | UploadStarted
  | UserAdded
  | UserOffline
  | UserOnline
  | UserRemoved
  | WorkbookAdded
  | WorkbookRemoved

-- instance decodeJsonEventTopic :: DecodeJson EventTopic where
--   decodeJson json = do

-- | Creates an agent.
createAgent :: Reader AppContext (Agent)
createAgent = do
  appEnv <- ask
  let
    url' = url appEnv.baseUrl $
      "/agents?" <> "environmentId=" <> toString appEnv.envId
  let headers' = headers appEnv.token
  pure $ Agent { id: AgentId "", topics: [ ClientInit ], compiler: "js", source: "" }
  where
  toString :: EnvironmentId -> String
  toString (EnvironmentId envId) = envId
