-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Events where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Internal.Prelude
import GitHub.Data.URL         (URL)
import Prelude                 ()

-- | Events.
--
-- /TODO:/
--
-- * missing payload, id
data Event = Event
    -- { eventId        :: !(Id Event) -- id can be encoded as string.
    { eventType         :: !EventType'
    , eventActor        :: !SimpleUser
    , eventRepos        :: !SimpleRepository
    , eventPublic       :: !Bool
    , eventCreatedAt    :: !UTCTime
    , eventOrganisation :: !(Maybe SimpleOrganization)
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Event where rnf = genericRnf
instance Binary Event 
-- | Event Types.
-- 
-- See <https://developer.github.com/v3/activity/events/types/> for more informations
-- about those types.
data EventType'= CommitCommentEvent
               | CreateEvent
               | DeleteEvent
               | DeploymentEvent
               | DeploymentStatusEvent
               | DownloadEvent
               | FollowEvent
               | ForkEvent
               | ForkApplyEvent
               | GistEvent
               | GollumEvent
               | IssueCommentEvent
               | IssuesEvent
               | LabelEvent
               | MemberEvent
               | MembershipEvent
               | MilestoneEvent
               | OrganizationEvent
               | OrgBlockEvent
               | PageBuildEvent
               | ProjectCardEvent
               | ProjectColumnEvent
               | ProjectEvent
               | PublicEvent
               | PullRequestEvent'
               | PullRequestReviewEvent
               | PullRequestReviewCommentEvent
               | PushEvent
               | ReleaseEvent
               | RepositoryEvent
               | StatusEvent
               | TeamEvent
               | TeamAddEvent
               | WatchEvent
             deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData EventType' where rnf = genericRnf
instance Binary EventType'

data SimpleRepository = SimpleRepository
    { simpleRepositoryId   :: !(Id SimpleRepository)
    , simpleRepositoryName :: !(Name SimpleRepository)
    , simpleRepositoryUrl  :: !URL
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData SimpleRepository where rnf = genericRnf
instance Binary SimpleRepository

instance FromJSON Event where
    parseJSON = withObject "Event" $ \obj -> Event 
        -- <$> obj .: "id"
        <$> obj .:   "type"
        <*> obj .:   "actor"
        <*> obj .:   "repo"
        <*> obj .:   "public"
        <*> obj .:   "created_at"
        <*> obj .:?  "org"

instance FromJSON EventType' where
    parseJSON (String "CommitCommentEvent")            = pure CommitCommentEvent
    parseJSON (String "CreateEvent")                   = pure CreateEvent
    parseJSON (String "DeleteEvent")                   = pure DeleteEvent
    parseJSON (String "DeploymentEvent")               = pure DeploymentEvent
    parseJSON (String "DeploymentStatusEvent")         = pure DeploymentStatusEvent
    parseJSON (String "DownloadEvent")                 = pure DownloadEvent
    parseJSON (String "FollowEvent")                   = pure FollowEvent
    parseJSON (String "ForkEvent")                     = pure ForkEvent
    parseJSON (String "ForkApplyEvent")                = pure ForkApplyEvent
    parseJSON (String "GistEvent")                     = pure GistEvent
    parseJSON (String "GollumEvent")                   = pure GollumEvent
    parseJSON (String "IssueCommentEvent")             = pure IssueCommentEvent
    parseJSON (String "IssuesEvent")                   = pure IssuesEvent
    parseJSON (String "LabelEvent")                    = pure LabelEvent
    parseJSON (String "MemberEvent")                   = pure MemberEvent
    parseJSON (String "MembershipEvent")               = pure MembershipEvent
    parseJSON (String "MilestoneEvent")                = pure MilestoneEvent
    parseJSON (String "OrganizationEvent")             = pure OrganizationEvent
    parseJSON (String "OrgBlockEvent")                 = pure OrgBlockEvent
    parseJSON (String "PageBuildEvent")                = pure PageBuildEvent
    parseJSON (String "ProjectCardEvent")              = pure ProjectCardEvent
    parseJSON (String "ProjectColumnEvent")            = pure ProjectColumnEvent
    parseJSON (String "ProjectEvent")                  = pure ProjectEvent
    parseJSON (String "PublicEvent")                   = pure PublicEvent
    parseJSON (String "PullRequestEvent")              = pure PullRequestEvent'
    parseJSON (String "PullRequestReviewEvent")        = pure PullRequestReviewEvent
    parseJSON (String "PullRequestReviewCommentEvent") = pure PullRequestReviewCommentEvent
    parseJSON (String "PushEvent")                     = pure PushEvent
    parseJSON (String "ReleaseEvent")                  = pure ReleaseEvent
    parseJSON (String "RepositoryEvent")               = pure RepositoryEvent
    parseJSON (String "StatusEvent")                   = pure StatusEvent
    parseJSON (String "TeamEvent")                     = pure TeamEvent
    parseJSON (String "TeamAddEvent")                  = pure TeamAddEvent
    parseJSON (String "WatchEvent")                    = pure WatchEvent
    parseJSON err@(String _)                           = fail ("Could not build EventType " ++ show err)
    parseJSON _                                        = fail ("Could not build an EventType.")

instance FromJSON SimpleRepository where
    parseJSON = withObject "SimpleRepository" $ \obj -> do
        SimpleRepository
            <$> obj .: "id"
            <*> obj .: "name"
            <*> obj .: "url"

