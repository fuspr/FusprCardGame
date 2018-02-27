{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Data.Kind (Type)
import Data.Label ((:->), lens)
import Data.Text (Text)
import Data.Type.Equality (TestEquality(..), (:~:)(..))
import Fuspr.Ex
import Fuspr.IdList (Id, IdList)

type Bag                 = []
type PlayerRef           = Id
type ObjectRef ty        = (ZoneRef ty, Id)
type SomeObjectRef       = (Some ZoneRef, Id)
type ActivatedAbilityRef = (SomeObjectRef, Int)
type LastKnownObjectInfo = (SomeObjectRef, Object)
type Timestamp           = Int
--type Evolution           = (TyCard, TyCard, TyCard)

toSomeObjectRef :: ObjectRef ty -> SomeObjectRef
toSomeObjectRef (zoneRef, i) = (Some zoneRef, i)

data ObjectType = TyCard
                | TyPermanent

-- TODO: populate this
data Event = Event

data ZoneRef :: ObjectType -> Type where
  Library   :: PlayerRef -> ZoneRef TyCard
  Hand      :: PlayerRef -> ZoneRef TyCard
  Channel   ::              ZoneRef TyPermanent
  Graveyard :: PlayerRef -> ZoneRef TyCard
  Exile     ::              ZoneRef TyCard
  Avatar    :: PlayerRef -> ZoneRef TyCard
deriving instance Show (ZoneRef ty)
deriving instance Eq   (ZoneRef ty)

instance TestEquality ZoneRef where
  testEquality (Library p1) (Library p2)     | p1 == p2 = Just Refl
  testEquality (Hand p1)    (Hand p2)        | p1 == p2 = Just Refl
  testEquality Channel Channel                          = Just Refl
  testEquality (Graveyard p1) (Graveyard p2) | p1 == p2 = Just Refl
  testEquality Exile Exile                              = Just Refl
  testEquality (Avatar p1) (Avatar p2)       | p1 == p2 = Just Refl
  testEquality _ _                                      = Nothing

-- FIXME: Turn Structure
--
data Step
  = BeginningPhase BeginningStep
  | MainPhase
  | CombatPhase CombatStep
  | EndPhase EndStep
  deriving (Eq, Ord, Show, Read)

data BeginningStep
  = UpkeepStep
  | DrawStep
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data CombatStep
  = BeginningOfCombatStep
  | EndOfCombatStep
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

data EndStep
  = EndOfTurnStep
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data World = World
  { _players       :: IdList PlayerRef
  , _activePlayer  :: PlayerRef
  , _activeStep    :: Step
  , _time          :: Timestamp
  , _turnStructure :: [(PlayerRef, [Step])]
  , _exile         :: IdList (ObjectOfType TyCard)
  , _channel       :: IdList (ObjectOfType TyPermanent)
  , _avatar        :: IdList (ObjectOfType TyCard)
  , _turnHistory   :: [Event]
  }

data Player = Player
  { _lift :: Int
  , _gold :: Int -- FIXME
  , _library :: IdList (ObjectOfType TyCard)
  , _hand :: IdList (ObjectOfType TyCard)
  , _graveyard :: IdList (ObjectOfType TyCard)
  , _maximumHandSize :: Maybe Int
  , _failedCardDraw :: Bool
  }

data Card = Card
  { instantiateCard :: PlayerRef -- ^ Owner
                    -> PlayerRef -- ^ Controller
                    -> Object    -- ^ Card
  }

type Deck = [Card]

-- FIXME
data Object = Object
  { _name       :: Maybe Text
  , _owner      :: PlayerRef
  , _controller :: PlayerRef
  
  , _staticKeyWordAbilities :: Bag StaticKeywordAbility
  , _activatedAbilities     :: Bag ActivatedAbility
  , _triggeredAbilities     :: TriggeredAbilities
  }

-- FIXME
data ObjectOfType :: ObjectType -> Type where
  CardObject :: { _cardObject :: Object }
             -> ObjectOfType TyCard
  Permanent  :: { _permanentObject :: Object 
                , _damage          :: Int }
             -> ObjectOfType TyPermanent

cardObject :: ObjectOfType TyCard :-> Object
cardObject = lens _cardObject (\f rec -> rec { _cardObject = f (_cardObject rec) })

permanentObject :: ObjectOfType TyPermanent :-> Object
permanentObject = lens _permanentObject (\f rec -> rec { _permanentObject = f (_permanentObject rec) })

damage :: ObjectOfType TyPermanent :-> Int
damage = lens _damage (\f rec -> rec { _damage = f (_damage rec) })

type Contextual a = SomeObjectRef -- ^ Source object
                 -> PlayerRef     -- ^ effectful player
                 -> a
data ActivatedAbility = ActivatedAbility
  { _abilityActivation :: Activation
  , _abilityType :: AbilityType
  , _cost        :: Int
  }

-- FIXME: Populate
data Activation = Activation
data AbilityType = AbilityType
data StaticKeywordAbility = StaticKeywordAbility
data TriggeredAbilities = TriggeredAbilities
