module Euchre ( dealAndPlay
              , Suit(..)
              , Rank(..)
              , Card(..) ) where

import qualified System.Random as R
import qualified Data.Array.IO as AIO
import qualified Control.Monad as M
import qualified Data.Maybe    as MB
import qualified Data.List     as L
import qualified Data.Vector   as V
import qualified Text.Show.Pretty as P

data Suit = Hearts | Diamonds | Spades | Clubs
            deriving (Eq, Show)

data CallingRound = ProposedTrump | FreeSelection deriving (Eq, Show)

type TrumpSuit = Suit

data Rank = Nine | Ten | Jack | Queen | King | Ace
            deriving (Eq, Show)

data Card = Card { rank::Rank, suit::Suit }
            deriving (Eq, Show)

data CardColor = Red | Black deriving (Eq, Show)

data Player = Player1 | Player2 | Player3 | Player4
              deriving (Eq, Show, Enum, Ord)

data Team = TeamA | TeamB deriving (Eq, Show)

type Turn = (Player, Card)

type Deck = [Card]

type Trick = V.Vector Turn

type Holdings = V.Vector (V.Vector Card)

suitColors = [(Black, [Clubs, Spades]), (Red, [Diamonds, Hearts])]

players = [Player1, Player2, Player3, Player4]

teams = [[Player1, Player3], [Player2, Player4]]

ranks = [Nine, Ten, Jack, Queen, King, Ace]
suits = [Hearts, Diamonds, Clubs, Spades]

cards = [Card {rank = rs, suit = ss} | rs <- ranks, ss <- suits]

data CallingRoundEvent = Pass | Call deriving (Show, Eq)

data Hand = Hand { trump         :: Maybe Suit
                 , dealer        :: Player
                 , holdings      :: Holdings
                 , discards      :: [Card]
                 , tricks        :: V.Vector Trick
                 , loner         :: Maybe Player
                 , callingRound  :: [(Player, CallingRoundEvent)]
                 } deriving (Eq, Show)

cardsPerHand   = 5
playersAtTable = 4

-- | Returns the index of the given player.
playerIndex :: Player -> Int
playerIndex player = MB.fromJust $ L.elemIndex player players

{-|
  Returns a Hand after Dealer picks up the proposed Trump. Removes the card
  from the discard pile and places it in Dealer hand. Dealer will still have
  to drop a card after this function.
-}
pickUpTrump :: Hand -> Hand
pickUpTrump hand =
    hand {
        holdings = holdings hand V.// [(pIndex, newHand)]
      , discards = tail (discards hand)
    }
    where newHand = V.snoc (playerHand (dealer hand) hand) (proposedTrump hand)
          pIndex  = playerIndex (dealer hand)

-- | Returns the Hand with the given card of the Player removed.
dropCard :: Hand -> Player -> Card -> Hand
dropCard hand player card =
    hand { holdings = holdings hand V.// [(pIndex, newHand)] }
    where pIndex     = playerIndex player
          newHand    = V.filter (/= card) (playerHand player hand)

-- | Returns a random element from a List.
pick :: [a] -> IO a
pick xs = M.liftM (xs !!) (R.randomRIO (0, length xs - 1))

callingRoundEventTypes = [Pass, Call]

randomCallEvent :: Player -> IO (Player, CallingRoundEvent)
randomCallEvent player = do
  randOption <- pick callingRoundEventTypes
  return (player, randOption)

-- Returns a series of events leading up to a Call or eight passes.
callEventSeries :: Player -> IO [(Player, CallingRoundEvent)]
callEventSeries startPlayer = do
  fullSeries <- M.mapM randomCallEvent $
                take (playersAtTable * 2) $
                playerSeries startPlayer Nothing

  let firstCallIdx = L.findIndex (\(_, event) -> event == Call) fullSeries
  return $ case firstCallIdx of
    Nothing  -> fullSeries
    Just idx -> take (idx + 1) fullSeries

alonePlayer :: Player -> Bool -> Maybe Player
alonePlayer player isAlone = if isAlone then Just player else Nothing

-- | Returns a random Player to act as Dealer.
chosenDealer :: IO Player
chosenDealer = do
  playerNum <- R.randomRIO(0, 3)
  return (players !! playerNum)

{-|
  Randomly shuffle a list
    /O(N)/
  From http://www.haskell.org/haskellwiki/Random_shuffle
-}
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        M.forM [1..n] $ \i -> do
            j <- R.randomRIO (i,n)
            vi <- AIO.readArray ar i
            vj <- AIO.readArray ar j
            AIO.writeArray ar j vi
            return vj
  where
    n = length xs
    newArray   :: Int -> [a] -> IO (AIO.IOArray Int a)
    newArray n =  AIO.newListArray (1,n)

{-|
  List utility function to split into parts of given size.
  From http://stackoverflow.com/questions/8680888/subdividing-a-list-in-haskell
-}
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

deal :: Player -> IO Hand
deal dealer = do
  splitCards <- M.liftM (splitEvery cardsPerHand) $ shuffle cards

  return Hand { trump         = Nothing
              , dealer        = dealer
              , holdings      = V.fromList $ map V.fromList $
                                take playersAtTable splitCards
              , discards      = last splitCards
              , tricks        = V.fromList []
              , loner         = Nothing
              , callingRound  = []
              }

playerAfter :: Player -> Maybe Player -> Player
playerAfter player loner = take 2 (playerSeries player loner) !! 1

proposedTrump :: Hand -> Card
proposedTrump hand = head $ discards hand

currentTrick :: Hand -> Trick
currentTrick hand = if V.null $ tricks hand then
                        V.fromList []
                    else
                        V.last $ tricks hand

suitColorInfo :: Suit -> (CardColor, [Suit])
suitColorInfo suit = MB.fromJust $
                     L.find (\(_, suits) -> suit `elem` suits) suitColors

-- | Returns the CardColor of the given Suit.
suitColor      :: Suit -> CardColor
suitColor suit = fst $ suitColorInfo suit

-- | All suites having the given color.
suitsOfColor       :: CardColor -> [Suit]
suitsOfColor color = snd $ MB.fromJust $
                     L.find (\(clr, suits) -> clr == color) suitColors

-- | Returns the other suit of the same color as the given TrumpSuit.
nextSuit      :: TrumpSuit -> Suit
nextSuit suit = head $ dropWhile (== suit) $ suitsOfColor $
                suitColor suit

leftBower           :: TrumpSuit -> Card
leftBower trumpSuit = Card Jack $ nextSuit trumpSuit

{-|
   The nominal suit is the suit based on the chosen Trump suit. For example
   a Jack of Spades is a Club when Clubs are Trump.
-}
nominalSuit :: Card -> TrumpSuit -> Suit
nominalSuit card trumpSuit = if card == leftBower trumpSuit then
                                 trumpSuit
                             else
                                 suit card

-- | Returns the number of the given player, starting at 0 for Player 1
playerNumber :: Player -> Int
playerNumber player = MB.fromJust $ L.elemIndex player players

-- | Returns the cards currently in the given Player's hand.
playerHand :: Player -> Hand -> V.Vector Card
playerHand player hand = holdings hand V.! playerNumber player

leadCard :: Trick -> Maybe Card
leadCard trick =
    case firstTurn of
      Nothing -> Nothing
      Just (_, card) -> Just card
    where firstTurn = MB.listToMaybe (V.toList trick)

isTrump :: Card -> TrumpSuit -> Bool
isTrump card trumpSuit = nominalSuit card trumpSuit == trumpSuit

cardsWithSameNominalSuitAs :: Card -> TrumpSuit -> [Card] -> [Card]
cardsWithSameNominalSuitAs card trumpSuit =
    filter (\card -> nominalSuit card trumpSuit == cardNominalSuit)
    where cardNominalSuit = nominalSuit card trumpSuit

-- | Rotate to the start position of a List.
rotateToStart :: Eq a => a -> [a] -> [a]
rotateToStart start [] = []
rotateToStart start (x:xs) = if x == start then
                                 x : xs
                             else
                                 rotateToStart start $ xs ++ [x]

-- | Returns an infinite series of players from the given starting Player.
playerSeries :: Player -> Maybe Player -> [Player]
playerSeries startingPlayer alonePlayer =
    case alonePlayer of
      Nothing -> cycle orderedPlayers
      Just p  -> cycle $ filter (/= partnerOf p) orderedPlayers
    where orderedPlayers = rotateToStart startingPlayer players

-- | Returns a List of Players on this team.
teamIncluding :: Player -> [Player]
teamIncluding player = MB.fromJust $ L.find (elem player) teams

partnerOf :: Player -> Player
partnerOf player = head $ dropWhile (== player) $ teamIncluding player

{-|
  If there is a lead card, follow nominal suit of lead if we have it.
  Otherwise play any card in hand.
-}
playableCards :: Player -> Hand -> V.Vector Card
playableCards player hand =
    case lead of
      Nothing   -> pcards -- If there is no card lead, any card can be played.
      Just card -> let cardsFollowingLead =
                           cardsWithSameNominalSuitAs card trumpSuit
                                                          (V.toList pcards) in
                   if null cardsFollowingLead then
                       pcards
                   else
                       V.fromList cardsFollowingLead

    where lead = leadCard $ currentTrick hand
          pcards = playerHand player hand
          trumpSuit = MB.fromJust $ trump hand

{-|
  Returns the ordering of cards for a Suit given its relationship to the
  TrumpSuit.
-}
cardOrdering :: Suit -> TrumpSuit -> [Card]
cardOrdering suit trumpSuit =
    if suit == trumpSuit then
        [ Card Jack suit, leftBower suit, Card Ace suit
        , Card King suit, Card Queen suit, Card Ten suit
        , Card Nine suit ]
    else
        filter (/= leftBower trumpSuit)
                   [ Card Ace suit, Card King suit, Card Queen suit
                   , Card Jack suit, Card Ten suit, Card Nine suit ]

{-|
  Returns a List of cards in order of importance based on the
  Trump suit and lead card suit.
-}
topCards :: TrumpSuit -> Suit -> [Card]
topCards trumpSuit leadSuit =
    if trumpSuit == leadSuit then
        cardOrdering trumpSuit trumpSuit
    else
        cardOrdering trumpSuit trumpSuit ++ cardOrdering leadSuit trumpSuit

{-|
  Euchre ordering of cards is as follows:
  * Trump cards win
  * Lead suit comes next
  * Everything else is equal
-}
compareCards :: TrumpSuit -> Card -> Card -> Card -> Ordering
compareCards trumpSuit leadCard cardA cardB =
    case cardAIndex of
      Nothing -> case cardBIndex of
                   Nothing -> EQ
                   Just _  -> LT

      Just idxA -> case cardBIndex of
                    Nothing   -> GT
                    -- Lower index is better since card ordering is from highest
                    -- to lowest.
                    Just idxB -> compare idxB idxA

    where cardOrdering = topCards trumpSuit $ suit leadCard
          cardAIndex = L.elemIndex cardA cardOrdering
          cardBIndex = L.elemIndex cardB cardOrdering

{-|
  Returns the Hand after running a random calling round. Does not apply
  pickups / discards required by the calling round.
-}
randomCallingRound :: Hand -> IO Hand
randomCallingRound hand = do
  -- We don't consider a loner here since it doesn't make sense for a calling
  -- round.
  cSeries <- callEventSeries $ playerAfter (dealer hand) Nothing
  return $ hand { callingRound =  cSeries }

{-|
  Returns Bool indicating if trump was declared in the callingRound of the given
  Hand.
-}
trumpCalled :: Hand -> Bool
trumpCalled hand = L.any (\(_, event) -> event == Call) $ callingRound hand

-- | Returns Bool indicating if the dealer should pick up the proposed trump.
dealerOrderedUp :: Hand -> Bool
dealerOrderedUp hand = length (callingRound hand) <= 4

-- | Returns a random Suit excluding the proposed trump suit.
chooseNonProposedTrump :: Hand -> IO Hand
chooseNonProposedTrump hand = do
  chosenSuit <- pick $ filter
                (/= suit (proposedTrump hand))
                suits
  return $ hand { trump = Just chosenSuit }

{-|
  Simulates a random calling round. Returns the hand unchanged
  if there are eight Passes, otherwise updated dealer hand and
  discards as necessary for the Call event.
-}
randomTrumpSelection :: Hand -> IO Hand
randomTrumpSelection hand = do
  handAfterCallingRound <- randomCallingRound hand

  if trumpCalled handAfterCallingRound then
      if dealerOrderedUp handAfterCallingRound then
          let handWithNewCard = pickUpTrump $ handAfterCallingRound {
                                  trump = Just (suit $ proposedTrump hand) } in

          -- Drop a random card from the hand.
          M.liftM (dropCard handWithNewCard (dealer hand))
               (pick $ V.toList $ playerHand (dealer hand) handWithNewCard)

      else -- Trump was called in second round, choose a non-proposed trump.
          chooseNonProposedTrump handAfterCallingRound

  else
      return handAfterCallingRound

-- | Returns the active number of players depending on whether Hand is a loner.
activePlayerCount :: Hand -> Int
activePlayerCount hand = case loner hand of
                           Nothing -> 4
                           Just _ -> 3

numTurns :: Hand -> Int
numTurns hand = activePlayerCount hand * cardsPerHand

anyCardsPlayed :: Hand -> Bool
anyCardsPlayed hand = V.length (tricks hand) > 0

{-|
  Returns Bool indicating if last Trick is complete. Partial function that
  causes runtime exception if no tricks are found.
-}
lastTrickComplete :: Hand -> Bool
lastTrickComplete hand =
    V.length (V.last (tricks hand)) == activePlayerCount hand

-- | Returns Bool indicating whether a new trick should be started.
startingNewTrick :: Hand -> Bool
startingNewTrick hand = not (anyCardsPlayed hand) || lastTrickComplete hand

{-|
   Inserts card into the tricks collection, either as a new trick
   or as an element in an existing trick.
-}
insertCardToTrick :: Hand -> Player -> Card -> Hand
insertCardToTrick hand player card =
    if startingNewTrick hand then
        hand { tricks = V.snoc (tricks hand) (V.fromList [(player, card)]) }
    else
        let lastTrick    = V.last $ tricks hand
            lastTrick'   = lastTrick V.++ V.fromList [(player, card)]
            lastTrickIdx = V.length (tricks hand) - 1
        in
          hand { tricks = tricks hand V.// [(lastTrickIdx, lastTrick')] }

{-|
   Plays the suggested card, ie, it is removed from hand and appended
   to the current trick.
-}
playCard :: Hand -> Player -> Card -> Hand
playCard hand player card =
    dropCard (insertCardToTrick hand player card) player card

{-|
  Plays a single random Card for the given Hand.

  Requires that Trump is called first or runtime error will occur.
-}
playRandomCard :: Hand -> Player -> IO Hand
playRandomCard hand player = do
  card <- pick $ V.toList $ playableCards player hand
  return $ playCard hand player card

{-|
  Plays an entire random Trick.

  Requires that Trump is called first or runtime error will occur.
-}
playTrick :: Hand -> IO Hand
playTrick hand =
    M.foldM playRandomCard hand players
    where players = take (activePlayerCount hand) $
                    playerSeries (playerStartingTrick hand) (loner hand)

{-|
  Plays an entire random Hand. Needs to be called after Deal so that Trump is
  known. If no trump is called on the given Hand, returns the original Hand
  unmodified.
-}
playRandomHand :: Hand -> IO Hand
playRandomHand hand
    | trumpCalled hand = M.foldM (\h _ -> playTrick h) hand [1..cardsPerHand]
    | otherwise        = return hand

-- | Returns the Player who called Trump on this Hand.
caller :: Hand -> Maybe Player
caller hand = case L.find (\(player, event) -> event == Call)
              (callingRound hand) of
                Nothing -> Nothing
                Just (p, _) -> Just p

-- | Randomly makes the given Hand a loner.
randomlyAlone :: Hand -> IO Hand
randomlyAlone hand
    -- randomly choose loner if trump is selected
    | trumpCalled hand = do
  isAlone <- pick [True, False]
  return $ if isAlone then
               hand { loner = caller hand }
           else
               hand
    -- if no trump selected, return unchanged hand
    | otherwise = return hand

{-|
   Plays an entirely random game with an equal possibility of any alternative.
   Ie., there is no intelligence in this game algorithm, but it follows the
   rules of Euchre.
-}
dealAndPlay :: IO Hand
dealAndPlay = do
  M.join (M.liftM deal chosenDealer) >>= randomTrumpSelection >>=
   randomlyAlone >>= playRandomHand

prettyPrintHand :: Hand -> String
prettyPrintHand = P.ppShow

-- | Returns the correct Player to be starting this trick based on game state.
playerStartingTrick :: Hand -> Player
playerStartingTrick hand =
    -- If no tricks, start with player left of dealer.
    if V.null $ tricks hand then
        playerAfter (dealer hand) (loner hand)
    else -- At least one trick is played so take winner of previous Trick.
        winner (MB.fromJust $ trump hand) (V.last $ tricks hand)

-- | Returns a List of the winners of all Tricks played.
winners :: Hand -> [Player]
winners hand = map (winner (MB.fromJust (trump hand))) (V.toList $ tricks hand)

tricksPerTeam :: Hand -> (Int, Int)
tricksPerTeam hand = foldl
                     (\(t1, t2) winner ->
                          if team winner == TeamA then
                              (t1 + 1, t2)
                          else
                              (t1, t2 + 1)
                     ) (0, 0) (winners hand)

isLoner :: Hand -> Bool
isLoner hand = case loner hand of
                 Nothing -> False
                 Just _  -> True

team :: Player -> Team
team player = if player `elem` [Player1, Player3] then TeamA else TeamB

makers :: Hand -> Maybe Team
makers hand = case callingEvent of
                Nothing -> Nothing
                Just (p, _) -> Just $ team p
    where callingEvent = L.find
                        (\(_, event) -> event == Call)
                        (callingRound hand)

winningTeam :: (Int, Int) -> Team
winningTeam (teamATricks, teamBTricks) =
    if teamATricks > teamBTricks then TeamA else TeamB

winningTeamTricks :: (Int, Int) -> Int
winningTeamTricks (teamATricks, teamBTricks) =
    maximum [teamATricks, teamBTricks]

score :: (Int, Int) -> Team -> Bool -> (Team, Int)
score scores maker alone
      | winningTeam scores == maker &&
        winningTeamTricks scores == 3 ||
        winningTeamTricks scores == 4    = (winningTeam scores, 1)
      | winningTeam scores == maker &&
        winningTeamTricks scores == 5 &&
        not alone                        = (winningTeam scores, 2)
      | winningTeam scores == maker &&
        winningTeamTricks scores == 5 &&
        alone                            = (winningTeam scores, 4)
      | winningTeam scores /= maker      = (winningTeam scores, 2)

-- | Returns the Player that won the given Trick.
winner :: TrumpSuit -> Trick -> Player
winner trumpSuit trick =
    fst $ V.maximumBy sortFunc trick
    where
      leadCard = snd $ V.head trick
      sortFunc (playerA, cardA) (playerB, cardB) =
          compareCards trumpSuit leadCard cardA cardB
