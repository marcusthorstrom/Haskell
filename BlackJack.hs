module BlackJack where
import Cards
import RunGame

import Data.List
import Test.QuickCheck
import System.Random


-- Task 1 --
empty :: Hand
empty = Empty

-- 3.2 Method calculating size
--
--  hand2 = (Add (Card (Numeric 2) Hearts) (Add (Card Jack Spades) Empty))
--  size hand2 -> 1 + (Add (Card Jack Spades) Empty)
--  size hans2 -> 1 + Empty
--  size hand2 -> Empty -> 0
--  step 3 -> 0 + 1
--  step 2 -> (0 + 1) +1
--  size returns 2
--
fullSuitList :: Suit -> [Card]
fullSuitList suit = [(Card (Numeric a ) suit) | a <- [2..10]] ++ [(Card Jack suit), (Card Queen suit), (Card King suit) , (Card Ace suit)]

-- First changed name to ListToHand, but looked up foldr and changed below

fullSuit :: Suit -> Hand
fullSuit suit =  foldr Add Empty (fullSuitList suit)

-- Taks 2

numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card r s) hand) = if r == Ace then 1 + numberOfAces hand else numberOfAces hand

--Without ace
valueRank :: Card -> Integer
valueRank (Card r s) = case r of  Jack -> 10
                                  Queen -> 10
                                  King -> 10
                                  Numeric a -> a
                                  Ace -> 0
-- Skips aces
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add card hand) = (valueRank card) + (valueHand hand)

value :: Hand -> Integer
value hand = if valueHand hand + (numberOfAces hand * 11) > 21
  then
    valueHand hand + (numberOfAces hand * 1)
  else
    valueHand hand + (numberOfAces hand * 11)


-- Task 3 --
gameOver :: Hand -> Bool
gameOver hand = (value hand > 21)

-- Task 4 --
-- First check if guest got gameover, then check if bank, then check value
--         Guest   Bank
winner :: Hand -> Hand -> Player
winner g1 b1 | gameOver g1 = Bank -- The guest fails
winner g1 b1 | gameOver b1 = Guest
winner g1 b1 | (value g1 ) > (value b1) = Guest
winner g1 b1 = Bank   -- Else bank


-- Alternative Task 2 (Aces can have different values) --
-- Get the value of the card (The array is because the Ace has two values)
cardValue :: Card -> [Integer]
cardValue (Card rank suit) = case rank of Jack -> [10]
                                          Queen -> [10]
                                          King -> [10]
                                          Ace -> [1, 11]
                                          Numeric a -> [a]
-- The hand is evaluated according to cardValue and put to a list of values
valueAsList :: Hand -> [[Integer]]
valueAsList Empty = []
valueAsList (Add card hand) = valueAsList hand ++ [cardValue card]
-- Then the different combinations (sequence) is generated and summed to create
-- a list of posible values
possibleValues :: Hand -> [Integer]
possibleValues hand = map (sum) (sequence ( valueAsList hand ))
-- Then, since we want to return one value:
--  * if the list contains 21, return 21
--  * if the list contains value under 21, return the highest value
--  * else (does not matter, they are bust anyway) return the first element
secondValue :: Hand -> Integer
secondValue hand | (filter (\x -> x == 21) (possibleValues hand)) /= [] = 21
secondValue hand | (filter (\x -> x <  21) (possibleValues hand)) /= []
              = last (sort( ( filter (\x -> x <  21) (possibleValues hand) ) ))
secondValue hand = head (possibleValues hand)


-- PART 2 --
-- Task 1 --
getCard :: Hand -> Card
getCard (Add card hand) = card
getHand :: Hand -> Hand
getHand (Add card hand) = hand

(<+) :: Hand -> Hand -> Hand
Empty <+ hand2 = hand2
(Add c1 h1) <+ hand2 = (Add c1 (h1 <+ hand2))

prop_onTopOf_assoc :: Hand -> Hand -> Hand -> Bool
prop_onTopOf_assoc p1 p2 p3 = p1<+(p2<+p3) == (p1<+p2)<+p3

prop_size_onTopOf :: Hand -> Hand -> Bool
prop_size_onTopOf h1 h2 = (size h1) + (size h2) == (size (h1 <+ h2))

-- Task 2 --
suitHand :: [Rank] -> Suit -> Hand
suitHand [] s = Empty
suitHand (r:rs) s = (Add (Card r s) (suitHand rs s) )

createSuit:: Suit -> Hand
createSuit r = suitHand ([ Numeric n | n <- [2..10] ] ++ [Jack, Queen, King, Ace]) r

fullDeck :: Hand
fullDeck = createSuit(Hearts) <+ createSuit(Clubs) <+
          createSuit(Diamonds) <+ createSuit(Spades)


-- Task 3 --
--      Deck    Hand     Deck, Hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty hand = error "draw: The deck is empty."
--draw (Add card deck) hand | deck == Empty = (deck, hand <+ (Add (card) Empty))
draw (Add card deck) hand = (deck, hand <+ (Add (card) Empty))



-- Task 4 --

playBank :: Hand -> Hand
playBank deck = (playBank' deck Empty )
  where
    playBank' :: Hand -> Hand -> Hand
    playBank' deck bankHand = if value bankHand < 16
      then -- The value is still below 16
        let (deck, bankHand) = (draw deck bankHand) in
        playBank' deck bankHand
      else -- The value is 16 or above
        bankHand

-- Task 5 --
-- Go all the way through the deck, then when number is 0, return back upp
-- append the first of the return with the current value as a new card
--            Counter    deck
getNthCard :: Integer -> Hand -> ( Hand, Card )
getNthCard number Empty = error "getNthCard: Cannot get this card"
getNthCard number deck | number == 0 = ( getHand deck, (getCard deck) )
getNthCard number deck = let (hand, card) = ( getNthCard (number - 1) (getHand deck) ) in
  ( (Add (getCard deck) Empty) <+ hand , card)


shuffle :: StdGen -> Hand -> Hand
--shuffle g Empty = error "shuffle: Deck cannot be empty"
shuffle g hand = drawRandomCard g hand Empty
  where
    drawRandomCard :: StdGen -> Hand -> Hand -> Hand
    drawRandomCard g Empty hand = hand
    drawRandomCard g old new =
        let (num, g2) = randomR (0, ((size old) - 1) ) g
            (hand, card) = getNthCard num old
        in
            drawRandomCard g2 hand ( new <+ (Add card Empty) )

prop_shuffle_sameCards :: StdGen -> Card -> Hand -> Bool
prop_shuffle_sameCards g c h =
  c `belongsTo` h == (c `belongsTo` (BlackJack.shuffle g h))

belongsTo :: Card -> Hand -> Bool
belongsTo c Empty = False
belongsTo c (Add cc h) = (c == cc) || (belongsTo c h)

prop_size_shuffle :: StdGen -> Hand -> Bool
prop_size_shuffle g hand = (size hand) == (size (BlackJack.shuffle g hand))

implementation = Interface
  { iEmpty    = empty
  , iFullDeck = fullDeck
  , iValue    = value
  , iGameOver = gameOver
  , iWinner   = winner
  , iDraw     = draw
  , iPlayBank = playBank
  , iShuffle  = BlackJack.shuffle
  }

main :: IO ()
main = runGame implementation
