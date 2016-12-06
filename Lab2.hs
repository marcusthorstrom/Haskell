data Suit = Spades | Hearts | Diamonds | Clubs
            deriving (Show, Eq)
data Colour = Red | Black
colour :: Suit -> Colour
colour Spades   = Black
colour Hearts   = Red
colour Diamonds = Black
colour Clubs    = Red
data Rank = N2 | N3 | N4 | N5 | N6 | N7 | N8 | N9 | N10
          | Jack | Queen | King | Ace
          deriving (Eq,Ord,Show,Enum)
rankBeats :: Rank -> Rank -> Bool
rankBeats r1 r2 = r1>r2

data Card = Card {rank::Rank, suit:: Suit}
            deriving Show

cardBeats :: Card -> Card -> Bool
cardBeats (Card r1 s1) (Card r2 s2) = s1==s2 && rankBeats r1 r2

data Hand = Empty | Add Card Hand
            deriving Show