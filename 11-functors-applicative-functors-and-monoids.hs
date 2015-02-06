import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
	fmap _ Empty = Empty
	fmap f (Value x rest) = Value (f x) $ fmap f rest

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty x = x
combineLists (Value x rest) y = Value x (combineLists rest y)

-- Make our list a Monoid
instance Monoid (List a) where
	mempty = Empty
	mappend = combineLists

-- Make our list an Applicative
instance Applicative List where
	pure = (\a -> Value a Empty)
	Empty <*> _ = Empty
	(Value f b) <*> c = combineLists (fmap f c) (b <*> c)


-- Make sure that the List obeys the laws for Applicative and Monoid

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty
threeValueList = Value 5 $ Value 7 $ Value 9 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)
plusTwo1 = plusTwo <$> twoValueList
plusTwo2 = plusTwo <$> threeValueList

-- Use <$> and <*> on the lists with a binary function
bin1 = (+) <$> twoValueList <*> threeValueList

-- Create some lists of binary functions
binList1 = Value (+) $ Value (*) Empty

-- Use <*> on the binary functions list and the number lists
res = binList1 <*> twoValueList <*> threeValueList