module ConstructDeconstruct where

data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a
             | Second b
             deriving (Eq, Show)

data RecordProduct a b =
    RecordProduct { pfirst :: a
                  , psecond :: b }
                  deriving (Eq, Show)

newtype NumCow = NumCow Int deriving (Eq, Show)

newtype NumPig = NumPig Int deriving (Eq, Show)

newtype NumSheep = NumSheep Int deriving (Eq, Show)

data Farmhouse = Farmhouse NumCow NumPig
                 deriving (Eq, Show)

data BigFarmhouse = 
    BigFarmhouse NumCow NumPig NumSheep
    deriving (Eq, Show)

type Farmhouse' = Product NumCow NumPig

type BigFarmhouse' =
    Product NumCow (Product NumPig NumSheep)

type Name = String
type Age = Int
type LovesMud = Bool
type PoundsOfWool = Int

data CowInfo = CowInfo Name Age deriving (Eq, Show)
data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal = Cow CowInfo
            | Pig PigInfo
            | Sheep SheepInfo
            deriving (Eq, Show)

type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

getIdItem :: Id a -> a
getIdItem (MkId x) = x

type Awesome = Bool

person :: Product Name Awesome
person = Product "Simon" True

--data Twitter = Twitter deriving (Eq, Show)
--data AskFm = AskFm deriving (Eq, Show)

--socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter

data SocialNetwork = Twitter
                   | AskFm
                   deriving (Eq, Show)

type Twitter = String
type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"

askfm :: Sum Twitter AskFm
askfm = First "AskFm"

unpackFirst :: Sum a b -> a
unpackFirst (First x) = x

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct { pfirst = 42
                         , psecond = 0.00001 }

data OperatingSystem = GnuPlusLinux
                     | OpenBSDPlusNevermindJustBSDStill
                     | Mac
                     | Windows
                     deriving (Eq, Show, Enum, Bounded)

data ProgrammingLanguage = Haskell
                         | Agda
                         | Idris
                         | PureScript
                         deriving (Eq, Show, Enum, Bounded)

data Programmer = Programmer { os :: OperatingSystem
                             , lang :: ProgrammingLanguage }
                             deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac
                        , lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda
                             , os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = 
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
  ]

allLanguages :: [ProgrammingLanguage]
allLanguages =
  [ Haskell
  , Agda
  , Idris
  , PureScript
  ]

allProgrammers :: [Programmer]
allProgrammers = [ Programmer os lang | 
    os <- allOperatingSystems,
    lang <- allLanguages ]

-- without using AllOperatingSystems, allLanguages lists;
-- first added Enum and Bounded derivations
allProgrammers' :: [Programmer]
allProgrammers' = [ Programmer os lang |
    os <- [minBound :: OperatingSystem .. maxBound :: OperatingSystem],
    lang <- [minBound :: ProgrammingLanguage .. maxBound :: ProgrammingLanguage] ]

data ThereYet = 
    There Integer Float String Bool
    deriving (Eq, Show)
