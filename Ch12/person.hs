module PersonExample where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

--nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay :: Name -> ValidatePerson Name
nameOkay name = case name /= "" of
    True  -> Right name
    False -> Left [NameEmpty]

--ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay :: Age -> ValidatePerson Age
ageOkay age = case age >= 0 of
    True  -> Right age
    False -> Left [AgeTooLow]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = 
    mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name
          -> ValidatePerson Age
          -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) =
    Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) =
    Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge)  = Left badAge
