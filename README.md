# tapioca

tapioca is a package that builds on cassava, to provide a simpler, more succinct method of encoding and decoding CSV's with headers.

## Why?
Let's say we have a list of data `MyRecord` which we want to encode and decode to and from a CSV file:

```haskell
data MyRecord = MyRecord 
  { field1 :: Int
  , field2 :: String
  }

myRecords :: [a]
myRecords = ..
```

Here is how it might be done in **cassava**:

```haskell
import Data.Csv

instance ToNamedRecord MyRecord where
  toNamedRecord (MyRecord field1 field2)= namedRecord
    [ "Header for Field 1" .= field1
    , "Header for Field 2" .= field2
    ]

instance DefaultOrdered MyRecord where
  headerOrder _ =
    [ "Header for Field 1"
    , "Header for Field 2"
    ]

instance FromNamedRecord MyRecord where
    parseNamedRecord m = MyRecord
      <$> m .: "Header for Field 1"
      <*> m .: "Header for Field 2"

-- Example usage
myCSV :: ByteString
myCSV = encodeDefaultOrderedByName myRecords

fromCSV :: ByteString -> Either String (Vector MyRecord)
fromCSV = (snd <$>) . decodeByName
```

While serviceable, the need to define headers twice is less than ideal, resulting in code that is bulkier and more fragile. 

Here's how we do it in **tapioca**:
```haskell
import Data.Tapioca

instance CsvMapped MyRecord where
  csvMap = mkCsvMap
    [ "Header for Field 1" := #field1
    , "Header for Field 2" := #field2
    ]

-- Example usage
myCSV :: ByteString
myCSV = encode WithHeader myRecords

fromCSV :: ByteString -> Either String (Vector MyRecord)
fromCSV = decode WithHeader
```

We see here that tapioca provides us with a more succinct definition for defining CSV mappings, avoiding any unnecessary duplication, and keeping the entire definition within a single typeclass.

## How?
The trick here is that #field1 and #field2 are OverloadedLabels which create a Mapping data object each.
Each mapping object uses 
  - The HasField typeclass to store an accessor to the field, eg `MyRecord -> Int` or `MyRecord -> String`. 
  - This can then infer IsLabel, and we can store the name of the field selector. Generics are then used to match up the associated field headers with the correct selectors when decoding a csv.

```haskell
instance (HasField x r a, KnownSymbol x, C.ToField a) => IsLabel x (Mapping r) where
  fromLabel = Mapping (symbolVal' (proxy# :: Proxy# x)) (getField @x)

data FieldMapping r = B.ByteString := Mapping r
data Mapping r = forall f. C.ToField f => Mapping String (r -> f)
```