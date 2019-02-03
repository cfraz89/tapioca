# tapioca

tapioca is a package that builds on [cassava](http://hackage.haskell.org/package/cassava), to provide a simpler, more succinct method of encoding and decoding CSV's with headers.

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
  csvMap = CsvMap
     $ "Header for Field 1" <-> #field1
    :| "Header for Field 2" <-> #field2

-- Example usage
myCSV :: ByteString
myCSV = encode HasHeader myRecords

fromCSV :: ByteString -> Either String (Vector MyRecord)
fromCSV = decode HasHeader
```

We see here that tapioca provides us with a more succinct definition for defining CSV mappings, avoiding any unnecessary duplication, and keeping the entire definition within a single typeclass.

## Usage
As seen earlier, the key part of using Tapioca is to define an instance of `CsvMapped` for your type:

```haskell
instance CsvMapped MyRecord where
  csvMap = CsvMap
     $ "Header for Field 1" <-> #field1
    :| "Header for Field 2" <-> #field2
    :| "Header for Field 3" <-> #field3

```

### Mapping selectors
Fields can be mapped on top of cassava's `FromField` and `ToField` instances on a per-field basis.
As mappings are expected to be isomorphisms, they are defined a lens `Iso`.
If you wish to map a field over an isomorphism, you can use either the `<:>` or  `codec` (synonyms of each other) combinators:

```haskell
import Control.Lens.Iso

instance CsvMapped MyRecord where
  csvMap = mkCsvMap
     $ "Header for Field 1" <-> #field1 <:> iso asOrdinal fromOrdinal
     -- or
    :| "Header for Field 2" <-> codec #field2 (iso asOrdinal fromOrdinal)

```
Refer to the *CodecField* example to see this in practice.

### Nesting maps
Occasionally you may want to nest a record within another record. Provided that both your records implement `CsvMapped`, this can be done by using the `Nest` constructor:

```haskell
data NestingRecord = NestingRecord
  { exampleRecord :: ExampleRecord
  , other :: Int
  }
  deriving (Show, Generic)

instance CsvMapped NestingRecord where
  csvMap = CsvMap
     $ nest #exampleRecord
    :| "Other" <-> #other

```
Then in this example, for each row, the fields of ExampleRecord will precede the "Other" column field.
Note that when decoding a spliced CSV with Headers, order of each field of ExampleRecord within the row is inferred from the order of the CSV headers.
It is not required that the CSV's ExampleRecord columns are contiguous.

Refer to the *NestedEncode* and *NestedDecode* examples to see this in practice.
