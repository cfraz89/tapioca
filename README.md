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

## Usage
As seen earlier, the key part of using cassava is to define an instance of `CsvMapped` for your type:

```haskell
instance CsvMapped MyRecord where
  csvMap = mkCsvMap
    [ "Header for Field 1" := #field1
    , "Header for Field 2" := #field2
    ]
```

### Mapping selectors
Field mappings are each a `Profunctor` from `Data.Profunctors`, and mappings of record selectors can be achieved by using the functions `lmap`, `rmap`, and `dimap`

If you wish to map how a field is encoded, you can use `rmap`

```haskell
instance CsvMapped MyRecord where
  csvMap = mkCsvMap
    [ "Header for Field 1" := rmap asOrdinal #field1
    , "Header for Field 2" := #field2
    ]
```

Likewise, you may wish to alter how a field is decoded. For this you can use `lmap`:

```haskell
instance CsvMapped MyRecord where
  csvMap = mkCsvMap
    [ "Header for Field 1" := lmap fromOrdinal #field1
    , "Header for Field 2" := #field2
    ]
```

If you would like to keep the mapping consistent between encoding and decoding, you will probably want to specify both mappings. For this use `dimap`:

```haskell
instance CsvMapped MyRecord where
  csvMap = mkCsvMap
    [ "Header for Field 1" := dimap asOrdinal fromOrdinal #field1
    , "Header for Field 2" := #field2
    ]
```

## Splicing maps
Occasionally you may want to nest a record within another record. Provided that both your records implement CsvMapped, this can be done by simply using the field selector of the inner record in the desired position:

```haskell
data SplicingRecord = SplicingRecord
  { exampleRecord :: ExampleRecord
  , other :: Int
  }
  deriving (Show, Generic)

instance CsvMapped SplicingRecord where
  csvMap = mkCsvMap
    [ #exampleRecord
    , "Other" := #other
    ]
```
Then in this example, for each row, the fields of ExampleRecord will precede the "Other" column field. Note that when decoding a spliced CSV with Headers, order of each field of ExampleRecord within the row is inferred from the order of the CSV headers. It is not required that the CSV's ExampleRecord columns are contiguous.