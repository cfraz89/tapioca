# tapioca

tapioca is a package that builds on cassava, to provide a simpler, more succinct method of exporting CSV's with headers.

## Why?
Let's say we have a list of data `MyRecord` which we want to encode to a CSV file:

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

-- Example usage
myCSV :: ByteString
myCSV = encodeDefaultOrderedByName myRecords
```

While serviceable, the need to define headers twice is less than ideal, resulting in code that is bulkier and more fragile. 

Here's how we do it in **tapioca**:
```haskell
import Data.Tapioca

instance CsvMapped MyRecord where
  csvMap = mkCsvMap
    [ "Header for Field 1" := field1
    , "Header for Field 2" := field2
    ]

-- Example usage
myCSV :: ByteString
myCSV = encode WithHeader myRecords
```

We see here that tapioca provides us with a more succinct definition for defining CSV mappings, avoiding any unnecessary duplication, and keeping the entire definition within a single typeclass.

## How?
The trick here is that field1 and field2 in this example are no longer field values of `Int` and `String` types respectively, but are being used as field accessors, taking types `MyRecord -> Int` and `MyRecord -> String`. This is enabled by the `:=` data constructor:

```haskell
data FieldMapping a = forall f. C.ToField f => ByteString := (a -> f)
```

Knowing this, we can customise the data which is encoded by composing over the field accessor function. For example, to append to field 2:

```haskell
"Header for Field 2" := (<> " items") . field2

-- or with Control.Arrow
"Header for Field 2" := field2 >>> (<> " items")
```