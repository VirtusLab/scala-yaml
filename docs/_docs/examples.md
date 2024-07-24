## Supported types 

Scala-yaml has built-in support for:
- all primitive types
- some types from stdlib like `Option`
- collections from stdlib

## Primitives

```scala sc:compile
import org.virtuslab.yaml.*

"1".as[Double]
// Right(1)

"aezakmi".as[Double]
// Cannot parse aezakmi as Double
```

## Option
When deserializing, YAML differentiates between empty string and null value. Hence in the following yaml
```yaml
key1: ""
key2: !!null
key3:
```
`key1` has empty String as value while both `key2` and `key3` are considered to have null value.  

(there is an [issue](YamlDecoder) with deriving YamlEncoder instance for Option datatype hence `YamlDecoder` instead `YamlCodec`)  

```scala 
import org.virtuslab.yaml.*

case class Keys(key1: String, key2: Option[String], key3: Option[String]) derives YamlDecoder 
"""|key1: ""
   |key2: !!null
   |key3: 
   |""".stripMargin.as[Keys]
```

## List
```scala sc:compile
import org.virtuslab.yaml.*

"""|- 1
   |- !!null
   |- 3
   |""".stripMargin.as[List[Option[Int]]]
// Right(List(Some(1), None, Some(3)))

"""|- 1
   |- 2
   |- 3
   |""".stripMargin.as[List[Int]]
// Right(List(1, 2, 3))
```

## Map
```scala sc:compile
import org.virtuslab.yaml.*

"""|1: one
   |2: two
   |3: three
   |""".stripMargin.as[Map[Int, String]]
// Right(Map(1 -> one, 2 -> two, 3 -> three))
```

## Decoding from unknown type
It is also possible to decode yaml of unknown type. Since yaml consists of: scalars, mappings and sequences one can use:
- `as[Any]` - it will decode yaml into `List` or `Map` depending on its structure
- `as[List]` or other sequence datatypes - it will try to decode yaml sequence
- `as[Map]` - similarly, it will try to decode yaml mapping

```scala sc:compile
import org.virtuslab.yaml.*

"""|- 1
   |- !!null
   |- 3
   |""".stripMargin.as[Any]
// Right(List(1, None, 1))

"""|- 1
   |- 2
   |- 3
   |""".stripMargin.as[Any]
// Right(List(1, 2, 3))
```

Take a closer look at outcome of first snippet, it `List(1, None, 1)`. Since `Any` doesn't provide any suggestions, scala-yaml will try to 
construct as precised typed as it can.

```scala sc:compile
import org.virtuslab.yaml.*

"""|1: one
   |2: two
   |3: three
   |""".stripMargin.as[Any]
// Right(Map(1 -> one, 2 -> two, 3 -> three))
```
