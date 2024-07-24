Using scala-yaml is very easy, all you have to do is import `org.virtuslab.yaml.*` and write `derives YamlCodec` after your case class definition. 
Then you're able to use following extension methods:
- **`as[T]`** yields `Either[YamlError, T]` trying to convert String instance to the provided type `T`
- **`asYaml`** converts your datatype into yaml-formatted String

```scala 
import org.virtuslab.yaml.*

case class Address(city: String, zipcode: String) derives YamlCodec
case class Person(name: String, age: Int, address: Address) derives YamlCodec

val yaml = s"""name: John Wick
              |age: 40
              |address:
              |  city: Anywhere
              |  zipcode: 12-345
              |""".stripMargin

val decoded = yaml.as[Person]
// Either[YamlError, Person] = Right(Person(John Wick,40,Address(Anywhere,12-345)))

case class Activity(kind: String, distance: Seq[Double]) derives YamlCodec

val activity = Activity("running", Seq(5.37, 4.98, 5.73))
val encoded  = activity.asYaml
//kind: running
//distance: 
//  - 5.37
//  - 4.98
//  - 5.73
```

It is important not to omit `derives YamlCodec` in case class definition otherwise compiler will not be able to create codecs and will fail as shown below:

```scala sc:fail
import org.virtuslab.yaml.*

case class Address(city: String, zipcode: String)
val address = Address("EŁk", "19-300")
address.asYaml
```