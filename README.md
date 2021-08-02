# Scala Yaml

Scala Yaml is written in Scala, dependency-free library that allows to work with the [YAML](https://yaml.org/spec/1.2/spec.html).  

The goal of this project is to create simple and convenient library which provides:
* compile time type safety
* semiautomatic derivation of codecs instances
* explicit error handling and helpful error messages

# Help us create our roadmap!
Take part in our [discussions](https://github.com/VirtusLab/scala-yaml/discussions), post your ideas, vote for feature requests and have a real impact on how our next milestone will look like!

# Run unit tests

```
$ sbt test
```

# Run integration tests

```
$ sbt IntegrationTest/test
```

# Usage

```scala sc:compile
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

# Supported types 

- [x] Primitive types such as `Int`, `Long`, `Double`, `Float`, `Short`, `Byte`, `Boolean` and `String`.
- [x] Collection types such as `Seq`, `List`, `Set`, `Map`*  
  \* for now, when serializing, we only support `Map[String, V]` where `V` is an arbitrary datatype 

### Useful Links
* [YAML spec](https://yaml.org/spec/1.2/spec.html)
