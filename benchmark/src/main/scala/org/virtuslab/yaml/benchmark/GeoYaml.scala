package org.virtuslab.yaml.benchmark

import org.virtuslab.yaml.*

object GeoYaml {
  extension [T](res: Either[YamlError, T]) {
    def toConstructResult(node: Node): Either[ConstructError, T] = res match {
      case Right(value)              => Right(value)
      case Left(err: ConstructError) => Left(err)
      case Left(err)                 => Left(ConstructError(err.toString, Some(node), None))
    }
  }

  given YamlDecoder[(Double, Double)] = new YamlDecoder[(Double, Double)] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, (Double, Double)] = {
      node.as[List[Double]].toConstructResult(node).flatMap {
        case List(x, y) => Right((x, y))
        case other =>
          Left(
            ConstructError(
              s"Expected 2 elements for coordinate, got ${other.size}",
              Some(node),
              None
            )
          )
      }
    }
  }

  given YamlDecoder[(Double, Double, Double, Double)] =
    new YamlDecoder[(Double, Double, Double, Double)] {
      override def construct(node: Node)(implicit
          settings: LoadSettings
      ): Either[ConstructError, (Double, Double, Double, Double)] = {
        node.as[List[Double]].toConstructResult(node).flatMap {
          case List(x1, y1, x2, y2) => Right((x1, y1, x2, y2))
          case other =>
            Left(
              ConstructError(s"Expected 4 elements for bbox, got ${other.size}", Some(node), None)
            )
        }
      }
    }

  given [A](using YamlDecoder[A]): YamlDecoder[IndexedSeq[A]] = new YamlDecoder[IndexedSeq[A]] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, IndexedSeq[A]] = {
      node.as[List[A]].toConstructResult(node).map(_.toIndexedSeq)
    }
  }

  private def extractType(
      node: Node
  )(implicit settings: LoadSettings): Either[ConstructError, String] = {
    node match {
      case mapping: Node.MappingNode =>
        mapping.mappings
          .collectFirst {
            case (Node.ScalarNode(key, _), value) if key == "type" =>
              value.as[String].toConstructResult(node)
          }
          .getOrElse(Left(ConstructError("Missing 'type' discriminator field", Some(node), None)))
      case _ => Left(ConstructError("Expected a YAML mapping object", Some(node), None))
    }
  }

  sealed trait Geometry
  sealed trait SimpleGeometry extends Geometry

  case class Point(coordinates: (Double, Double)) extends SimpleGeometry derives YamlDecoder
  case class MultiPoint(coordinates: IndexedSeq[(Double, Double)]) extends SimpleGeometry
      derives YamlDecoder
  case class LineString(coordinates: IndexedSeq[(Double, Double)]) extends SimpleGeometry
      derives YamlDecoder
  case class MultiLineString(coordinates: IndexedSeq[IndexedSeq[(Double, Double)]])
      extends SimpleGeometry derives YamlDecoder
  case class Polygon(coordinates: IndexedSeq[IndexedSeq[(Double, Double)]]) extends SimpleGeometry
      derives YamlDecoder
  case class MultiPolygon(coordinates: IndexedSeq[IndexedSeq[IndexedSeq[(Double, Double)]]])
      extends SimpleGeometry derives YamlDecoder

  given YamlDecoder[SimpleGeometry] = new YamlDecoder[SimpleGeometry] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, SimpleGeometry] = {
      extractType(node).flatMap {
        case "Point"           => node.as[Point].toConstructResult(node)
        case "MultiPoint"      => node.as[MultiPoint].toConstructResult(node)
        case "LineString"      => node.as[LineString].toConstructResult(node)
        case "MultiLineString" => node.as[MultiLineString].toConstructResult(node)
        case "Polygon"         => node.as[Polygon].toConstructResult(node)
        case "MultiPolygon"    => node.as[MultiPolygon].toConstructResult(node)
        case other => Left(ConstructError(s"Unknown SimpleGeometry type: $other", Some(node), None))
      }
    }
  }

  case class GeometryCollection(geometries: IndexedSeq[SimpleGeometry]) extends Geometry
      derives YamlDecoder

  given YamlDecoder[Geometry] = new YamlDecoder[Geometry] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, Geometry] = {
      extractType(node).flatMap {
        case "GeometryCollection" => node.as[GeometryCollection].toConstructResult(node)
        case _ => node.as[SimpleGeometry].toConstructResult(node) // fallback to SimpleGeometry
      }
    }
  }

  sealed trait GeoYaml       extends Product with Serializable
  sealed trait SimpleGeoYaml extends GeoYaml

  case class Feature(
      properties: Map[String, String] = Map.empty,
      geometry: Geometry,
      bbox: Option[(Double, Double, Double, Double)] = None
  ) extends SimpleGeoYaml
      derives YamlDecoder

  given YamlDecoder[SimpleGeoYaml] = new YamlDecoder[SimpleGeoYaml] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, SimpleGeoYaml] = {
      extractType(node).flatMap {
        case "Feature" => node.as[Feature].toConstructResult(node)
        case other => Left(ConstructError(s"Unknown SimpleGeoYaml type: $other", Some(node), None))
      }
    }
  }

  case class FeatureCollection(
      features: IndexedSeq[SimpleGeoYaml],
      bbox: Option[(Double, Double, Double, Double)] = None
  ) extends GeoYaml
      derives YamlDecoder

  given YamlDecoder[GeoYaml] = new YamlDecoder[GeoYaml] {
    override def construct(
        node: Node
    )(implicit settings: LoadSettings): Either[ConstructError, GeoYaml] = {
      extractType(node).flatMap {
        case "FeatureCollection" => node.as[FeatureCollection].toConstructResult(node)
        case "Feature"           => node.as[Feature].toConstructResult(node)
        case other => Left(ConstructError(s"Unknown GeoYaml type: $other", Some(node), None))
      }
    }
  }
}
