package org.virtuslab.yaml

import org.virtuslab.yaml.internal.load.parse.ParserImpl

import scala.util.{Failure, Success, Try}

import org.virtuslab.yaml
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.parse.{Event, ParserImpl}
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle._
import org.virtuslab.yaml.internal.load.reader.token.Token._
import scala.annotation.tailrec
import scala.collection.mutable

case class TestSuiteRunner(testYamlML: os.Path) extends TestRunner {
  private val testMl = TestMlEntry.from(os.read(testYamlML))

  override val inYaml         = testMl.inYaml
  override val expectedEvents = testMl.seqEvent
}
