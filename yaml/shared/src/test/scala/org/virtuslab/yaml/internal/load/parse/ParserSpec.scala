package org.virtuslab.yaml.internal.load.parse

import org.virtuslab.yaml.internal.load.parse._
import org.virtuslab.yaml.internal.load.parse.Event._
import org.virtuslab.yaml.internal.load.reader.Scanner
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class ParserSpec extends BaseParseSuite:

  test("should parse sequence of scalars") {
    val yaml = s"""- Mark McGwire
                  |- Sammy Sosa
                  |- Ken Griffey""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      Event.StreamStart,
      Event.DocumentStart(),
      Event.SequenceStart(),
      Event.Scalar("Mark McGwire"),
      Event.Scalar("Sammy Sosa"),
      Event.Scalar("Ken Griffey"),
      Event.SequenceEnd(),
      Event.DocumentEnd(),
      Event.StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("should parse mapping scalars to scalars") {
    val yaml =
      s"""hr:  65
         |avg: 0.278
         |rbi: 147""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("hr"),
      Scalar("65"),
      Scalar("avg"),
      Scalar("0.278"),
      Scalar("rbi"),
      Scalar("147"),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("should parse sequence of mapping") {
    val yaml = s"""-
                  |  name: Mark McGwire
                  |  hr:   65
                  |  avg:  0.278
                  |-
                  |  name: Sammy Sosa
                  |  hr:   63
                  |  avg:  0.288
                  |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      Event.StreamStart,
      Event.DocumentStart(),
      Event.SequenceStart(),
      Event.MappingStart(),
      Event.Scalar("name"),
      Event.Scalar("Mark McGwire"),
      Event.Scalar("hr"),
      Event.Scalar("65"),
      Event.Scalar("avg"),
      Event.Scalar("0.278"),
      Event.MappingEnd(),
      Event.MappingStart(),
      Event.Scalar("name"),
      Event.Scalar("Sammy Sosa"),
      Event.Scalar("hr"),
      Event.Scalar("63"),
      Event.Scalar("avg"),
      Event.Scalar("0.288"),
      Event.MappingEnd(),
      Event.SequenceEnd(),
      Event.DocumentEnd(),
      Event.StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("mapping scalar to sequences") {
    val yaml = s"""american:
                  |  - Boston Red Sox
                  |  - Detroit Tigers
                  |  - New York Yankees
                  |national:
                  |  - New York Mets
                  |  - Chicago Cubs
                  |  - Atlanta Braves
                  |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("american"),
      SequenceStart(),
      Scalar("Boston Red Sox"),
      Scalar("Detroit Tigers"),
      Scalar("New York Yankees"),
      SequenceEnd(),
      Scalar("national"),
      SequenceStart(),
      Scalar("New York Mets"),
      Scalar("Chicago Cubs"),
      Scalar("Atlanta Braves"),
      SequenceEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("should parse mapping of sequence") {
    val yaml = s"""
                  |spec:
                  |  containers:
                  |  - name: iscsipd-rw
                  |  volumes:
                  |  - name: iscsipd-rw
                  |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("spec"),
      MappingStart(),
      Scalar("containers"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      MappingEnd(),
      SequenceEnd(),
      Scalar("volumes"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      MappingEnd(),
      SequenceEnd(),
      MappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }

  test("should parse kubernetess config") {
    val yaml = s"""apiVersion: v1
                  |kind: Pod
                  |metadata:
                  |  name: iscsipd
                  |spec:
                  |  containers:
                  |  - name: iscsipd-rw
                  |    image: kubernetes/pause
                  |    volumeMounts:
                  |    - mountPath: "/mnt/iscsipd"
                  |      name: iscsipd-rw
                  |  volumes:
                  |  - name: iscsipd-rw
                  |    iscsi:
                  |      targetPortal: 10.0.2.15.3260
                  |      iqn: iqn.2001-04.com.example.storage.kube.sys1.xyz
                  |      lun: 0
                  |      fsType: ext4
                  |      readOnly: true
                  |""".stripMargin

    val reader = Scanner(yaml)
    val events = ParserImpl(reader).getEvents()

    val expectedEvents = List(
      StreamStart,
      DocumentStart(),
      MappingStart(),
      Scalar("apiVersion"),
      Scalar("v1"),
      Scalar("kind"),
      Scalar("Pod"),
      Scalar("metadata"),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd"),
      MappingEnd(),
      Scalar("spec"),
      MappingStart(),
      Scalar("containers"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      Scalar("image"),
      Scalar("kubernetes/pause"),
      Scalar("volumeMounts"),
      SequenceStart(),
      MappingStart(),
      Scalar("mountPath"),
      Scalar("/mnt/iscsipd", ScalarStyle.DoubleQuoted),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      MappingEnd(),
      SequenceEnd(),
      MappingEnd(),
      SequenceEnd(),
      Scalar("volumes"),
      SequenceStart(),
      MappingStart(),
      Scalar("name"),
      Scalar("iscsipd-rw"),
      Scalar("iscsi"),
      MappingStart(),
      Scalar("targetPortal"),
      Scalar("10.0.2.15.3260"),
      Scalar("iqn"),
      Scalar("iqn.2001-04.com.example.storage.kube.sys1.xyz"),
      Scalar("lun"),
      Scalar("0"),
      Scalar("fsType"),
      Scalar("ext4"),
      Scalar("readOnly"),
      Scalar("true"),
      MappingEnd(),
      MappingEnd(),
      SequenceEnd(),
      MappingEnd(),
      MappingEnd(),
      DocumentEnd(),
      StreamEnd
    )

    assertEventsEquals(events, expectedEvents)
  }
