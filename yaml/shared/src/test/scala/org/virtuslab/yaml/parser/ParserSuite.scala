package org.virtuslab.yaml
package parser

import org.virtuslab.yaml.internal.load.parse.EventKind.*
import org.virtuslab.yaml.internal.load.reader.token.ScalarStyle

class ParserSuite extends BaseYamlSuite:

  test("kubernetes config") {
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
      MappingEnd,
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
      MappingEnd,
      SequenceEnd,
      MappingEnd,
      SequenceEnd,
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
      MappingEnd,
      MappingEnd,
      SequenceEnd,
      MappingEnd,
      MappingEnd,
      DocumentEnd(),
      StreamEnd
    )

    assertEquals(yaml.events, Right(expectedEvents))
  }
