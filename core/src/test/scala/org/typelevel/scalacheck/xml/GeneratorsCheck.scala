package org.typelevel.scalacheck.xml

import org.scalacheck.Prop
import org.scalacheck.Properties

class GeneratorsCheck extends Properties("GeneratorsCheck") {

  property("generates xml") = Prop.forAll(generators.genXml) { _ =>
    Prop.passed
  }

}
