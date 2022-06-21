/*
 * Copyright 2022 Typelevel
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.typelevel.scalacheck.xml

import org.scalacheck.Gen

import java.util.Locale
import scala.xml._

/** Roughly generates XML 1.0, Fourth Edition, which is digestible by Xerces
  * 2.12.
  *
  * Not yet generated:
  *   - EntityRefs (and EntityDecl)
  *   - Surrogate pair characters
  *   - Namespace bindings
  */
object generators {

  /** As defined in XML 1.0, Fourth Edition, Appendix B. These rules are
    * "orphaned" in XML 1.0, Fifth Edition, but still the basis of xerces2-j
    * version 2.12.2.
    *
    * @see
    *   https://www.w3.org/TR/xml/#CharClasses
    * @see
    *   https://xerces.apache.org/xerces2-j/
    */
  object classes {
    val char: Seq[Char] =
      Seq(
        0x0009 to 0x0009,
        0x000a to 0x000a,
        0x000d to 0x000d,
        0x0020 to 0xd7ff,
        0xe000 to 0xfffd
      ).flatten.map(_.toChar)

    val baseChar: Seq[Char] =
      Seq(
        0x0041 to 0x005a,
        0x0061 to 0x007a,
        0x00c0 to 0x00d6,
        0x00d8 to 0x00f6,
        0x00f8 to 0x00ff,
        0x0100 to 0x0131,
        0x0134 to 0x013e,
        0x0141 to 0x0148,
        0x014a to 0x017e,
        0x0180 to 0x01c3,
        0x01cd to 0x01f0,
        0x01f4 to 0x01f5,
        0x01fa to 0x0217,
        0x0250 to 0x02a8,
        0x02bb to 0x02c1,
        0x0386 to 0x0386,
        0x0388 to 0x038a,
        0x038c to 0x038c,
        0x038e to 0x03a1,
        0x03a3 to 0x03ce,
        0x03d0 to 0x03d6,
        0x03da to 0x03da,
        0x03dc to 0x03dc,
        0x03de to 0x03de,
        0x03e0 to 0x03e0,
        0x03e2 to 0x03f3,
        0x0401 to 0x040c,
        0x040e to 0x044f,
        0x0451 to 0x045c,
        0x045e to 0x0481,
        0x0490 to 0x04c4,
        0x04c7 to 0x04c8,
        0x04cb to 0x04cc,
        0x04d0 to 0x04eb,
        0x04ee to 0x04f5,
        0x04f8 to 0x04f9,
        0x0531 to 0x0556,
        0x0559 to 0x0559,
        0x0561 to 0x0586,
        0x05d0 to 0x05ea,
        0x05f0 to 0x05f2,
        0x0621 to 0x063a,
        0x0641 to 0x064a,
        0x0671 to 0x06b7,
        0x06ba to 0x06be,
        0x06c0 to 0x06ce,
        0x06d0 to 0x06d3,
        0x06d5 to 0x06d5,
        0x06e5 to 0x06e6,
        0x0905 to 0x0939,
        0x093d to 0x093d,
        0x0958 to 0x0961,
        0x0985 to 0x098c,
        0x098f to 0x0990,
        0x0993 to 0x09a8,
        0x09aa to 0x09b0,
        0x09b2 to 0x09b2,
        0x09b6 to 0x09b9,
        0x09dc to 0x09dd,
        0x09df to 0x09e1,
        0x09f0 to 0x09f1,
        0x0a05 to 0x0a0a,
        0x0a0f to 0x0a10,
        0x0a13 to 0x0a28,
        0x0a2a to 0x0a30,
        0x0a32 to 0x0a33,
        0x0a35 to 0x0a36,
        0x0a38 to 0x0a39,
        0x0a59 to 0x0a5c,
        0x0a5e to 0x0a5e,
        0x0a72 to 0x0a74,
        0x0a85 to 0x0a8b,
        0x0a8d to 0x0a8d,
        0x0a8f to 0x0a91,
        0x0a93 to 0x0aa8,
        0x0aaa to 0x0ab0,
        0x0ab2 to 0x0ab3,
        0x0ab5 to 0x0ab9,
        0x0abd to 0x0abd,
        0x0ae0 to 0x0ae0,
        0x0b05 to 0x0b0c,
        0x0b0f to 0x0b10,
        0x0b13 to 0x0b28,
        0x0b2a to 0x0b30,
        0x0b32 to 0x0b33,
        0x0b36 to 0x0b39,
        0x0b3d to 0x0b3d,
        0x0b5c to 0x0b5d,
        0x0b5f to 0x0b61,
        0x0b85 to 0x0b8a,
        0x0b8e to 0x0b90,
        0x0b92 to 0x0b95,
        0x0b99 to 0x0b9a,
        0x0b9c to 0x0b9c,
        0x0b9e to 0x0b9f,
        0x0ba3 to 0x0ba4,
        0x0ba8 to 0x0baa,
        0x0bae to 0x0bb5,
        0x0bb7 to 0x0bb9,
        0x0c05 to 0x0c0c,
        0x0c0e to 0x0c10,
        0x0c12 to 0x0c28,
        0x0c2a to 0x0c33,
        0x0c35 to 0x0c39,
        0x0c60 to 0x0c61,
        0x0c85 to 0x0c8c,
        0x0c8e to 0x0c90,
        0x0c92 to 0x0ca8,
        0x0caa to 0x0cb3,
        0x0cb5 to 0x0cb9,
        0x0cde to 0x0cde,
        0x0ce0 to 0x0ce1,
        0x0d05 to 0x0d0c,
        0x0d0e to 0x0d10,
        0x0d12 to 0x0d28,
        0x0d2a to 0x0d39,
        0x0d60 to 0x0d61,
        0x0e01 to 0x0e2e,
        0x0e30 to 0x0e30,
        0x0e32 to 0x0e33,
        0x0e40 to 0x0e45,
        0x0e81 to 0x0e82,
        0x0e84 to 0x0e84,
        0x0e87 to 0x0e88,
        0x0e8a to 0x0e8a,
        0x0e8d to 0x0e8d,
        0x0e94 to 0x0e97,
        0x0e99 to 0x0e9f,
        0x0ea1 to 0x0ea3,
        0x0ea5 to 0x0ea5,
        0x0ea7 to 0x0ea7,
        0x0eaa to 0x0eab,
        0x0ead to 0x0eae,
        0x0eb0 to 0x0eb0,
        0x0eb2 to 0x0eb3,
        0x0ebd to 0x0ebd,
        0x0ec0 to 0x0ec4,
        0x0f40 to 0x0f47,
        0x0f49 to 0x0f69,
        0x10a0 to 0x10c5,
        0x10d0 to 0x10f6,
        0x1100 to 0x1100,
        0x1102 to 0x1103,
        0x1105 to 0x1107,
        0x1109 to 0x1109,
        0x110b to 0x110c,
        0x110e to 0x1112,
        0x113c to 0x113c,
        0x113e to 0x113e,
        0x1140 to 0x1140,
        0x114c to 0x114c,
        0x114e to 0x114e,
        0x1150 to 0x1150,
        0x1154 to 0x1155,
        0x1159 to 0x1159,
        0x115f to 0x1161,
        0x1163 to 0x1163,
        0x1165 to 0x1165,
        0x1167 to 0x1167,
        0x1169 to 0x1169,
        0x116d to 0x116e,
        0x1172 to 0x1173,
        0x1175 to 0x1175,
        0x119e to 0x119e,
        0x11a8 to 0x11a8,
        0x11ab to 0x11ab,
        0x11ae to 0x11af,
        0x11b7 to 0x11b8,
        0x11ba to 0x11ba,
        0x11bc to 0x11c2,
        0x11eb to 0x11eb,
        0x11f0 to 0x11f0,
        0x11f9 to 0x11f9,
        0x1e00 to 0x1e9b,
        0x1ea0 to 0x1ef9,
        0x1f00 to 0x1f15,
        0x1f18 to 0x1f1d,
        0x1f20 to 0x1f45,
        0x1f48 to 0x1f4d,
        0x1f50 to 0x1f57,
        0x1f59 to 0x1f59,
        0x1f5b to 0x1f5b,
        0x1f5d to 0x1f5d,
        0x1f5f to 0x1f7d,
        0x1f80 to 0x1fb4,
        0x1fb6 to 0x1fbc,
        0x1fbe to 0x1fbe,
        0x1fc2 to 0x1fc4,
        0x1fc6 to 0x1fcc,
        0x1fd0 to 0x1fd3,
        0x1fd6 to 0x1fdb,
        0x1fe0 to 0x1fec,
        0x1ff2 to 0x1ff4,
        0x1ff6 to 0x1ffc,
        0x2126 to 0x2126,
        0x212a to 0x212b,
        0x212e to 0x212e,
        0x2180 to 0x2182,
        0x3041 to 0x3094,
        0x30a1 to 0x30fa,
        0x3105 to 0x312c,
        0xac00 to 0xd7a3
      ).flatten.map(_.toChar)

    val ideographic: Seq[Char] =
      Seq(
        0x4e00 to 0x9fa5,
        0x3007 to 0x3007,
        0x3021 to 0x3029
      ).flatten.map(_.toChar)

    val letter: Seq[Char] =
      baseChar ++ ideographic

    val combiningChar: Seq[Char] = Seq(
      0x0300 to 0x0345,
      0x0360 to 0x0361,
      0x0483 to 0x0486,
      0x0591 to 0x05a1,
      0x05a3 to 0x05b9,
      0x05bb to 0x05bd,
      0x05bf to 0x05bf,
      0x05c1 to 0x05c2,
      0x05c4 to 0x05c4,
      0x064b to 0x0652,
      0x0670 to 0x0670,
      0x06d6 to 0x06dc,
      0x06dd to 0x06df,
      0x06e0 to 0x06e4,
      0x06e7 to 0x06e8,
      0x06ea to 0x06ed,
      0x0901 to 0x0903,
      0x093c to 0x093c,
      0x093e to 0x094c,
      0x094d to 0x094d,
      0x0951 to 0x0954,
      0x0962 to 0x0963,
      0x0981 to 0x0983,
      0x09bc to 0x09bc,
      0x09be to 0x09be,
      0x09bf to 0x09bf,
      0x09c0 to 0x09c4,
      0x09c7 to 0x09c8,
      0x09cb to 0x09cd,
      0x09d7 to 0x09d7,
      0x09e2 to 0x09e3,
      0x0a02 to 0x0a02,
      0x0a3c to 0x0a3c,
      0x0a3e to 0x0a3e,
      0x0a3f to 0x0a3f,
      0x0a40 to 0x0a42,
      0x0a47 to 0x0a48,
      0x0a4b to 0x0a4d,
      0x0a70 to 0x0a71,
      0x0a81 to 0x0a83,
      0x0abc to 0x0abc,
      0x0abe to 0x0ac5,
      0x0ac7 to 0x0ac9,
      0x0acb to 0x0acd,
      0x0b01 to 0x0b03,
      0x0b3c to 0x0b3c,
      0x0b3e to 0x0b43,
      0x0b47 to 0x0b48,
      0x0b4b to 0x0b4d,
      0x0b56 to 0x0b57,
      0x0b82 to 0x0b83,
      0x0bbe to 0x0bc2,
      0x0bc6 to 0x0bc8,
      0x0bca to 0x0bcd,
      0x0bd7 to 0x0bd7,
      0x0c01 to 0x0c03,
      0x0c3e to 0x0c44,
      0x0c46 to 0x0c48,
      0x0c4a to 0x0c4d,
      0x0c55 to 0x0c56,
      0x0c82 to 0x0c83,
      0x0cbe to 0x0cc4,
      0x0cc6 to 0x0cc8,
      0x0cca to 0x0ccd,
      0x0cd5 to 0x0cd6,
      0x0d02 to 0x0d03,
      0x0d3e to 0x0d43,
      0x0d46 to 0x0d48,
      0x0d4a to 0x0d4d,
      0x0d57 to 0x0d57,
      0x0e31 to 0x0e31,
      0x0e34 to 0x0e3a,
      0x0e47 to 0x0e4e,
      0x0eb1 to 0x0eb1,
      0x0eb4 to 0x0eb9,
      0x0ebb to 0x0ebc,
      0x0ec8 to 0x0ecd,
      0x0f18 to 0x0f19,
      0x0f35 to 0x0f35,
      0x0f37 to 0x0f37,
      0x0f39 to 0x0f39,
      0x0f3e to 0x0f3e,
      0x0f3f to 0x0f3f,
      0x0f71 to 0x0f84,
      0x0f86 to 0x0f8b,
      0x0f90 to 0x0f95,
      0x0f97 to 0x0f97,
      0x0f99 to 0x0fad,
      0x0fb1 to 0x0fb7,
      0x0fb9 to 0x0fb9,
      0x20d0 to 0x20dc,
      0x20e1 to 0x20e1,
      0x302a to 0x302f,
      0x3099 to 0x3099,
      0x309a to 0x309a
    ).flatten.map(_.toChar)

    val digit: Seq[Char] =
      Seq(
        0x0030 to 0x0039,
        0x0660 to 0x0669,
        0x06f0 to 0x06f9,
        0x0966 to 0x096f,
        0x09e6 to 0x09ef,
        0x0a66 to 0x0a6f,
        0x0ae6 to 0x0aef,
        0x0b66 to 0x0b6f,
        0x0be7 to 0x0bef,
        0x0c66 to 0x0c6f,
        0x0ce6 to 0x0cef,
        0x0d66 to 0x0d6f,
        0x0e50 to 0x0e59,
        0x0ed0 to 0x0ed9,
        0x0f20 to 0x0f29
      ).flatten.map(_.toChar)

    val extender: Seq[Char] =
      Seq(
        0x00b7 to 0x00b7,
        0x02d0 to 0x02d0,
        0x02d1 to 0x02d1,
        0x0387 to 0x0387,
        0x0640 to 0x0640,
        0x0e46 to 0x0e46,
        0x0ec6 to 0x0ec6,
        0x3005 to 0x3005,
        0x3031 to 0x3035,
        0x309d to 0x309e,
        0x30fc to 0x30fe
      ).flatten.map(_.toChar)

    val ncNameStartChar: Seq[Char] =
      letter ++ "_"

    val ncNameChar: Seq[Char] =
      letter ++ digit ++ ".-_" ++ combiningChar ++ extender
  }

  /* We're going through Xerces-J and scala-xml, and need to restrict
   * ourselves to the union of both.
   *
   * @see https://github.com/scala/scala-xml/issues/607
   */
  private val ncNameStartChar =
    classes.ncNameStartChar.filter(Utility.isNameStart)

  val genNcNameStartChar: Gen[Char] =
    Gen.oneOf(ncNameStartChar)

  /* We're going through Xerces-J and scala-xml, and need to restrict
   * ourselves to the union of both.
   *
   * @see https://github.com/scala/scala-xml/issues/607
   */
  private val ncNameChar =
    classes.ncNameChar.filter(Utility.isNameChar)

  val genNcNameChar: Gen[Char] =
    Gen.oneOf(ncNameChar)

  val genNcName: Gen[String] = for {
    head <- genNcNameStartChar.map(_.toString)
    n <- Gen.poisson(4)
    tail <- Gen.stringOfN(n, genNcNameChar)
  } yield head + tail

  val genPrefix: Gen[String] =
    Gen.oneOf(
      Gen.const(null),
      genNcName
    )

  val genAttrValue: Gen[String] =
    for {
      n <- Gen.poisson(5)
      s <- Gen.stringOfN(n, Gen.oneOf(classes.char))
      // Serialization trims whitespace, so we normalize ours
      tb = new TextBuffer()
      _ = tb.append(s)
    } yield tb.sb.toString

  val genAttribute: Gen[Attribute] =
    for {
      prefix <- genPrefix
      name <- genNcName
      value <- genAttrValue
    } yield Attribute(prefix, name, value, Null)

  val genAttributes: Gen[MetaData] =
    for {
      n <- Gen.geometric(0.5)
      attributes <- Gen.listOfN(n, genAttribute)
    } yield attributes.foldLeft(Null: MetaData) { (md, attr) =>
      Attribute(attr.pre, attr.key, attr.value, md)
    }

  val genText: Gen[Text] =
    for {
      n <- Gen.poisson(5)
      s <- Gen.stringOfN(n, Gen.oneOf(classes.char))
    } yield Text(s)

  val genComment: Gen[Comment] =
    for {
      n <- Gen.poisson(5)
      s <- Gen.stringOfN(n, Gen.oneOf(classes.char))
      if !s.contains("--") && !s.endsWith("-")
    } yield new Comment(s)

  val genProcInstr: Gen[ProcInstr] =
    for {
      target <- genNcName
      if target.toLowerCase(Locale.ROOT) != "xml"
      n <- Gen.poisson(5)
      s <- Gen.stringOfN(n, Gen.oneOf(classes.char))
      if !s.contains("?>")
    } yield ProcInstr(target, s)

  val genNode: Gen[Node] =
    Gen.frequency(
      20 -> genText,
      5 -> genComment,
      1 -> genProcInstr
    )

  val genContent: Gen[Seq[Node]] =
    for {
      n <- Gen.geometric(0.5)
      node <- Gen.listOfN(n, genNode)
    } yield node

  val genXml: Gen[Elem] = for {
    prefix <- genPrefix
    label <- genNcName
    attributes <- genAttributes
    children <- genContent
    elem = Elem(prefix, label, attributes, TopScope, true, children: _*)
  } yield Utility.trim(elem).asInstanceOf[Elem]
}
