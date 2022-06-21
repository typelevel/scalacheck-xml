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

  /* We're going through Xerces-J and scala-xml, and need to restrict
   * ourselves to the union of both.
   *
   * @see https://github.com/scala/scala-xml/issues/607
   */
  private val ncNameStartChar =
    characterClasses.ncNameStartChar.filter(Utility.isNameStart)

  val genNcNameStartChar: Gen[Char] =
    Gen.oneOf(ncNameStartChar)

  /* We're going through Xerces-J and scala-xml, and need to restrict
   * ourselves to the union of both.
   *
   * @see https://github.com/scala/scala-xml/issues/607
   */
  private val ncNameChar =
    characterClasses.ncNameChar.filter(Utility.isNameChar)

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
      s <- Gen.stringOfN(n, Gen.oneOf(characterClasses.char))
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
      s <- Gen.stringOfN(n, Gen.oneOf(characterClasses.char))
    } yield Text(s)

  val genComment: Gen[Comment] =
    for {
      n <- Gen.poisson(5)
      s <- Gen.stringOfN(n, Gen.oneOf(characterClasses.char))
      if !s.contains("--") && !s.endsWith("-")
    } yield new Comment(s)

  val genProcInstr: Gen[ProcInstr] =
    for {
      target <- genNcName
      if target.toLowerCase(Locale.ROOT) != "xml"
      n <- Gen.poisson(5)
      s <- Gen.stringOfN(n, Gen.oneOf(characterClasses.char))
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
