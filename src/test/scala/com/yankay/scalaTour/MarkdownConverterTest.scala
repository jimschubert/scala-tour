package com.yankay.scalaTour

import org.specs2.mutable._
import scala.xml.NodeSeq
import scala.xml.Utility.{trim => tr}

class MarkdownConverterTest extends Specification {

  // TODO Convert this to a proper specs2 matcher
  def xmlValuesShouldEqual(first: NodeSeq, second: NodeSeq): Boolean = {
    first.flatten(x => tr(x)) == second.flatten(x => tr(x))
  }

  "MarkdownConverter" should {
    "class accepts a string as input" in {
      val input = "*some input*"
      val mdc = new MarkdownConverter(input)

      mdc.input must beTheSameAs(input)
    }

    "companion accepts a string as input" in {
      val input = "*some input*"
      val mdc = MarkdownConverter(input)

      mdc.input must beTheSameAs(input)
    }

    "converts string to MarkdownConverter instance" in {
      var mdc: MarkdownConverter = null
      val input: String = "*input*"
      mdc = input

      mdc must beAnInstanceOf[MarkdownConverter]
      mdc.input must beEqualTo(input)
    }

    "converts markdown to expected html" in {
      val input: String =
        """
          |# Scala Tour
          |
          | Hello, *World*!
          |
          | ## Example
          |
          | `val x: String = "Hi"`
          |
          | **bold** text *italic*""".stripMargin

      val expected = """<h1>Scala Tour</h1><p> Hello, <em>World</em>!
                       |</p><p> ## Example
                       |</p><p> <code>val x: String = &quot;Hi&quot;</code>
                       |</p><p> <strong>bold</strong> text <em>italic</em></p>""".stripMargin

      MarkdownConverter(input).asHtml.toString() must equalTo(expected)
    }

    "converts bold to expected html" in {
      val input: String = "**input**"
      val expected = <p><strong>input</strong></p>

      val contents = MarkdownConverter(input).asHtml
      xmlValuesShouldEqual(contents, expected) must beTrue
    }

    "handles unicode (Chinese) characters" in {
      val input: String = "**表达式和值**"
      val expected = <p><strong>表达式和值</strong></p>

      val contents = MarkdownConverter(input).asHtml
      xmlValuesShouldEqual(contents, expected) must beTrue
    }

    "finds a title from markdown with title" in {
      val input: String =
        """# Some title
          | Some more text
        """.stripMargin

      val expected: String = "Some title"

      MarkdownConverter(input).title must equalTo(expected)
    }

    "finds no title from markdown missing title" in {
      val input: String =
        """Some preceding text
          | Some title
          | Some more text
        """.stripMargin

      val expected: String = ""

      MarkdownConverter(input).title must equalTo(expected)
    }

    "finds desired (first) code block" in {
      val input: String =
        """# Sample code
          |Some preceding text
          |
          |    var x: String = "found"
          |
          |Some more text
        """.stripMargin

      val expected: String = """var x: String = "found""""

      MarkdownConverter(input).code.trim must equalTo(expected)
    }

    "finds desired content block" in {
      val input: String =
        """# Sample code
          |
          |    var x: String = "found"
          |
          |This is the contents with some `extra` code.
          |
          |It must include *all* contents.
        """.stripMargin

      val expected = <p>This is the contents with some <code>extra</code> code.
      </p><p>It must include <em>all</em> contents.
      </p>;

      val contents = MarkdownConverter(input).contents
      xmlValuesShouldEqual(contents, expected) must beTrue
    }

    "finds desired content block" in {
      val input: String =
        """# Sample code
          |Before Text
          |
          |    var x: String = "found"
          |
          |This is the contents with some `extra` code.
          |
          |It must include *all* contents.
        """.stripMargin

      val expected =  <p>Before Text
        </p><p>This is the contents with some <code>extra</code> code.
        </p><p>It must include <em>all</em> contents.
        </p>;

      val contents = MarkdownConverter(input).contents
      xmlValuesShouldEqual(contents, expected) must beTrue
    }
  }
}