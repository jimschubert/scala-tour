package com.yankay.scalaTour

import org.specs2.mutable._

class MarkdownConverterTest extends Specification {
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

      val expected: String = """<h1>Scala Tour</h1><p> Hello, <em>World</em>!
                       |</p><p> ## Example
                       |</p><p> <code>val x: String = &quot;Hi&quot;</code>
                       |</p><p> <strong>bold</strong> text <em>italic</em></p>""".stripMargin

      MarkdownConverter(input).asHtml.toString() must equalTo(expected)
    }

    "converts bold to expected html" in {
      val input: String = "**input**"
      val expected: String = """<p><strong>input</strong></p>"""

      MarkdownConverter(input).asHtml.toString must equalTo(expected)
    }

    "handles unicode (Chinese) characters" in {
      val input: String = "**表达式和值**"
      val expected: String = """<p><strong>表达式和值</strong></p>"""

      MarkdownConverter(input).asHtml.toString must equalTo(expected)
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
  }
}
