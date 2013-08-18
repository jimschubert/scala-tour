package com.yankay.scalaTour

import scala.language.implicitConversions
import com.tristanhunt.knockoff.DefaultDiscounter._

class MarkdownConverter(val input: String) {
  lazy val title = {
    (asHtml \\ "h1").text
  }

  lazy val code = {
    (asHtml \\ "pre").text
  }

  lazy val contents = {
    (asHtml \\ "p")
  }

  lazy val asHtml = {
    toXHTML(knockoff(input))
  }
}

object MarkdownConverter {
  implicit def toHtml(input: String) = new MarkdownConverter(input)

  def apply(input: String) = {
    val mdc = new MarkdownConverter(input)
    mdc
  }
}