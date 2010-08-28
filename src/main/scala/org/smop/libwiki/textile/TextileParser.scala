package org.smop.libwiki.textile

import util.parsing.combinator.RegexParsers
import xml.{Text, TopScope, Elem, NodeSeq, Null}

class TextileParsers extends RegexParsers {
  override def skipWhitespace = false
  def doc: Parser[NodeSeq] = rep(block) ^^ (_.flatten)
  def block: Parser[NodeSeq] = heading | para
  def heading: Parser[NodeSeq] = ("^h".r ~> "[1-6]".r <~ """\.\s+""".r) ~ lines ^^
          { case n~content => Elem(null, "h"+n, Null, TopScope, content: _*)}
  def para: Parser[NodeSeq] = lines ^^ (x => <p>{x}</p>)
  def eol: Parser[Any] = """\r?\n""".r
  def lines: Parser[NodeSeq] = ((rep1sep(inline, eol) ^^
          (_.reduceLeft((_ : NodeSeq) ++ <br/> ++ _))) ^^
          ( x => x.reduceLeft((_: NodeSeq) ++ _))) <~ rep(eol)
  def inline: Parser[NodeSeq] = italic | emphasis |plaintext //  bold | strong |
  def plaintext = """\S.*(?<!_)""".r ^^ (Text(_))
  def italic = "__" ~> (inline ^^ (x => <i>{x}</i>)) <~ "__"
  def emphasis = "_" ~> (inline ^^ (x => <em>{x}</em>)) <~ "_"
}

object TextileParser extends TextileParsers {
  def parseDoc(in: CharSequence): NodeSeq = parseAll(doc, in).get
}
