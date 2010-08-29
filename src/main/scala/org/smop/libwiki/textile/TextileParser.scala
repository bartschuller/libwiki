package org.smop.libwiki.textile

import util.parsing.combinator.RegexParsers
import xml.{Text, TopScope, Elem, NodeSeq, Null}

class TextileParsers extends RegexParsers {
  override def skipWhitespace = false
  def doc: Parser[NodeSeq] = rep(block) ^^ (_.flatten)
  def block: Parser[NodeSeq] = heading | blockquote | para
  def heading: Parser[NodeSeq] = ("^h".r ~> "[1-6]".r <~ """\.\s+""".r) ~ lines ^^
          { case n~content => Elem(null, "h"+n, Null, TopScope, content: _*)}
  def blockquote: Parser[NodeSeq] = ("""^bq\.\s+""".r) ~> para ^^
          (x => <blockquote>{x}</blockquote>)
  def unordered: Parser[NodeSeq] = ("""^\*\s+""".r) ~> listlines ^^
          (x => <ul>{x}</ul>)
  def listlines: Parser[NodeSeq] = ((rep1sep(inline, eol) ^^
          (_.reduceLeft((_ : NodeSeq) ++ <br/> ++ _))) ^^
          (_.flatten : NodeSeq)) <~ rep(eol)
  def para: Parser[NodeSeq] = lines ^^ (x => <p>{x}</p>)
  def eol: Parser[Any] = """\r?\n""".r
  def lines: Parser[NodeSeq] = ((rep1sep(inline, eol) ^^
          (_.reduceLeft((_ : NodeSeq) ++ <br/> ++ _))) ^^
          (_.flatten : NodeSeq)) <~ rep(eol)
  def inline: Parser[NodeSeq] = italic | emphasis | bold | strong |plaintext
  def plaintext = """\S.*(?<![_*])""".r ^^ (Text(_))
  def italic = "__" ~> (inline ^^ (x => <i>{x}</i>)) <~ "__"
  def emphasis = "_" ~> (inline ^^ (x => <em>{x}</em>)) <~ "_"
  def bold = "**" ~> (inline ^^ (x => <b>{x}</b>)) <~ "**"
  def strong = "*" ~> (inline ^^ (x => <strong>{x}</strong>)) <~ "*"
}

object TextileParser extends TextileParsers {
  def parseDoc(in: CharSequence): NodeSeq = parseAll(doc, in).get
}
