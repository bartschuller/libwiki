import org.specs._
import org.smop.libwiki.textile.TextileParser
import org.smop.libwiki.textile.TextileParser._

object TextileSpec extends Specification {
  def parse = addToSusVerb("parse")
  "a textile parser" should parse {
    "headings" in {
      TextileParser.parseDoc("h1. Hello") must ==/(<h1>Hello</h1>)
      TextileParser.parseDoc("h6. Hello\nthere") must ==/(<h6>Hello<br/>there</h6>)
    }
    "paragraphs" in {
      TextileParser.parseDoc("Hello") must ==/(<p>Hello</p>)
      TextileParser.parseDoc("Hello\nthere") must ==/(<p>Hello<br/>there</p>)
    }
    "a mix of headings and paragraphs" in {
      TextileParser.parseDoc("h1. Hello\n\nthere\n\nh2. h2") must ==/(<h1>Hello</h1><p>there</p><h2>h2</h2>)
    }
    "italics" in {
      TextileParser.parseDoc("__Hello__") must ==/(<p><i>Hello</i></p>)
    }
    "emphasis" in {
      TextileParser.parseDoc("_Hello_") must ==/(<p><em>Hello</em></p>)
    }
  }
}
