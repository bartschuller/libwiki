import org.smop.libwiki.{Wiki, Textile}
import org.specs._

object WikiDomainSpec extends Specification {
  object wiki extends Wiki with Textile
  def have = addToSusVerb("have")
  "a wiki" should {
    "have a way to create pages from source text" in {}
    "allow editing pages in source text syntax" in {}
    "provide pages by key" in {}
    "render pages to html" in {}
    "allow pages to be renamed" in {}
    "provide a list of all pages" in {}
    "provide a list of recent changes for all pages" in {}
    "list the syntaxes it understands" in {
      wiki.syntaxes must contain("textile")
    }
    "allow plugins" in {}
    "group pages into 'spaces' (optional)" in {}
  }
  "a wiki page" should have {
      "an author and other obvious metadata, e.g. time stamps" in {}
      "tags" in {}
      "file attachments (optional)" in {}
  }
  "a versioning wiki" can {
    "provide a version timeline per page" in {}
    "show differences between versions of a page" in {}
    "allow rollback to an earlier version of a page" in {}
  }
  "a wiki with comments" can {
    "attach a comment to a page" in {}
    "attach a comment to a comment" in {}
    "render a tree of comments as html" in {}
  }
  "a searchable wiki" can {
    "be searched using a text query" in {}
  }
}