import org.specs._

object WikiDomainSpec extends Specification {
  "a wiki" should {
    "have a way to create pages from source text" in {}
    "provide pages by key" in {}
    "render pages to html" in {}
    "allow pages to be renamed" in {}
    "list the syntaxes it understands" in {}
    "allow plugins" in {}
  }
  "a versioning wiki" can {
    "provide a version timeline" in {}
    "show differences between versions" in {}
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