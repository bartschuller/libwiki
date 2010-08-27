package org.smop.libwiki

trait Wiki {
  def syntaxes: List[String] = Nil

}

trait Textile extends Wiki {
  override def syntaxes = "textile" :: super.syntaxes
}