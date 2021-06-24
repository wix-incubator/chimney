package io.scalaland.chimney.internal.utils

import scala.StringContext.treatEscapes

object ErrInterpolator {
  val NewLine = "\u200B\n"; // scalac doesn't allow non-printable chars apparently

  implicit class ErrInterpolator(private val sc: StringContext) extends AnyVal {
    def err(args: Any*): String = sc.standardInterpolator(treatEscapes, args).split('\n').mkString(NewLine)
  }
}
