package org.jetbrains.plugins.scala
package lang
package parser
package parsing
package params

import org.jetbrains.plugins.scala.lang.lexer.{ScalaTokenType, ScalaTokenTypes}
import org.jetbrains.plugins.scala.lang.parser.parsing.builder.ScalaPsiBuilder
import org.jetbrains.plugins.scala.lang.parser.parsing.expressions.{Annotations, Expr}
import org.jetbrains.plugins.scala.lang.parser.parsing.types.ParamType

/**
 * [[Param]] ::= [[Annotations]] id [':' ParamType] ['=' Expr]
 */
object Param extends ParsingRule {

  override def parse(implicit builder: ScalaPsiBuilder): Boolean = {
    val paramMarker = builder.mark()

    Annotations()

    //empty modifiers
    val modifiersMarker = builder.mark()
    modifiersMarker.done(ScalaElementType.MODIFIERS)

    if (builder.isScala3 && builder.lookAhead(1, ScalaTokenTypes.tIDENTIFIER)) {
      builder.tryParseSoftKeyword(ScalaTokenType.InlineKeyword)
    }

    builder.getTokenType match {
      case ScalaTokenTypes.tIDENTIFIER =>
        builder.advanceLexer() //Ate id
      case _ =>
        paramMarker.rollbackTo()
        return false
    }

    builder.getTokenType match {
      case ScalaTokenTypes.tCOLON =>
        builder.advanceLexer() //Ate :
        if (!ParamType()) builder error ErrMsg("wrong.type")
      case _ =>
    }
    builder.getTokenType match {
      case ScalaTokenTypes.tASSIGN =>
        builder.advanceLexer() //Ate =
        if (!Expr()) builder error ErrMsg("wrong.expression")
      case _ =>
    }
    paramMarker.done(ScalaElementType.PARAM)
    true
  }
}