package org.jetbrains.plugins.scala
package lang
package actions
package editor
package autobraces

class AutoBracesTypingTest extends AutoBraceTestBase {
//  def testBlubBlub(): Unit = checkGeneratedTextAfterTyping(
//    s"""
//       |for (e <- expr)
//       |  $CARET
//       |  expr
//       |""".stripMargin,
//    s"""
//       |for (e <- expr) {
//       |  e$CARET
//       |  expr
//       |}
//       |""".stripMargin,
//    'e'
//  )


  def testTypingAfterIndentation(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  expr
       |  $CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test = {
       |  expr
       |  x$CARET
       |}
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |def test =
       |  expr
       |  x$CARET
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  def testTypingAfterIndentBeforeIndentedExpr(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  $CARET
       |  expr
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test = {
       |  x$CARET
       |  expr
       |}
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |def test =
       |  x$CARET
       |  expr
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  def testTypingAfterDoubleIndentation(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  expr
       |   $CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  expr
       |   x$CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  expr
       |   x$CARET
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  def testTypingAfterSecondIndentation(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  expr
       |   .prod
       |  $CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test = {
       |  expr
       |   .prod
       |  x$CARET
       |}
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |def test =
       |  expr
       |   .prod
       |  x$CARET
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  def testTypingInUnindented(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  expr
       |$CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  expr
       |x$CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  expr
       |x$CARET
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  def testTypingBetweenCommentAndIndentedExpr(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  // test
       |  $CARET
       |  expr
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test = {
       |  // test
       |  x$CARET
       |  expr
       |}
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |def test =
       |  // test
       |  x$CARET
       |  expr
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  def testTypingInsideOfIndentedCall(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  call(
       |  $CARET
       |  )
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  call(
       |  x$CARET
       |  )
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  call(
       |  x$CARET
       |  )
       |""".stripMargin -> ContinuationOnNewline,
    'x'
  )

  // SCL-17794
  def testClosingStringQuote(): Unit = checkTypingInAllContexts(
    s"""
       |def test =
       |  "test"
       |  $CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test = {
       |  "test"
       |  "$CARET"
       |}
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |def test =
       |  "test"
       |  "$CARET"
       |""".stripMargin -> ContinuationOnNewline,
    '\"'
  )

  // SCL-17793
  def testNoAutoBraceOnDot(): Unit = checkTypingInUncontinuedContexts(
    s"""
       |def test =
       |  expr
       |  $CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  expr
       |  .$CARET
       |""".stripMargin -> ContinuationOnNewline,
    s"""
       |def test =
       |  expr
       |  .$CARET
       |""".stripMargin -> ContinuationOnNewline,
    '.'
  )

  def testTypingBeforeContinuation(): Unit = checkTypingInContinuedContexts(
    s"""
       |try
       |  expr
       |  $CARET
       |finally ()
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |try {
       |  expr
       |  a$CARET
       |} finally ()
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |try
       |  expr
       |  a$CARET
       |finally ()
       |""".stripMargin -> ContinuationOnSameLine,
    'a'
  )

  // todo: fix SCL-17843
  /*def testMultilineBeforeContinuation(): Unit = checkTypingInContinuedContexts(
    s"""
       |try
       |  expr
       |  $CARET
       |
       |
       |
       |
       |finally ()
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |try {
       |  expr
       |  a$CARET
       |
       |
       |
       |
       |} finally ()
       |""".stripMargin -> ContinuationOnSameLine,
    s"""
       |try
       |  expr
       |  a$CARET
       |
       |
       |
       |
       |finally ()
       |""".stripMargin -> ContinuationOnSameLine,
    'a'
  )*/
}