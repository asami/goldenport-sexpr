package org.goldenport.sexpr.eval

import java.util.Locale
import org.apache.commons.jxpath._

/*
 * @since   Sep. 18, 2018
 * @version Sep. 19, 2018
 * @author  ASAMI, Tomoharu
 */
trait JXPathPart[C <: LispContext] { self: LispEvaluator[C] =>
  def f(p: String) = {
    val context = JXPathContext.newContext(p)
    context.getValue(".").asInstanceOf[String]
  }
}

object JXPathPart {
  import org.apache.commons.jxpath.ri._
  import org.apache.commons.jxpath.ri.model._

  class A(
    parent: JXPathContext,
    contextbean: AnyRef,
    contextpointer: Pointer
  ) extends JXPathContextReferenceImpl(parent, contextbean, contextpointer) {
  }

  class B(parent: NodePointer, locale: Locale) extends NodePointer(parent, locale) {
   def compareChildNodePointers(x$1: NodePointer,x$2: NodePointer): Int = ???
   def getBaseValue(): Object = ???
   def getImmediateNode(): Object = ???
   def getLength(): Int = ???
   def getName(): QName = ???
   def isCollection(): Boolean = ???
   def isLeaf(): Boolean = ???
   def setValue(x$1: Any): Unit = ???
  }

  class C() extends NodeIterator {
   def getNodePointer(): NodePointer = ???
   def getPosition(): Int = ???
   def setPosition(x$1: Int): Boolean = ???
  }

  class D() extends NodePointerFactory {
   def createNodePointer(x$1: NodePointer,x$2: QName,x$3: Any): NodePointer = ???
   def createNodePointer(x$1: QName,x$2: Any,x$3: java.util.Locale): NodePointer = ???
   def getOrder(): Int = ???
  }
}
