package com.github.sonyandy.bc

import scala.util.control.ControlThrowable
  
import org.objectweb.asm.commons.EmptyVisitor
  
private[bc] sealed trait NameFound extends Throwable
private[bc] object NameFound extends ControlThrowable with NameFound

private[bc] final class ClassNameVisitor extends EmptyVisitor {
  
  private[bc] var name: String = null

  override final def visit(version: Int,
                           access: Int,
                           name: String,
                           signature: String,
                           superName: String,
                           interfaces: Array[String]) {
    this.name = name
    throw NameFound
  }
}
