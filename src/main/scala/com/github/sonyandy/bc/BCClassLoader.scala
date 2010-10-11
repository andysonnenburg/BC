package com.github.sonyandy.bc

import org.objectweb.asm.ClassWriter
import ClassWriter.COMPUTE_MAXS
  
final class BCClassLoader(parent: ClassLoader) extends ClassLoader(parent) {

  def this() = this(ClassLoader.getSystemClassLoader)
  
  final def loadClass[A](bc: BC[A]): Class[A] = {
    val cv = new ClassNameVisitor
    val name = try {
      bc.defineClass(cv)
      throw new AssertionError
    } catch {
      case _: NameFound => cv.name
    }
    synchronized {
      try {
        loadClass(name).asInstanceOf[Class[A]]
      } catch {
        case _: ClassNotFoundException => {
          val writer = new ClassWriter(COMPUTE_MAXS)
          bc.defineClass(writer)
          val b = writer.toByteArray()
          defineClass(name, b, 0, b.length).asInstanceOf[Class[A]]
        }
      }
    }
  }
}
