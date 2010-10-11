package com.github.sonyandy.bc

import java.lang.StringBuilder
import java.lang.Thread.currentThread
import java.lang.reflect.Method
import java.util.Comparator
import java.security.AccessController.doPrivileged
import java.security.PrivilegedAction

import scala.collection.Iterable
import scala.collection.mutable.{ArrayBuffer, WrappedArray}

import org.objectweb.asm
import asm.{ClassVisitor,
            ClassWriter,
            MethodVisitor,
            Opcodes => O}
import ClassWriter.COMPUTE_MAXS
import asm.Type.getInternalName

object BC {

  private[BC] sealed trait Descriptor {

    def appendTo(descriptor: StringBuilder)
  }
  
  private[BC] sealed trait ReturnDescriptor extends Descriptor

  private[BC] sealed trait ParameterDescriptor extends Descriptor

  private[BC] object VoidDescriptor extends ReturnDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('V')
  }

  private[BC] object BooleanDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('Z')
  }
  
  private[BC] object CharDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('C')
  }
  
  private[BC] object ByteDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('B')
  }

  private[BC] object ShortDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('S')
  }
  
  private[BC] object IntDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('I')
  }

  private[BC] object FloatDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('F')
  }

  private[BC] object LongDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('J')
  }

  private[BC] object DoubleDescriptor extends ReturnDescriptor
                    with ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) = descriptor.append('D')
  }

  private[BC] final class ObjectDescriptor(private[this] val descriptor: String)
                    extends ParameterDescriptor {

    final def appendTo(descriptor: StringBuilder) {
      descriptor.append(this.descriptor)
    }
  }
  
  private[this] val defineClass = {
    classOf[ClassLoader].getDeclaredMethod("defineClass",
                                           classOf[String],
                                           classOf[Array[Byte]],
                                           Integer.TYPE,
                                           Integer.TYPE)
  }
  
  doPrivileged(new PrivilegedAction[Void] {
    final def run() = {
      defineClass.setAccessible(true)
      null
    }
  })
  
  implicit private[BC] final def loadClass[A](bc: BC[A]): Class[_ <: A] = {
    val cv = new ClassNameVisitor
    val name = try {
      bc.defineClass(cv)
      throw new AssertionError
    } catch {
      case _: NameFound => cv.name
    }
    val classLoader = currentThread.getContextClassLoader
    classLoader.synchronized {
      try {
        classLoader.loadClass(name).asInstanceOf[Class[_ <: A]]
      } catch {
        case _: ClassNotFoundException => {
          val writer = new ClassWriter(COMPUTE_MAXS)
          bc.defineClass(writer)
          val b = writer.toByteArray()
          defineClass.invoke(classLoader, name, b, 0.asInstanceOf[AnyRef],
                             b.length.asInstanceOf[AnyRef]).asInstanceOf[Class[_ <: A]]
        }
      }
    }
  }
  
  implicit final def bc2Class[A](bc: BC[A]) = bc.findClass
}
  
trait BC[A] {
  import BC._
  
  private[BC] var cv: ClassVisitor = null
  private[BC] var mv: MethodVisitor = null

  private[BC] var access: Int = 0
  
  private[BC] final def method(returnDescriptor: ReturnDescriptor,
                               methodName: String,
                               parameters: Iterable[ParameterDescriptor],
                               body: () => Unit) {
    val descriptor = new StringBuilder
    descriptor.append('(')
    parameters.foreach(_.appendTo(descriptor))
    descriptor.append(')')
    returnDescriptor.appendTo(descriptor)
    try {
      mv = cv.visitMethod(access, methodName, descriptor.toString, null, null)
      mv.visitCode()
      body()
      mv.visitMaxs(0, 0)
      mv.visitEnd()
    } finally {
      mv = null
    }
  } 

  protected[this] def `final` = {
    access = O.ACC_FINAL
    Final
  }
  
  protected[this] def public = {
    access = O.ACC_PUBLIC
    Public
  }

  protected[this] def `class` = Class
  
  protected[this] final def void(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(VoidDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def boolean(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(BooleanDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def char(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(CharDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def byte(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(ByteDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def short(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(ShortDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def int(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(IntDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def float(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(FloatDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def long(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(LongDescriptor, methodName, parameters, body _)
  }
  
  protected[this] final def double(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
    method(DoubleDescriptor, methodName, parameters, body _)
  }

  private[BC] sealed trait ClassAccess {
    
    final def `class`(internalName: String) = {
      new AccessClass(internalName)
    }
  }
  
  private[BC] sealed trait MethodAccess {
  
    final def void(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(VoidDescriptor, methodName, parameters, body _)
    }
    
    final def boolean(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(BooleanDescriptor, methodName, parameters, body _)
    }
    
    final def char(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(CharDescriptor, methodName, parameters, body _)
    }
    
    final def byte(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(ByteDescriptor, methodName, parameters, body _)
    }
    
    final def short(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(ShortDescriptor, methodName, parameters, body _)
    }
    
    final def int(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(IntDescriptor, methodName, parameters, body _)
    }
    
    final def float(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(FloatDescriptor, methodName, parameters, body _)
    }
    
    final def long(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(LongDescriptor, methodName, parameters, body _)
    }
    
    final def double(methodName: String)(parameters: ParameterDescriptor*)(body: => Unit) {
      method(DoubleDescriptor, methodName, parameters, body _)
    }
  }

  private[BC] object Final extends ClassAccess with MethodAccess {

    final def public = {
      access = O.ACC_PUBLIC | O.ACC_FINAL
      PublicFinal
    }
  }
  
  private[BC] object Public extends ClassAccess with MethodAccess {

    final def `final` = {
      access = O.ACC_PUBLIC | O.ACC_FINAL
      PublicFinal
    }
  }
  
  private[BC] object Class extends ClassAccess

  private[BC] object PublicFinal extends ClassAccess with MethodAccess

  private[BC] abstract sealed class CanHaveClassBody(protected[this] val internalName: String,
                                                     protected[this] val superName: String,
                                                     protected[this] val interfaces: Array[String]) {

    final def apply(body: => Unit) {
      try {
        cv.visit(O.V1_5, access, internalName, null, superName, interfaces)
        body
        cv.visitEnd()
      } finally {
        cv = null
      }
    }
  }
  
  private[BC] final class AccessClass(internalName: String)
                    extends CanHaveClassBody(internalName, "java/lang/Object", null) {

    final def `extends`(superName: String) = {
      new AccessClassExtends(internalName, superName)
    }

    final def implements(interface: String) = {
      new AccessClassExtendsImplements(internalName, superName, Array(interface))
    }

    final def implements(head: String, tail: String*) = {
      val length = tail.size
      val interfaces = new Array[String](tail.size + 1)
      interfaces(0) = head
      Array.copy(tail.asInstanceOf[WrappedArray[String]].array, 0,
                 interfaces, 1, length)
      new AccessClassImplements(internalName, interfaces)
    }
  }

  private[BC] final class AccessClassExtends(internalName: String,
                                             superName: String)
                    extends CanHaveClassBody(internalName, superName, null) {

    final def implements(interface: String) = {
      new AccessClassExtendsImplements(internalName, superName, Array(interface))
    }
  
    final def implements(head: String, tail: String*) = {
      val length = tail.size
      val interfaces = new Array[String](tail.size + 1)
      interfaces(0) = head
      Array.copy(tail.asInstanceOf[WrappedArray[String]].array, 0,
                 interfaces, 1, length)
      new AccessClassExtendsImplements(internalName, superName, interfaces)
    }                    
  }
  
  private[BC] final class AccessClassImplements(internalName: String,
                                                interfaces: Array[String])
                    extends CanHaveClassBody(internalName, "java/lang/Object", interfaces) {
    
    final def `extends`(superName: String) {
      new AccessClassExtendsImplements(internalName, superName, interfaces)
    }
  }
  
  private[BC] final class AccessClassExtendsImplements(internalName: String,
                                                       superName: String,
                                                       interfaces: Array[String])
                    extends CanHaveClassBody(internalName, superName, interfaces)

  implicit protected[this] final def string2ObjectParameterType(descriptor: String) = {
    new ObjectDescriptor(descriptor)
  }
  
  protected[this] final class Label extends asm.Label {
    
    final def apply() = mv.visitLabel(this)
  }

  protected[this] final def AALOAD = mv.visitInsn(O.AALOAD)
  protected[this] final def AASTORE = mv.visitInsn(O.AASTORE)
  protected[this] final def ACONST_NULL = mv.visitInsn(O.ACONST_NULL)
  protected[this] final def ALOAD(`var`: Int) = mv.visitVarInsn(O.ALOAD, `var`)
  protected[this] final def ANEWARRAY(`type`: String) = mv.visitTypeInsn(O.ANEWARRAY, `type`)
  protected[this] final def ARETURN = mv.visitInsn(O.ARETURN)
  protected[this] final def ARRAYLENGTH = mv.visitInsn(O.ARRAYLENGTH)
  protected[this] final def ASTORE(`var`: Int) = mv.visitVarInsn(O.ASTORE, `var`)
  protected[this] final def ATHROW = mv.visitInsn(O.ATHROW)
  protected[this] final def CHECKCAST(`type`: String) = mv.visitTypeInsn(O.CHECKCAST, `type`)
  protected[this] final def DCMPG = mv.visitInsn(O.DCMPG)
  protected[this] final def DCMPL = mv.visitInsn(O.DCMPL)
  protected[this] final def DLOAD(`var`: Int) = mv.visitVarInsn(O.DLOAD, `var`)
  protected[this] final def DSTORE(`var`: Int) = mv.visitVarInsn(O.DSTORE, `var`)
  protected[this] final def DUP = mv.visitInsn(O.DUP)
  protected[this] final def DUP2 = mv.visitInsn(O.DUP2)
  protected[this] final def FCMPG = mv.visitInsn(O.FCMPG)
  protected[this] final def FCMPL = mv.visitInsn(O.FCMPL)
  protected[this] final def ICONST_0 = mv.visitInsn(O.ICONST_0)
  protected[this] final def ICONST_1 = mv.visitInsn(O.ICONST_1)
  protected[this] final def ICONST_M1 = mv.visitInsn(O.ICONST_M1)
  protected[this] final def IFEQ(label: Label) = mv.visitJumpInsn(O.IFEQ, label)
  protected[this] final def IFGE(label: Label) = mv.visitJumpInsn(O.IFGE, label)
  protected[this] final def IFLE(label: Label) = mv.visitJumpInsn(O.IFLE, label)
  protected[this] final def IFNONNULL(label: Label) = mv.visitJumpInsn(O.IFNONNULL, label)
  protected[this] final def IF_ACMPNE(label: Label) = mv.visitJumpInsn(O.IF_ACMPNE, label)
  protected[this] final def IF_ICMPGE(label: Label) = mv.visitJumpInsn(O.IF_ICMPGE, label)
  protected[this] final def IF_ICMPLE(label: Label) = mv.visitJumpInsn(O.IF_ICMPLE, label)
  protected[this] final def INEG = mv.visitInsn(O.INEG)
  protected[this] final def INVOKEINTERFACE(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKEINTERFACE, owner, name, desc)
  protected[this] final def INVOKESPECIAL(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKESPECIAL, owner, name, desc)
  protected[this] final def INVOKESTATIC(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKESTATIC, owner, name, desc)
  protected[this] final def INVOKEVIRTUAL(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKEVIRTUAL, owner, name, desc)
  protected[this] final def IRETURN = mv.visitInsn(O.IRETURN)
  protected[this] final def ISUB = mv.visitInsn(O.ISUB)
  protected[this] final def LCMP = mv.visitInsn(O.LCMP)
  protected[this] final def LLOAD(`var`: Int) = mv.visitVarInsn(O.LLOAD, `var`)
  protected[this] final def LSTORE(`var`: Int) = mv.visitVarInsn(O.LSTORE, `var`)
  protected[this] final def POP = mv.visitInsn(O.POP)
  protected[this] final def SWAP = mv.visitInsn(O.SWAP)
  protected[this] final def RETURN = mv.visitInsn(O.RETURN)
  
  protected[this] def defineClass()

  final def defineClass(cv: ClassVisitor) {
    this.cv = cv
    defineClass
  }
  
  final def findClass()(implicit loadClass: BC[A] => Class[_ <: A]) = {
    loadClass(this)
  }
}
