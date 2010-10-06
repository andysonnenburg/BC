package com.github.sonyandy.bc

import java.lang.Thread.currentThread
import java.lang.reflect.Method
import java.util.Comparator
import java.security.AccessController.doPrivileged
import java.security.PrivilegedAction

import scala.collection.mutable.ArrayBuffer

import org.objectweb.asm
import asm.{ClassVisitor,
            ClassWriter,
            MethodVisitor,
            Opcodes => O}
import ClassWriter.COMPUTE_MAXS
import asm.Type.getInternalName
import asm.commons.EmptyVisitor

object BC {
  import BC._

  private[BC] sealed trait Type {
    def desc(builder: StringBuilder)
  }
  
  private[BC] sealed trait ReturnType extends Type
  private[BC] sealed trait ParameterType extends Type
  
  private[BC] object Void extends ReturnType {
    def desc(builder: StringBuilder) {
      builder.append('V')
    }
  }

  private[BC] object Int extends ParameterType with ReturnType {
    def desc(builder: StringBuilder) {
      builder.append('I')
    }
  }

  final class ObjectType(private[BC] val internalName: String) extends ParameterType with ReturnType {
    def desc(builder: StringBuilder) {
      builder.append('L').append(internalName).append(';')
    }
  }

  implicit final def class2ObjectType(`class`: Class[_]) = {
    new ObjectType(getInternalName(`class`))
  }

  private[BC] trait NoStackTrace extends Throwable {
    override def fillInStackTrace(): Throwable = this
  }
  
  private[BC] final class NameFound extends Throwable with NoStackTrace

  private[BC] val nameFoundException = new NameFound

  private[BC] def nameFound { throw nameFoundException }
  
  private[BC] final class ClassNameVisitor extends EmptyVisitor {
    
    private[BC] var name: String = null

    override final def visit(version: Int,
                             access: Int,
                             name: String,
                             signature: String,
                             superName: String,
                             interfaces: Array[String]) {
      this.name = name
      nameFound
    }
  }
  
  private[BC] object LoadClass {
    private[this] val defineClass = classOf[ClassLoader].getDeclaredMethod("defineClass",
                                                                           classOf[String],
                                                                           classOf[Array[Byte]],
                                                                           Integer.TYPE,
                                                                           Integer.TYPE)
    doPrivileged(new PrivilegedAction[Void] {
      final def run() = {
        defineClass.setAccessible(true)
        null
      }
    })
    
    private[this] val classLoader = doPrivileged(new PrivilegedAction[ClassLoader] {
      final def run() = {
        currentThread.getContextClassLoader
      }
    })

    private[BC] final def apply(bc: BC): Class[_] = {
      val cv = new ClassNameVisitor
      bc.cv = cv
      val name = try {
        bc.privateDefineClass()
        throw new AssertionError
      } catch {
        case _: NameFound => cv.name
      }
      classLoader.synchronized {
        try {
          classLoader.loadClass(name)
        } catch {
          case _: ClassNotFoundException => {
            val writer = new ClassWriter(COMPUTE_MAXS)
            bc.cv = writer
            bc.privateDefineClass()
            val b = writer.toByteArray()
            defineClass.invoke(classLoader, name, b, 0.asInstanceOf[AnyRef],
                               b.length.asInstanceOf[AnyRef]).asInstanceOf[Class[_]]
          }
        } 
      }
    }
  }
}
  
trait BC {
  import BC._

  type ObjectType = BC.ObjectType
  
  private[BC] var cv: ClassVisitor = null
  private[BC] var mv: MethodVisitor = null
  
  protected[this] final def `abstract` = new AbstractWord(O.ACC_ABSTRACT)
  protected[this] final def `final` = new AccessBuilder(O.ACC_FINAL)
  protected[this] final def `private` = new AccessBuilder(O.ACC_PRIVATE)
  protected[this] final def `protected` = new AccessBuilder(O.ACC_PROTECTED)
  protected[this] final def public = new AccessBuilder(O.ACC_PUBLIC)

  protected[this] final def `class`(name: String) = new ClassName(name)

  protected[this] final def void(name: String) = new MethodDeclarationBuilder(0, Void, name)
  protected[this] final def int(name: String) = new MethodDeclarationBuilder(0, Int, name)
  
  protected[this] def int = Int

  private[BC] final class AccessBuilder(private var opcode: Int) {
    def `abstract` = { opcode |= O.ACC_ABSTRACT; this }
    def `final` = { opcode |= O.ACC_FINAL; this }
    def `private` = { opcode |= O.ACC_PRIVATE; this }
    def `protected` = { opcode |= O.ACC_PROTECTED; this }
    def public = { opcode |= O.ACC_PUBLIC; this }

    // def `class`(name: String) = new ClassDeclarationBuilder(opcode, name)

    // def void(name: String) = new MethodDeclarationBuilder(opcode, Void, name)
    // def int(name: String) = new MethodDeclarationBuilder(opcode, Int, name)
    def apply(name: ClassName): ClassDeclarationBuilder = {
      new ClassDeclarationBuilder(opcode, name.name)
    }
  }

  private[BC] final class ResultOfClassWordApplication(val name: String)
  
  private[BC] final class ClassDeclarationBuilder(private val access: Int,
                                                  private val name: String) {
    private[this] var superName = "java/lang/Object"
    private[this] val interfaces = new ArrayBuffer[String]

    final def `extends`(superType: ObjectType) {
      superName = superType.internalName
    }
  
    final def implements(interfaceTypes: ObjectType*) = {
      for (interfaceType <- interfaceTypes) {
        interfaces.append(interfaceType.internalName)
      }
      this
    }
      
    final def apply(body: => Unit) {
      cv.visit(O.V1_5, access, name, null, superName, interfaces.toArray)
      body
      cv.visitEnd
      cv = null
    }
  }

  private[BC] final class MethodDeclarationBuilder(private val access: Int,
                                                   private val returnType: ReturnType,
                                                   private val name: String) {
    final def apply(parameterTypes: ParameterType*) = new MethodDeclaration(access, returnType, name, parameterTypes)
  }

  private[BC] final class MethodDeclaration(private val access: Int,
                                            private val returnType: ReturnType,
                                            private val name: String,
                                            private val parameterTypes: Seq[ParameterType]) {
    final def apply(body: => Unit) {
      val desc = new StringBuilder
      desc.append('(')
      for (parameterType <- parameterTypes) {
        parameterType.desc(desc)
      }
      desc.append(')')
      returnType.desc(desc)
      mv = cv.visitMethod(access, name, desc.toString, null, null)
      mv.visitCode()
      body
      mv.visitMaxs(0, 0)
      mv.visitEnd()
      mv = null
    }
  }

  protected[this] final class Label extends asm.Label {
    def apply() = mv.visitLabel(this)
  }
  
  protected[this] final def ALOAD(`var`: Int) = mv.visitVarInsn(O.ALOAD, `var`)      
  protected[this] final def ASTORE(`var`: Int) = mv.visitVarInsn(O.ASTORE, `var`)
  protected[this] final def CHECKCAST(`type`: String) = mv.visitTypeInsn(O.CHECKCAST, `type`)
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
  protected[this] final def INVOKEINTERFACE(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKEINTERFACE, owner, name, desc)
  protected[this] final def INVOKESPECIAL(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKESPECIAL, owner, name, desc)
  protected[this] final def INVOKEVIRTUAL(owner: String, name: String, desc: String) = mv.visitMethodInsn(O.INVOKEVIRTUAL, owner, name, desc)
  protected[this] final def IRETURN = mv.visitInsn(O.IRETURN)
  protected[this] final def ISUB = mv.visitInsn(O.ISUB)
  protected[this] final def POP = mv.visitInsn(O.POP)
  protected[this] final def SWAP = mv.visitInsn(O.SWAP)
protected[this] final def RETURN = mv.visitInsn(O.RETURN)
  protected[this] def defineClass()

  final def defineClass(cv: ClassVisitor) {
    this.cv = cv
    defineClass
  }
  
  private[BC] def privateDefineClass() = defineClass()

  final def findClass(): Class[_] = {
    LoadClass(this)
  }
}

  
