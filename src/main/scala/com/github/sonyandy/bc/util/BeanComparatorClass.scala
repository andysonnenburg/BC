package com.github.sonyandy.bc.util

import java.lang.StringBuilder
import java.lang.reflect.Method
import java.lang.reflect.Modifier.isStatic
import java.util.Comparator

import scala.collection.Set
import scala.collection.mutable.HashMap

import com.github.sonyandy.bc.BC
import com.github.sonyandy.bc.util.BeanComparatorClass.Field

import org.objectweb.asm.Type
import Type.{getInternalName, getMethodDescriptor, getType}

object BeanComparatorClass {
  class Field(val name: String, val descending: Boolean) {
    def this(name: String) = this(name, false)
  }
}

final class BeanComparatorClass[A](private[this] val beanClass: Class[A],
                                   private[this] val fields: Array[Field]) extends BC[Comparator[A]] {

  private[this] val beanInternalName = getInternalName(beanClass)
  private[this] val comparatorClassName = {
    val builder = new StringBuilder
    builder.append("$BeanComparator$$")
    builder.append(beanClass.getName.replace('.', '$'))
    for (field <- fields) {
      builder.append("$$").append(field.name).append("$$")
      if (field.descending) {
        builder.append("D")
      } else {
        builder.append("A")
      }
    }
    builder.toString
  }
  private[this] val beanProperties = {
    val properties = new HashMap[String, Method]
    val methods = beanClass.getMethods
    for (method <- methods) {
      for(propertyName <- propertyName(method)) {
        properties.put(propertyName, method)
      }
    }
    properties
  }

  private[this] def propertyName(method: Method): Option[String] = {
    (isStatic(method.getModifiers),
     method.getParameterTypes.length,
     method.getReturnType) match {
      case (_, _, java.lang.Void.TYPE) => None
      case (false, 0, java.lang.Boolean.TYPE) => {
        val name = method.getName
        if (!name.startsWith("is")) None
        else Some(name(2).toLower + name.substring(3))
      }
      case (false, 0, _) => {
        val name = method.getName
        if (!name.startsWith("get")) None
        else Some(name(3).toLower + name.substring(4))
      }
      case _ => None
    }
  }

  protected[this] final def defineClass() {

    public.`final`.`class`(comparatorClassName).implements("java/util/Comparator") {

      public.void("<init>")() {
        ALOAD(0)
        INVOKESPECIAL("java/lang/Object", "<init>", "()V")
        RETURN
      }

      public.`final`.int("compare")("Ljava/lang/Object;", "Ljava/lang/Object;") {
        ALOAD(1)
        ALOAD(2)
        val NE = new Label
        IF_ACMPNE(NE)
        ICONST_0
        IRETURN
        NE()
        ALOAD(1)
        val NONNULL1 = new Label
        IFNONNULL(NONNULL1)
        ICONST_M1
        IRETURN
        NONNULL1()
        ALOAD(2)
        val NONNULL2 = new Label
        IFNONNULL(NONNULL2)
        ICONST_1
        IRETURN
        NONNULL2()
        ALOAD(1)
        CHECKCAST(beanInternalName)
        ASTORE(1)
        ALOAD(2)
        CHECKCAST(beanInternalName)
        ASTORE(2)
        for (field <- fields) {
          for (accessor <- getAccessor(field.name)) {
            ALOAD(1)
            val accessorDescriptor = getMethodDescriptor(accessor)
            INVOKEVIRTUAL(beanInternalName,
                          accessor.getName,
                          accessorDescriptor)
            ALOAD(2)
            INVOKEVIRTUAL(beanInternalName,
                          accessor.getName,
                          accessorDescriptor)
            defineComparison(getType(accessor.getReturnType), field.descending)
          }
        }
        ICONST_0
        IRETURN
      }
    }
  }

  private[this] final def getAccessor(propertyName: String): Option[Method] = {
    beanProperties.get(propertyName)
  }

  private[this] final def defineComparison(`type`: Type, descending: Boolean) {
    `type`.getSort match {
      case
        Type.BOOLEAN
      | Type.BYTE
      | Type.CHAR
      | Type.SHORT => defineShortComparison(descending)
      case Type.INT => defineIntComparison(descending)
      case Type.FLOAT => defineFloatComparison(descending)
      case Type.DOUBLE => defineDoubleComparison(descending)
      case Type.LONG => defineLongComparison(descending)
      case Type.ARRAY => {
        throw new UnsupportedOperationException
      }
      case Type.OBJECT => defineObjectComparison(descending)
    }
  }

  private[this] final def defineShortComparison(descending: Boolean) {
    ISUB
    DUP
    val EQ = new Label
    IFEQ(EQ)
    if (descending) {
      INEG
    }
    IRETURN
    EQ()
    POP
  }
  
  private[this] final def defineIntComparison(descending: Boolean) {
    DUP2
    val LE = new Label
    IF_ICMPLE(LE)
    if (descending) {
      ICONST_M1
    } else {
      ICONST_1
    }
    IRETURN
    LE()
    val GE = new Label
    IF_ICMPGE(GE)
    if (descending) {
      ICONST_1
    } else {
      ICONST_M1
    }
    IRETURN
    GE()
  }

  private[this] final def defineFloatComparison(descending: Boolean) {
    DUP2
    FCMPL
    val LE = new Label
    IFLE(LE)
    if (descending) {
      ICONST_M1
    } else {
      ICONST_1
    }
    IRETURN
    LE()
    FCMPG
    val GE = new Label
    IFGE(GE)
    if (descending) {
      ICONST_1
    } else {
      ICONST_M1
    }
    IRETURN
    GE()
  }

  private[this] final def defineDoubleComparison(descending: Boolean) {
    DSTORE(5)
    DSTORE(3)
    DLOAD(3)
    DLOAD(5)
    DCMPL
    val LE = new Label
    IFLE(LE)
    if (descending) {
      ICONST_1
    } else {
      ICONST_M1
    }
    IRETURN
    LE()
    DLOAD(3)
    DLOAD(5)
    DCMPG
    val GE = new Label
    IFGE(GE)
    if (descending) {
      ICONST_1
    } else {
      ICONST_M1
    }
    IRETURN
    GE()
  }

  private[this] final def defineLongComparison(descending: Boolean) {
    LSTORE(5)
    LSTORE(3)
    LLOAD(3)
    LLOAD(5)
    LCMP
    val LE = new Label
    IFLE(LE)
    if (descending) {
      ICONST_M1
    } else {
      ICONST_1
    }
    IRETURN
    LE()
    LLOAD(3)
    LLOAD(5)
    LCMP
    val GE = new Label
    IFGE(GE)
    if (descending) {
      ICONST_1
    } else {
      ICONST_M1
    }
    IRETURN
    GE()
  }

  private[this] final def defineObjectComparison(descending: Boolean) {
    DUP2
    ASTORE(4)
    ASTORE(3)
    val NE = new Label
    IF_ACMPNE(NE)
    ICONST_0
    IRETURN
    NE()
    ALOAD(3)
    val NONNULL3 = new Label
    IFNONNULL(NONNULL3)
    if (descending) {
      ICONST_1
    } else {
      ICONST_M1
    }
    IRETURN
    NONNULL3()
    ALOAD(4)
    val NONNULL4 = new Label
    IFNONNULL(NONNULL4)
    if (descending) {
      ICONST_M1
    } else {
      ICONST_1
    }
    IRETURN
    NONNULL4()
    ALOAD(3)
    CHECKCAST("java/lang/Comparable")
    ALOAD(4)
    INVOKEINTERFACE("java/lang/Comparable",
                    "compareTo",
                    "(Ljava/lang/Object;)I")
    DUP
    val EQ = new Label
    IFEQ(EQ)
    if (descending) {
      INEG
    }
    IRETURN
    EQ()
    POP
  }
}
