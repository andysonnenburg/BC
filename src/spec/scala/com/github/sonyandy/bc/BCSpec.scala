package com.github.sonyandy.bc

import java.lang.reflect.Modifier._
import java.util.Comparator

import org.specs._

object BCSpec extends Specification {
  "BC" should {
    "create Test class" in {
      object Test extends BC[Comparator[AnyRef]] {
        
        protected[this] def defineClass() {
          
          public.`final`.`class`("Test").implements("java/util/Comparator") {
            
            public.void("<init>")() {
              ALOAD(0)
              INVOKESPECIAL("java/lang/Object", "<init>", "()V")
              RETURN
            }

            public.`final`.int("compare")("Ljava/lang/Object;", "Ljava/lang/Object;") {
              ALOAD(1)
              ALOAD(2)
              val L1 = new Label
              IF_ACMPNE(L1)
              ICONST_0
              IRETURN
              L1()
              ALOAD(1)
              val L2 = new Label
              IFNONNULL(L2)
              ICONST_M1
              IRETURN
              L2()
              ALOAD(2)
              val L3 = new Label
              IFNONNULL(L3)
              ICONST_1
              IRETURN
              L3()
              ICONST_0
              IRETURN
            }
          }
        }
        
      }

      val `class`: Class[_ <: Comparator[AnyRef]] = Test
      isPublic(`class`.getModifiers) must beTrue
      isFinal(`class`.getModifiers) must beTrue
      `class`.getName must_== "Test"
      val c = `class`.getDeclaredConstructor()
      isPublic(c.getModifiers) must beTrue
      val m = `class`.getDeclaredMethod("compare", classOf[Object], classOf[Object])
      isPublic(m.getModifiers) must beTrue
      isFinal(m.getModifiers) must beTrue
      isStatic(m.getModifiers) must beFalse
      (m.getReturnType eq java.lang.Integer.TYPE) must beTrue
      val o = `class`.newInstance
      o.compare(null, null) must_== 0
      o.compare(new Object, null) must beGreaterThan(0)
      o.compare(null, new Object) must beLessThan(0)
    }

    "create a new class using the BCClassLoader" in {
      val classLoader = new BCClassLoader
      object BCClassLoaderTest extends BC[AnyRef] {
        protected[this] final def defineClass {
          
          public.`final`.`class`("BCClassLoaderTest") {
            
            public.void("<init>")() {
              ALOAD(0)
              INVOKESPECIAL("java/lang/Object", "<init>", "()V")
              RETURN
            }
            
          }
        }
      }
      val `class`: Class[_] = classLoader.loadClass(BCClassLoaderTest)
      `class`.getClassLoader mustBe classLoader
      `class`.getName must beEqual("BCClassLoaderTest")
    }
  }
}
