package com.github.sonyandy.bc

import java.lang.reflect.Modifier._
import java.util.Comparator

import org.specs._

object BCSpec extends Specification {
  "BC" should {
    "create Simple1 class" in {
      object Simple1 extends BC[AnyRef] {
        
        protected[this] def defineClass() {
          
          public.`class`("Simple1") {
            
            public.void("<init>")() {
              ALOAD(0)
              INVOKESPECIAL("java/lang/Object", "<init>", "()V")
              RETURN
            }
          }
        }
        
      }

      val `class`: Class[_ <: AnyRef] = Simple1
      isPublic(`class`.getModifiers) must beTrue
      isFinal(`class`.getModifiers) must beFalse
      `class`.getName must_== "Simple1"
      val constructor = `class`.getDeclaredConstructor()
      isPublic(constructor.getModifiers) must beTrue
      `class`.newInstance()
    }

    "create Simple2 class with basic methods" in {
      object Simple2 extends BC[AnyRef] {

        protected[this] def defineClass() {

          public.`class`("Simple2") {

            public.void("<init>")() {
              ALOAD(0)
              INVOKESPECIAL("java/lang/Object", "<init>", "()V")
              RETURN
            }

            public.boolean("equals")("Ljava/lang/Object;") {
              ALOAD(1)
              INSTANCEOF("Simple2")
              val ISINSTANCEOF = new Label
              IFNE(ISINSTANCEOF)
              ICONST_0
              IRETURN
              ISINSTANCEOF()
              ICONST_1
              IRETURN
            }
          }
        }
      }

      val `class`: Class[_ <: AnyRef] = Simple2
      isPublic(`class`.getModifiers) must beTrue
      isFinal(`class`.getModifiers) must beFalse
      `class`.getName must_== "Simple2"
      val constructor = `class`.getDeclaredConstructor()
      isPublic(constructor.getModifiers) must beTrue
      val method = `class`.getMethod("equals", classOf[Object])
      isPublic(method.getModifiers) must beTrue
      isFinal(method.getModifiers) must beFalse
      val o = `class`.newInstance()
      o must be_==(o)
      o must not(be_==(new AnyRef))
      o must be_==(`class`.newInstance())
    }

    "create Simple3 class with final methods" in {
      object Simple3 extends BC[AnyRef] {

        protected[this] def defineClass() {

          public.`class`("Simple3") {

            public.void("<init>")() {
              ALOAD(0)
              INVOKESPECIAL("java/lang/Object", "<init>", "()V")
              RETURN
            }

            public.`final`.boolean("equals")("Ljava/lang/Object;") {
              ALOAD(1)
              INSTANCEOF("Simple3")
              val ISINSTANCEOF = new Label
              IFNE(ISINSTANCEOF)
              ICONST_0
              IRETURN
              ISINSTANCEOF()
              ICONST_1
              IRETURN
            }
          }
        }
      }

      val `class`: Class[_ <: AnyRef] = Simple3
      isPublic(`class`.getModifiers) must beTrue
      isFinal(`class`.getModifiers) must beFalse
      `class`.getName must_== "Simple3"
      val constructor = `class`.getDeclaredConstructor()
      isPublic(constructor.getModifiers) must beTrue
      val method = `class`.getMethod("equals", classOf[Object])
      isPublic(method.getModifiers) must beTrue
      isFinal(method.getModifiers) must beTrue
      val o = `class`.newInstance()
      o must be_==(o)
      o must not(be_==(new AnyRef))
      o must be_==(`class`.newInstance())
    }

    "create final Simple4 class with non-final methods" in {
      object Simple4 extends BC[AnyRef] {

        protected[this] def defineClass() {

          public.`final`.`class`("Simple4") {

            public.void("<init>")() {
              ALOAD(0)
              INVOKESPECIAL("java/lang/Object", "<init>", "()V")
              RETURN
            }

            public.boolean("equals")("Ljava/lang/Object;") {
              ALOAD(1)
              INSTANCEOF("Simple4")
              val ISINSTANCEOF = new Label
              IFNE(ISINSTANCEOF)
              ICONST_0
              IRETURN
              ISINSTANCEOF()
              ICONST_1
              IRETURN
            }
          }
        }
      }

      val `class`: Class[_ <: AnyRef] = Simple4
      isPublic(`class`.getModifiers) must beTrue
      isFinal(`class`.getModifiers) must beTrue
      `class`.getName must_== "Simple4"
      val constructor = `class`.getDeclaredConstructor()
      isPublic(constructor.getModifiers) must beTrue
      val method = `class`.getMethod("equals", classOf[Object])
      isPublic(method.getModifiers) must beTrue
      isFinal(method.getModifiers) must beFalse
      val o = `class`.newInstance()
      o must be_==(o)
      o must not(be_==(new AnyRef))
      o must be_==(`class`.newInstance())
    }
  
    "create a new class using the BCClassLoader" in {
      val classLoader = new BCClassLoader
      object BCClassLoaderTest extends BC[AnyRef] {
        protected[this] final def defineClass {
          
          public.`class`("BCClassLoaderTest") {
            
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
      isPublic(`class`.getModifiers) must beTrue
      isFinal(`class`.getModifiers) must beFalse
      val constructor = `class`.getDeclaredConstructor()
      isPublic(constructor.getModifiers) must beTrue
      `class`.newInstance()
    }
  }
}
