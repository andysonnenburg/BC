package com.github.sonyandy.bc.util

import scala.reflect.BeanProperty
  
import org.specs._
  
object BeanComparatorClassSpec extends Specification {
  "BeanComparatorClass" should {

    final class Bean {
      @BeanProperty
      var byte: Byte = 0
      @BeanProperty
      var string: String = null
    }

    "create a NOOP Comparator class for class Bean" in {
      val beanClass = classOf[Bean]
      val fields = new Array[BeanComparatorClass.Field](0)
      val beanComparatorClass = new BeanComparatorClass(beanClass, fields)
      val beanComparator = beanComparatorClass.newInstance
      beanComparator.compare(new Bean, new Bean) must_== 0
      beanComparator.compare(new Bean, null) must beGreaterThan(0)
      beanComparator.compare(null, new Bean) must beLessThan(0)
    }
  
    "create a Comparator class on the byte property of Bean" in {
      val beanClass = classOf[Bean]
      val fields = Array(new BeanComparatorClass.Field("byte"))
      val beanComparatorClass = new BeanComparatorClass[Bean](beanClass, fields)
      val beanComparator = beanComparatorClass.newInstance
      val x = new Bean
      val y = new Bean
      beanComparator.compare(x, y) must_== 0
      beanComparator.compare(y, x) must_== 0
      x.byte = 1
      beanComparator.compare(x, y) must beGreaterThan(0)
      beanComparator.compare(y, x) must beLessThan(0)
      x.byte = -1
      beanComparator.compare(x, y) must beLessThan(0)
      beanComparator.compare(y, x) must beGreaterThan(0)
      x.byte = Byte.MaxValue
      y.byte = Byte.MinValue
      beanComparator.compare(x, y) must beGreaterThan(0)
      beanComparator.compare(y, x) must beLessThan(0)
    }

    "create a Comparator class on the string property of Bean" in {
      val beanClass = classOf[Bean]
      val fields = Array(new BeanComparatorClass.Field("string"))
      val beanComparatorClass = new BeanComparatorClass[Bean](beanClass, fields)
      val beanComparator = beanComparatorClass.newInstance
      val x = new Bean
      val y = new Bean
      beanComparator.compare(x, y) must_== 0
      beanComparator.compare(y, x) must_== 0
      x.string = "1"
      beanComparator.compare(x, y) must beGreaterThan(0)
      beanComparator.compare(y, x) must beLessThan(0)
      y.string = "2"
      beanComparator.compare(x, y) must beLessThan(0)
      beanComparator.compare(y, x) must beGreaterThan(0)
      x.string = Byte.MaxValue.toString
      y.string = Byte.MinValue.toString
      beanComparator.compare(x, y) must beGreaterThan(0)
      beanComparator.compare(y, x) must beLessThan(0)
    }

    "create a descending Comparator class on the string property of Bean" in {
      val beanClass = classOf[Bean]
      val fields = Array(new BeanComparatorClass.Field("string", true))
      val beanComparatorClass = new BeanComparatorClass[Bean](beanClass, fields)
      val beanComparator = beanComparatorClass.newInstance
      val x = new Bean
      val y = new Bean
      beanComparator.compare(x, y) must_== 0
      beanComparator.compare(y, x) must_== 0
      x.string = "1"
      beanComparator.compare(x, y) must beLessThan(0)
      beanComparator.compare(y, x) must beGreaterThan(0)
      y.string = "2"
      beanComparator.compare(x, y) must beGreaterThan(0)
      beanComparator.compare(y, x) must beLessThan(0)
      x.string = Byte.MaxValue.toString
      y.string = Byte.MinValue.toString
      beanComparator.compare(x, y) must beLessThan(0)
      beanComparator.compare(y, x) must beGreaterThan(0)
    }
  }
}
