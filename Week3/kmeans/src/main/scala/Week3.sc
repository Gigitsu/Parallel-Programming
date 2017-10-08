object KM extends kmeans.KMeans
import KM._
import common._
import kmeans.Point

import scala.collection.{GenMap, GenSeq, IndexedSeq, mutable}

def sum(xs: Array[Int]): Int = {
  xs.par.foldLeft(0)(_ + _)
}


def max(xs: Array[Int]): Int = {
  xs.par.fold(xs(0))(
    (x, y) => scala.math.max(x, y)
  )
}

max(Array(1, 2, 3, 4, 7, 8, 5, 6))

trait Iterator[T] {
  def hasNext: Boolean

  def next(): T

  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z

    while (hasNext) {
      result = f(result, next())
    }

    result
  }
}

//trait Splitter[T] {
//  def split: Seq[Splitter[T]]
//
//  def remaining: Int
//
//  def fold(z: T)(f: (T, T) => T): T = {
//    if (remaining < threshold) foldLeft(z)(f)
//    else {
//      val children: Seq[Task[T]] = split.map(child => task {
//        child.fold(z)(f)
//      })
//      children.map(_.join()).foldLeft(z)(f)
//    }
//  }
//}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: mutable.Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach((x) => if(p(x)) b += x)
    b.result
  }
}

val points = GenSeq(new kmeans.Point(0, 0, 1), new kmeans.Point(0,0, -1), new kmeans.Point(0,1,0), new kmeans.Point(0,10,0))
val means = GenSeq(new kmeans.Point(0, -1, 0), new kmeans.Point(0, 2, 0))
val eta = 12.25

KM.kMeans(points, means, eta)
