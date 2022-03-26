
import scala.language.postfixOps
import scala._;

package object sdf {
  import linalg._

  type Distance = Float
  type Observable = Vec3 => Distance
  type Primitive[T] = T => Observable

  def sphere: Primitive[Vec4] =
    (pr: Vec4) => (o: Vec3) => 
      length(o - pr.xyz) - pr.w
    
  def union: Primitive[(Observable, Observable)] =
    (ab: (Observable, Observable)) => (o: Vec3) => 
      math.min(ab._1(o), ab._2(o))

  def intersect: Primitive[(Observable, Observable)] =
    (ab: (Observable, Observable)) => (o: Vec3) => 
      math.max(ab._1(o), ab._2(o))

  def difference: Primitive[(Observable, Observable)] =
    (ab: (Observable, Observable)) => (o: Vec3) => 
      math.max(-ab._1(o), ab._2(o))        
}
