package parsing {
    import scala.util.parsing.combinator._
    import linalg._
    import sdf._

    trait Geometry {
        def emit(): Observable
    }

    trait Primitive extends Geometry
    case class Sphere(xyz: Vec3, r: Float) extends Primitive {
      override def emit(): Observable = sphere(Vec4.from(xyz, r))
    }

    trait BoolOp extends Geometry
    case class Union(a: Geometry, b: Geometry) extends BoolOp {
      override def emit(): Observable = union(a.emit(), b.emit())
    }

    case class Intersect(a: Geometry, b: Geometry) extends BoolOp {
      override def emit(): Observable = intersect(a.emit(), b.emit())
    }

    case class Difference(a: Geometry, b: Geometry) extends BoolOp {
      override def emit(): Observable = difference(a.emit(), b.emit())
    }

    object SceneParser extends RegexParsers {
        def num: Parser[Float] =
            """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r ^^ { _.toFloat }
        
        def vec3 = "(" ~> num ~ "," ~ num ~ "," ~ num <~ ")" ^^ { case a ~ _ ~ b ~ _ ~ c => Vec3(a, b, c) }
        def vec4 = "(" ~> num ~ "," ~ num ~ "," ~ num ~ "," ~ num <~ ")" ^^ { case a ~ _ ~ b ~ _ ~ c ~ _ ~ d => Vec4(a, b, c, d) }
        
        def primitive = sphere
        def sphere = "sphere" ~> vec4 ^^ { case v => Sphere(v.xyz, v.w) }

        def bool_op: Parser[BoolOp] = 
            union | intersect | diff

        def union: Parser[Union] = 
            ("union" ~ "(") ~> geo ~ "," ~ geo <~ ")" ^^ { case a ~ _ ~ b => Union(a, b) }

        def intersect: Parser[Intersect] = 
            ("intersect" ~ "(") ~> geo ~ "," ~ geo <~ ")" ^^ { case a ~ _ ~ b => Intersect(a, b) }

        def diff: Parser[Difference] = 
            ("difference" ~ "(") ~> geo ~ "," ~ geo <~ ")" ^^ { case a ~ _ ~ b => Difference(a, b) }

        def geo = primitive | bool_op
    
        def scene = rep(geo) ^^ { _.toList }

        def apply(str: String): Either[List[Geometry], String] = {
            this.parse(scene, str) match {
                case Success(result, _) => Left(result)
                case Failure(msg, _) => Right(msg)
                case Error(msg, _) => Right(msg)
            }
        }
    }
}