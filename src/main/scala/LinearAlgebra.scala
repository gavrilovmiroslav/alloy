
package object linalg {
  case class Vec3(x: Float, y: Float, z: Float) {
    def length() = math.sqrt(x * x + y * y + z * z).asInstanceOf[Float]
    def +(other: Vec3) = Vec3(x + other.x, y + other.y, z + other.z)
    def *(v: Float) = Vec3(x * v, y * v, z * v)
    def -(other: Vec3) = Vec3(x - other.x, y - other.y, z - other.z)
    def dot(other: Vec3) = x * other.x + y * other.y + z * other.z
    def abs() = Vec3(math.abs(x), math.abs(y), math.abs(z))
    def normalize() = {
      val l = length()
      if (l == 0.0f) { this } else { Vec3(x / l, y / l, z / l) }
    }
  }

  object Vec3 {
    def Zero = Vec3(0, 0, 0)
  }

  def max(a: Vec3, b: Vec3) = Vec3(math.max(a.x, b.x), math.max(a.y, b.y), math.max(a.z, b.z))
  def min(a: Vec3, b: Vec3) = Vec3(math.min(a.x, b.x), math.min(a.y, b.y), math.min(a.z, b.z))  
  def length(a: Vec3) = a.length()
  def abs(a: Vec3) = a.abs()
  def normalize(a: Vec3) = a.normalize()
  def dot(a: Vec3, b: Vec3) = a.dot(b)

  case class Vec4(x: Float, y: Float, z: Float, w: Float) {
    def xyz = Vec3(x, y, z)
    def length() = math.sqrt(x * x + y * y + z * z + w * w)
  }

  object Vec4 {
    def from(xyz: Vec3, r: Float) = Vec4(xyz.x, xyz.y, xyz.z, r)
  }
}