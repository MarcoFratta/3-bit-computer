import ThreeBitsState.Registry.*

trait RegistryOps[S, K, V]:
  extension (state: S)
    def read(k: K): Option[V]
    def write(updates:(K, V)*): S
trait IpOps[S]:
  extension (state: S)
    def ip: Int
    def writeIp(v:Int):S

object RegistryOps:
 extension [S](s: S)
   def apply[K, V](k: K)(using ops: RegistryOps[S, K, V]): V = s.read(k).get

object ThreeBitsState:
  enum Registry:
    case IP, X, Y, Z
  type ValueType = Int
  case class State(registry: Map[Registry, ValueType])
  def state(ip:ValueType, x:ValueType, y: ValueType, z:ValueType): State = State(Seq(X-> x, Y-> y, Z->z, IP->ip).toMap)
  given RegistryOps[State, Registry, ValueType] with
    extension(s: State)
      override def read(k:Registry): Option[ValueType] = s.registry.get(k)
      override def write(updates:(Registry, ValueType)*): State = s.copy(registry = s.registry ++ updates)

  given IpOps[State] with
    extension (s:State)
      override def writeIp(v: Int): State = s.copy(registry = s.registry + (IP -> v))
      override def ip: Int = s.registry(IP).intValue




