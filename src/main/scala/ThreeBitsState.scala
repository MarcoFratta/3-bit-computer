import ThreeBitsState.Registry.*


object ThreeBitsState:
  type ValueType = Int

  def state(ip: ValueType, x: ValueType, y: ValueType, z: ValueType): State = 
    State(Seq(X -> x, Y -> y, Z -> z, IP -> ip).toMap)

  enum Registry:
    case IP, X, Y, Z

  case class State(registry: Map[Registry, ValueType])

  given RegistryOps[State, Registry, ValueType] with
    extension (s: State)
      override def read(k: Registry): Option[ValueType] = s.registry.get(k)
      override def write(updates: (Registry, ValueType)*): State = s.copy(registry = s.registry ++ updates)

  given IpOps[State] with
    extension (s: State)
      override def writeIp(v: Int): State = s.copy(registry = s.registry + (IP -> v))
      override def ip: Int = s.registry(IP).intValue




