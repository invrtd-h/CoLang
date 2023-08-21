package mte

private sealed trait Type

private case object NumT extends Type

private case class FnT(args: Vector[Type], ret: Type) extends Type

private case class BoxT(t: Type) extends Type

private case class IdT(id: String) extends Type

private case class TEnv(vars: Map[String, Type], typeIds: Map[String, Type]) extends Type