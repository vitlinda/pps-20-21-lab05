package u05lab.code

object Sequences{
  import scala.collection.immutable.List
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    if(a.contains(None)) None else Some(a.foldRight(List[A]())((h, t) =>  h.get :: t))
  }
}
