package grade_horaria

import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}
import collection.generic.CanBuildFrom
import collection.mutable.{Map => MMap}

import grade_horaria._

final class Cromossomo private (
  val genes: Array[Gene],
  val length: Int
) extends IndexedSeq[Gene] with IndexedSeqLike[Gene, Cromossomo]{

  override protected[this] def newBuilder:Builder[Gene,Cromossomo]=
    Cromossomo.newBuilder

  def apply(idx: Int) = genes(idx)

  // ce(t) é a lista dos eventos que ocorrem no horário t
  lazy val eventosSimultâneos:Map[Int,List[Int]] = Map() ++
    { val ce =  MMap[Int,List[Int]]()
      genes.indices foreach { e =>
        val h = genes(e).horário
        val s = genes(e).sala
        if (ce contains h)
          ce(h) = e :: ce(h)
        else
          ce(h) = List(e)
      }
      ce
    }

}

object Cromossomo {

  def fromSeq(genes: Seq[Gene]): Cromossomo = 
    new Cromossomo(genes.toArray,genes.length)

  def apply(genes: Gene*) = fromSeq(genes)  

  def apply(size: Int)(nSalas: Int, nHorários: Int) = {
    val genes =
      Array.fill(size)(Gene(rng.nextInt(nSalas), 
                            rng.nextInt(nHorários)))
    fromSeq(genes)
  }


  def newBuilder: Builder[Gene, Cromossomo] =
    new ArrayBuffer[Gene] mapResult fromSeq

  implicit def canBuildFrom:CanBuildFrom[Cromossomo,Gene,Cromossomo]=
    new CanBuildFrom[Cromossomo, Gene, Cromossomo] {
      def apply(): Builder[Gene, Cromossomo] = newBuilder
      def apply(from: Cromossomo): Builder[Gene, Cromossomo] =
        newBuilder
    }
}
