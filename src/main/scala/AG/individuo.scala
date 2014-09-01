package ag

abstract class Indivíduo[Cromossomo, Ind <: Indivíduo[Cromossomo,Ind]](
    val fapt:Aptidão[Cromossomo]
) extends Ordered[Ind] {

  val apt: Int

  //val penalidades: Array[Int]
  //val vInvioláveis: Array[Int]
  //val vPreferenciais: Array[Int]

  val cromossomo: Cromossomo
  def replica(c:Cromossomo): Ind

  def compare(outro: Ind):Int =
    if (this.apt >= outro.apt)
       1
    else
      -1
  
  override def toString = cromossomo.toString + " aptidão " + apt
}
