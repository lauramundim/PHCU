package ag

class Cronômetro {

  private var início:Long = 0L
  private var duração:Double = 0.0
  private var iniciado = false

  def iniciar() = {
   iniciado = true
   duração = 0.0
   início = System.currentTimeMillis
  }

  def decorrido():Double = {
    val agora = System.currentTimeMillis
    if (iniciado)
      (agora - início).toDouble / 1000.0
    else 0.0
  }

  def parar():Double = {
    val agora = System.currentTimeMillis
    if (iniciado) {
      duração = (agora - início).toDouble / 1000.0
      iniciado = false
    }
    duração
  }

  def duraçãoTotal = duração

  override def toString = {
    val tempo = if (iniciado) decorrido() else duração
    tempo + " s"
  }

}
