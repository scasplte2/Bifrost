package co.topl.nodeView.state.box

import co.topl.crypto.FastCryptographicHash
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs

trait PublicKeyNoncedBox[PKP <: PublicKey25519Proposition] extends GenericBox[PKP, Long] {
  val nonce: Long

  lazy val id: BoxId = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey = proposition

  override def equals(obj: Any): Boolean = obj match {
    case acc: PublicKeyNoncedBox[PKP] => (acc.id == this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object PublicKeyNoncedBox {

  def idFromBox[PKP <: PublicKey25519Proposition](prop: PKP, nonce: Long): BoxId = {
    val hashBytes = FastCryptographicHash(prop.pubKeyBytes ++ Longs.toByteArray(nonce))
    BoxId(hashBytes)
  }
}