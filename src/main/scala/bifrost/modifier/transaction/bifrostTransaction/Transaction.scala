package bifrost.modifier.transaction.bifrostTransaction

import bifrost.NodeViewModifier
import bifrost.NodeViewModifier._
import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.Proposition


/**
  * A transaction is an atomic state modifier
  */

abstract class Transaction[P <: Proposition] extends NodeViewModifier {
  override val modifierTypeId: Byte = Transaction.ModifierTypeId

  val fee: Long

  val timestamp: Long

  val messageToSign: Array[Byte]

  override lazy val id: ModifierId = FastCryptographicHash(messageToSign)
}


object Transaction {
  val ModifierTypeId = 2: Byte
  type TransactionId = NodeViewModifier.ModifierId
}