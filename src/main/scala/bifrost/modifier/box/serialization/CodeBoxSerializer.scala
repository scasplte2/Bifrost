package bifrost.modifier.box.serialization

import bifrost.modifier.box.{CodeBox, ProgramBox}
import bifrost.utils.Extensions._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

import scala.util.Try
import com.google.common.primitives.{Ints, Longs}
import java.util.UUID
import bifrost.modifier.box.proposition.{PublicKey25519Proposition, Constants25519}

object CodeBoxSerializer extends BifrostSerializer[CodeBox] {

  override def serialize(obj: CodeBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* code: Seq[String], List of strings of JS functions */
    w.putUInt(obj.code.length)
    obj.code.foreach(js => w.putIntString(js))

    /* interface: Map[String, Seq[String]] */
    w.putUInt(obj.interface.size)
    obj.interface.foreach { case (methodName, params) =>
      w.putIntString(methodName)
      w.putUInt(params.length)
      params.foreach(p => w.putIntString(p))
    }
  }

  override def parse(r: Reader): CodeBox = {
    val programBox: ProgramBox = ProgramBoxSerializer.parse(r)

    /* code: Seq[String], List of strings of JS functions */
    val codeLength: Int = r.getUInt().toIntExact
    val code: Seq[String] = (0 until codeLength).map(_ => r.getIntString())

    /* interface: Map[String, Seq[String]] */
    val interfaceSize: Int = r.getUInt().toIntExact

    val interface: Map[String, Seq[String]] = (0 until interfaceSize).map { _ =>
      val methodName: String = r.getIntString()
      val paramsLength: Int = r.getUInt().toIntExact
      val params: Seq[String] = (0 until paramsLength).map(_ => r.getIntString())
      methodName -> params
    }.toMap

    CodeBox(programBox.proposition, programBox.nonce, programBox.value, code, interface)
  }


  //TODO: Jing - remove
  def decode(obj: Array[Byte]): Try[CodeBox] = Try {
    var takenBytes = 0

    val boxTypeLength = Ints.fromByteArray(obj.take(Ints.BYTES))
    takenBytes += Ints.BYTES

    val boxType = new String(obj.slice(takenBytes, takenBytes + boxTypeLength))
    takenBytes += boxTypeLength

    require(boxType == "CodeBox")

    val nonce = Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES))
    takenBytes += Longs.BYTES

    val uuid = new UUID(Longs.fromByteArray(obj.slice(takenBytes, takenBytes + Longs.BYTES)),
      Longs.fromByteArray(obj.slice(takenBytes + Longs.BYTES, takenBytes + 2 * Longs.BYTES)))
    takenBytes += 2 * Longs.BYTES

    val codeLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    var code = Seq[String]()
    for (_ <- 1 to codeLength) {
      val l = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES
      code = code :+ new String(obj.slice(takenBytes, takenBytes + l))
      takenBytes += l
    }

    val interfaceLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
    takenBytes += Ints.BYTES

    val interface: Map[String, Seq[String]] = (0 until interfaceLength).map{ _ =>

      val methodNameLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES

      val methodName = new String(obj.slice(takenBytes, takenBytes + methodNameLength))
      takenBytes += methodNameLength

      val paramsLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
      takenBytes += Ints.BYTES

      val params: Seq[String] = (0 until paramsLength).map { _ =>
        val strLength = Ints.fromByteArray(obj.slice(takenBytes, takenBytes + Ints.BYTES))
        takenBytes += Ints.BYTES

        val str = new String(obj.slice(takenBytes, takenBytes + strLength))
        takenBytes += strLength
        str
      }
      methodName -> params
    }.toMap

    val prop = PublicKey25519Proposition(obj.slice(takenBytes, takenBytes + Constants25519.PubKeyLength))
    takenBytes += Constants25519.PubKeyLength

    CodeBox(prop, nonce, uuid, code, interface)
  }
}
