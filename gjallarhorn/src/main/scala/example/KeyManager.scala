package example

import java.io.File

import scorex.crypto.encode.Base58
import com.typesafe.scalalogging.StrictLogging

import scala.util.{Failure, Success}

case class KeyManager(var secrets: Set[PrivateKey25519], defaultKeyDir: String) extends StrictLogging {

  import KeyManager._
  type S = PrivateKey25519
  type PI = ProofOfKnowledgeProposition[S]

  def publicKeys: Set[PI] = {
    //secrets.map(_.publicImage)
    getListOfFiles(defaultKeyDir).map(file => PublicKey25519Proposition(KeyFile.readFile(file.getPath).pubKeyBytes))
      .toSet
  }

  def unlockKeyFile(publicKeyString: String, password: String): Unit = {
    val keyfiles = getListOfFiles(defaultKeyDir)
      .map(file => KeyFile.readFile(file.getPath))
      .filter(k => k
        .pubKeyBytes sameElements Base58
        .decode(publicKeyString)
        .get)

    assert(keyfiles.size == 1, "Cannot find a unique publicKey in key files")
    val privKey = keyfiles.head.getPrivateKey(password) match {
      case Success(priv) => Set(priv)
      case Failure(e) => throw e
    }
    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.head.privKeyBytes))) {
      // secrets.empty // should empty the current set of secrets meaning unlock only allows a single key to be unlocked
      // at once
      secrets += privKey.head
    } else {
      logger.warn(s"$publicKeyString is already unlocked")
    }
  }

  def lockKeyFile(publicKeyString: String, password: String): Unit = {
    val keyfiles = getListOfFiles(defaultKeyDir)
      .map(file => KeyFile.readFile(file.getPath))
      .filter(k => k
        .pubKeyBytes sameElements Base58
        .decode(publicKeyString)
        .get)
    assert(keyfiles.size == 1, "Cannot find a unique publicKey in key files")
    val privKey = keyfiles.head.getPrivateKey(password) match {
      case Success(priv) => Set(priv)
      case Failure(e) => throw e
    }
    // ensure no duplicate by comparing privKey strings
    if (!secrets.map(p => Base58.encode(p.privKeyBytes)).contains(Base58.encode(privKey.head.privKeyBytes))) {
      logger.warn(s"$publicKeyString is already locked")
    } else {
      secrets -= (secrets find (p => Base58.encode(p.privKeyBytes) == Base58.encode(privKey.head.privKeyBytes))).get
    }
  }
}

object KeyManager{
  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }
}
