package bifrost.api

import java.time.Instant

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.{AssetApiRoute, NodeViewApiRoute}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import bifrost.block.Block
import bifrost.blocks.BifrostBlock
import bifrost.transaction.bifrostTransaction.{AssetCreation, BifrostTransaction}
import bifrost.transaction.box.ArbitBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

class NodeViewRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val actorSystem = ActorSystem(settings.agentName)
  val nodeViewHolderRef: ActorRef = actorSystem.actorOf(Props(new BifrostNodeViewHolder(settings)))
  nodeViewHolderRef
  val route = NodeViewApiRoute(settings, nodeViewHolderRef).route

  val routeAsset = AssetApiRoute(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/nodeView/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("api_key", "test_key"))
  }

  def httpPOSTAsset(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("api_key", "test_key"))
  }

  implicit val timeout = Timeout(10.seconds)

  private def view() = Await.result((nodeViewHolderRef ? GetCurrentView)
    .mapTo[CurrentView[BifrostHistory, BifrostState, BWallet, BifrostMemPool]], 10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  // Unlock Secrets
  val gw: BWallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  val assetTx = AssetCreation.createAndApply(gw, IndexedSeq((PublicKey25519Proposition(Base58.decode(publicKeys("investor")).get), 10)),
    0L, PublicKey25519Proposition(Base58.decode(publicKeys("hub")).get), "etherAssets", "").get
  val assetTxId = Base58.encode(assetTx.id)

  view().pool.put(assetTx)
  val history = view().history
  //Create a block with the above created createAssets transaction
  val block = BifrostBlock(history.bestBlockId,
    System.currentTimeMillis(),
    ArbitBox(PublicKey25519Proposition(history.bestBlockId), 0L, 10000L),
    Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
    Seq(assetTx),
    10L,
    settings.version
  )
  history.append(block)

  "NodeView RPC" should {
    "Get first 100 transactions in mempool" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "mempool",
           |   "params": [{}]
           |}
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        val txHashesArray = ((res \\ "result").head \\ "txHash")
        txHashesArray.size should be <= 1
        txHashesArray.head.asString.get shouldEqual assetTxId
      }
    }

    "Get transaction from the mempool by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transactionFromMempool",
           |   "params": [{
           |      "transactionId": "${assetTxId}"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txHash").head.asString.get shouldEqual assetTxId

        //Removing the createAssets transaction from the mempool
        view().pool.remove(assetTx)
      }
    }

    "Get a confirmed transaction by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "transactionById",
           |   "params": [{
           |      "transactionId": "${assetTxId}"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txHash").head.asString.get shouldEqual assetTxId
      }
    }

    "Get block by id" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |
           |   "id": "1",
           |   "method": "blockById",
           |   "params": [{
           |      "blockId": "${Base58.encode(block.id)}"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        val txsArray = ((res \\ "result").head \\ "txs").head.asArray.get
        //Checking that the block found contains the above createAssets transaction
        //since that block's id was used as the search parameter
        txsArray.map(tx => (tx \\ "txHash").head.asString.get) should contain (assetTxId)
      }
    }
  }
}


object NodeViewRPCSpec {
  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())
}
