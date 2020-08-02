package bifrost.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.BifrostGenerators
import bifrost.crypto.Signature25519
import bifrost.history.History
import bifrost.http.api.routes.{AssetApiRoute, NodeViewApiRoute}
import bifrost.mempool.MemPool
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.modifier.box.ArbitBox
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.GetDataFromCurrentView
import bifrost.nodeView.{CurrentView, NodeViewHolderRef}
import bifrost.settings.BifrostContext
import bifrost.state.State
import bifrost.utils.NetworkTimeProvider
import bifrost.wallet.Wallet
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.reflect.io.Path
import scala.util.Try

class NodeViewRPCSpec extends AnyWordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */
  // save environment into a variable for reference throughout the application
  private val bifrostContext: BifrostContext = BifrostContext(
    messageSpecs = Map(),
    features = Seq(),
    upnpGateway = None,
    timeProvider = new NetworkTimeProvider(settings.ntp),
    externalNodeAddress = None
  )

  // Create Bifrost singleton actors
  private val nodeViewHolderRef: ActorRef = NodeViewHolderRef("nodeViewHolder", settings, bifrostContext)
  /* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- *//* ----------------- */

  // setup route for testing
  val route: Route = NodeViewApiRoute(settings, nodeViewHolderRef).route

  val routeAsset: Route = AssetApiRoute(settings, nodeViewHolderRef).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/nodeView/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  def httpPOSTAsset(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/asset/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  implicit val timeout: Timeout = Timeout(10.seconds)

  private def actOnCurrentView(v: CurrentView[History, State, Wallet, MemPool]): CurrentView[History, State, Wallet, MemPool] = v

  private def view() = Await.result(
    (nodeViewHolderRef ? GetDataFromCurrentView(actOnCurrentView)).mapTo[CurrentView[History, State, Wallet, MemPool]],
    10.seconds)

  val publicKeys = Map(
    "investor" -> "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ",
    "producer" -> "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb",
    "hub" -> "F6ABtYMsJABDLH2aj7XVPwQr5mH7ycsCE4QGQrLeB3xU"
  )

  // Unlock Secrets
  val gw: Wallet = view().vault
  gw.unlockKeyFile(publicKeys("investor"), "genesis")
  gw.unlockKeyFile(publicKeys("producer"), "genesis")
  gw.unlockKeyFile(publicKeys("hub"), "genesis")

  var txHash: String = ""
  var assetTxHash: String = ""
  var assetTxInstance: Transaction = _
  var blockId: Block.BlockId = ModifierId(Array[Byte]())

  val requestBody: ByteString = ByteString(
    s"""
       |{
       |   "jsonrpc": "2.0",
       |   "id": "1",
       |   "method": "createAssets",
       |   "params": [{
       |     "issuer": "${publicKeys("hub")}",
       |     "recipient": "${publicKeys("investor")}",
       |     "amount": 10,
       |     "assetCode": "x",
       |     "fee": 0,
       |     "data": ""
       |   }]
       |}
        """.stripMargin)

  httpPOSTAsset(requestBody) ~> routeAsset ~> check {
    val res = parse(responseAs[String]).right.get
    (res \\ "error").isEmpty shouldBe true
    (res \\ "result").head.asObject.isDefined shouldBe true
    assetTxHash = ((res \\ "result").head \\ "txHash").head.asString.get
  }

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
        val txHashesArray = (res \\ "result").head \\ "txHash"
        txHashesArray.find(tx => tx.asString.get == assetTxHash) match {
          case Some (tx) =>
            txHash = tx.asString.get
          case None =>
        }
        txHash shouldEqual assetTxHash
        assert(txHashesArray.size <= 100)
        val txHashId = ModifierId(Base58.decode(txHash).get)
        assetTxInstance = view().pool.getById(txHashId).get
        val history = view().history
        //Create a block with the above created createAssets transaction
        val tempBlock = Block(history.bestBlockId,
          System.currentTimeMillis(),
          ArbitBox(PublicKey25519Proposition(history.bestBlockId.hashBytes), 0L, 10000L),
          Signature25519(Array.fill(Curve25519.SignatureLength)(1: Byte)),
          Seq(assetTxInstance),
          10L,
          settings.forgingSettings.version
        )
        history.append(tempBlock)
        blockId = tempBlock.id
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
           |      "transactionId": "$txHash"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txHash").head.asString.get shouldEqual txHash

        //Removing the createAssets transaction from the mempool
        view().pool.remove(assetTxInstance)
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
           |      "transactionId": "$txHash"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        ((res \\ "result").head \\ "txHash").head.asString.get shouldEqual txHash
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
           |      "blockId": "$blockId"
           |   }]
           |}
           |
          """.stripMargin)

      httpPOST(requestBody) ~> route ~> check {
        val res = parse(responseAs[String]).right.get
        (res \\ "error").isEmpty shouldBe true
        (res \\ "result").isInstanceOf[List[Json]] shouldBe true
        val txsArray = ((res \\ "result").head \\ "txs").head.asArray.get
        txsArray.filter(tx => {(tx \\"txHash").head.asString.get == txHash})
        //Checking that the block found contains the above createAssets transaction
        //since that block's id was used as the search parameter
        txsArray.size shouldEqual 1
      }
    }
  }
}


object NodeViewRPCSpec {
  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())
}
