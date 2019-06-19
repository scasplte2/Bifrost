package bifrost.program

import java.time.Instant
import java.util.UUID

import bifrost.transaction.bifrostTransaction.AssetCreation
import bifrost.transaction.box.{CodeBox, StateBox}
import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import io.circe.syntax._
import org.graalvm.polyglot.management.ExecutionListener
import org.graalvm.polyglot.{Context, Value}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}

class ProgramMethodSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Can call a function from a program") {
    forAll(programGen) {
      c: Program => {
        val program = c.executionBuilderObj.core.code.foldLeft("")((a,b) => a ++ (b + "\n"))
        val party = propositionGen.sample.get
        /*val params = JsonObject.fromMap(
          Map("newStatus" -> stringGen.sample.get.asJson))
         */
        val params = JsonObject.empty

        val state = c.executionBuilderObj.core.variables
        println(s"state: ${state.toString}")

        val stateTwo = s"""{ "b": 0 }""".asJson
        val stateThree = s"""{ "c": 0 }""".asJson

        val stateBox = StateBox(c.parties.head._1, 0L, state, true)
        val stateBoxTwo = StateBox(c.parties.head._1, 1L, stateTwo, true)
        val stateBoxThree = StateBox(c.parties.head._1, 2L, stateThree, true)
        val codeBox = CodeBox(c.parties.head._1, 3L, Seq("add = function() { a += 1 }"))

        val stateBoxUuids = Seq(
          (stateBox, UUID.nameUUIDFromBytes(stateBox.id)),
          (stateBoxTwo, UUID.nameUUIDFromBytes(stateBoxTwo.id)),
          (stateBoxThree, UUID.nameUUIDFromBytes(stateBoxThree.id))
        )

        val result = Program.execute(stateBoxUuids, Seq(codeBox), "add")(party)(params)
        println(s"test result: $result")
      }
    }
  }

  property("Calling getStateFrom correctly reads from a read only StateBox") {
    forAll(programGen) {
      c: Program => {
        val program = c.executionBuilderObj.core.code.foldLeft("")((a,b) => a ++ (b + "\n"))
        val party = propositionGen.sample.get


        val state = c.executionBuilderObj.core.variables
        println(s"state: ${state.toString}")

        val stateTwo = s"""{ "b": 1 }""".asJson
        val stateThree = s"""{ "c": 2 }""".asJson

        val stateBox = StateBox(c.parties.head._1, 0L, state, true)
        val stateBoxTwo = StateBox(c.parties.head._1, 1L, stateTwo, true)
        val stateBoxThree = StateBox(c.parties.head._1, 2L, stateThree, true)
        val codeBox = CodeBox(c.parties.head._1, 3L, Seq("updateState = function(uuid, value) { a = getStateFrom(uuid, value) }"))

        val stateBoxUuids = Seq(
          (stateBox, UUID.nameUUIDFromBytes(stateBox.id)),
          (stateBoxTwo, UUID.nameUUIDFromBytes(stateBoxTwo.id)),
          (stateBoxThree, UUID.nameUUIDFromBytes(stateBoxThree.id))
        )

        val params = JsonObject.fromMap(Map(
            "uuid" -> stateBoxUuids.drop(1).head._2.toString.replace("-","_").asJson,
          ))

        val result = Program.execute(stateBoxUuids, Seq(codeBox), "add")(party)(params)
        println(s"test result: $result")
      }
    }
  }

  property("Mutating state in a read only StateBox does not persist after execution context closes") {
    forAll(programGen) {
      c: Program => {
        val program = c.executionBuilderObj.core.code.foldLeft("")((a,b) => a ++ (b + "\n"))
        val party = propositionGen.sample.get
        /*val params = JsonObject.fromMap(
          Map("newStatus" -> stringGen.sample.get.asJson))
         */
        val params = JsonObject.empty

        val state = c.executionBuilderObj.core.variables
        println(s"state: ${state.toString}")

        val stateTwo = s"""{ "b": 0 }""".asJson
        val stateThree = s"""{ "c": 0 }""".asJson

        val stateBox = StateBox(c.parties.head._1, 0L, state, true)
        val stateBoxTwo = StateBox(c.parties.head._1, 1L, stateTwo, true)
        val stateBoxThree = StateBox(c.parties.head._1, 2L, stateThree, true)
        val codeBox = CodeBox(c.parties.head._1, 3L, Seq("add = function() { a += 1 }"))

        val stateBoxUuids = Seq(
          (stateBox, UUID.nameUUIDFromBytes(stateBox.id)),
          (stateBoxTwo, UUID.nameUUIDFromBytes(stateBoxTwo.id)),
          (stateBoxThree, UUID.nameUUIDFromBytes(stateBoxThree.id))
        )

        val result = Program.execute(stateBoxUuids, Seq(codeBox), "add")(party)(params)
        println(s"test result: $result")
      }
    }
  }

  /*property("Can call createAssets protocol level function from a program") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "issuer" -> stringGen.sample.get.asJson,
            "to" -> stringGen.sample.get.asJson,
            "assetCode" -> stringGen.sample.get.asJson,
            "amount" -> positiveTinyIntGen.sample.get.asJson))

        println(s"createAssets params: ${params}")

        val result = Program.execute(c, "createAssets")(party)(params)
        println(s"test result: $result")
        assert(result.isSuccess)
      }
    }
  }*/

  /*property("Can call transferAssets protocol level function from a program") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "amount" -> positiveTinyIntGen.sample.get.asJson,
            "recipient" -> stringGen.sample.get.asJson,
            "issuer" -> stringGen.sample.get.asJson,
            "asset" -> stringGen.sample.get.asJson,
            "data" -> stringGen.sample.get.asJson))

        val result = Program.execute(c, "transferAssets")(party)(params)
        println(result)
        assert(result.isSuccess)
      }
    }
  }*/

  /*property("Can call polyTransfer protocol level function from a program") {
    forAll(programGen) {
      c: Program => {
        val party = propositionGen.sample.get
        val params = JsonObject.fromMap(
          Map(
            "publicKey" -> stringGen.sample.get.asJson,
            "amount" -> positiveTinyIntGen.sample.get.asJson))

        val result = Program.execute(c, "transferPolys")(party)(params)
        println(result)
        assert(result.isSuccess)
      }
    }
  }*/

    /*val name: String = "assetCreation"
    val assetCode: String = "Wheat"
    val effectiveTimestamp = Instant.now.toEpochMilli
    val expirationTimestamp = Instant.now.toEpochMilli + 10000L
    val program: String =
      s
         |this.$name = function(){
         |    this.programEffectiveTime = $effectiveTimestamp;
         |    this.programExpirationTime = $expirationTimestamp;
         |    this.status = "initialized"
         |    this.assetCode = "$assetCode"
         |    this.initialCapital = "0";
         |
         |    this.changeStatus = function(newStatus) {
         |      this.status = newStatus;
         |      return this;
         |    }
         |
         |    this.newAsset = function(asset, amount) {
         |      createAsset(asset, amount);
         |      return this;
         |    }
         |}
         |
         |this.$name.fromJSON = function(str) {
         |    return new $name();
         |}
         |
         |this.$name.toJSON = function(o) {
         |    return JSON.stringify(o);
         |}
     """.stripMargin

    var state = "{}".asJson

    val polyglot: Context = Context.create()

    val protocolStateListener: Value = polyglot.eval("js",
      s"""
         |function protocolStateListener() {
         |
       """.stripMargin)

    val createAsset: Value = polyglot.eval("js",
      s"""
         |async function createAsset() {
         |    await
       """.stripMargin)



    val party = propositionGen.sample.get
    val params = JsonObject.fromMap(Map("asset" -> stringGen.sample.get.asJson, "amount" -> positiveTinyIntGen.sample.get.asJson))

    val result = Program.execute(program, "createAsset")(party)(params)
    println(s">>>>>>>> Result: $result")*/
}