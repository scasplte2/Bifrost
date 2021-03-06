package co.topl.transaction

/**
  * Created by cykoz on 5/11/2017.
  */
import co.topl.crypto.Signature25519
import co.topl.modifier.transaction.ProgramCreation
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }

import scala.util.Success
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scorex.crypto.signatures.Signature

import scala.util.Success

class ProgramCreationSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  property("Generated ProgramCreation Tx should be valid") {
    forAll(validProgramCreationGen) {
      programCreation: ProgramCreation =>
        val semanticValid = State.syntacticValidity(programCreation)
        semanticValid shouldBe a[Success[_]]
    }
  }

  property("Tx with modified signature should be invalid") {
    forAll(validProgramCreationGen) {
      programCreation: ProgramCreation =>
        val wrongSig: Array[Byte] =
          (programCreation.signatures.head._2.bytes.head + 1).toByte +:
            programCreation.signatures.head._2.bytes.tail

        val wrongSigs: Map[PublicKey25519Proposition, Signature25519] =
          programCreation.signatures +
            (programCreation.signatures.head._1 -> Signature25519(Signature @@ wrongSig))

        State.syntacticValidity(programCreation.copy(signatures = wrongSigs)).isSuccess shouldBe false
    }
  }
/*
  property("Tx with effective date in the past should be invalid") {

    lazy val pastEffDateExecutionBuilderGen: Gen[ExecutionBuilder] = for {
      terms <- validExecutionBuilderTermsGen
      programEndTime <- positiveLongGen
      assetCode <- stringGen
    } yield ExecutionBuilder(terms, assetCode, Instant.now.toEpochMilli - 1L, programEndTime)

    forAll(
      for {
        executionBuilder <- pastEffDateExecutionBuilderGen
        parties <- partiesGen
        signature <- signatureGen
        fee <- positiveLongGen
        timestamp <- positiveLongGen
        numFeeBoxes <- positiveTinyIntGen
        numInvestmentBoxes <- positiveTinyIntGen
      } yield ProgramCreation(
        executionBuilder,
        (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
        parties,
        parties.map { case (_, v) => (v, signatureGen.sample.get) },
        parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get } },
        parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
        timestamp
      )
    ) {
      cc: ProgramCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }

  property("Tx with expiration date in the past should be invalid") {
    lazy val pastExpDateExecutionBuilderGen: Gen[ExecutionBuilder] = for {
      terms <- validExecutionBuilderTermsGen
      programEffectiveTime <- positiveLongGen
      assetCode <- stringGen
    } yield ExecutionBuilder(terms, assetCode, programEffectiveTime, Instant.now.toEpochMilli - 1L)

    forAll(
      for {
        executionBuilder <- pastExpDateExecutionBuilderGen
        parties <- partiesGen
        signature <- signatureGen
        fee <- positiveLongGen
        timestamp <- positiveLongGen
        numFeeBoxes <- positiveTinyIntGen
        numInvestmentBoxes <- positiveTinyIntGen
      } yield ProgramCreation(
        executionBuilder,
        (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
        parties,
        parties.map { case (_, v) => (v, signatureGen.sample.get) },
        parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get } },
        parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
        timestamp
      )
    ) {
      cc: ProgramCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }

  property("Tx with valid expiration date before valid effective date should be invalid") {
    lazy val expBeforeEffExecutionBuilderGen: Gen[ExecutionBuilder] = for {
      terms <- validExecutionBuilderTermsGen
      assetCode <- stringGen
    } yield ExecutionBuilder(terms, assetCode, Instant.now.toEpochMilli + 10000L, Instant.now.toEpochMilli + 1000L)

    forAll(
      for {
        executionBuilder <- expBeforeEffExecutionBuilderGen
        parties <- partiesGen
        signature <- signatureGen
        fee <- positiveLongGen
        timestamp <- positiveLongGen
        numFeeBoxes <- positiveTinyIntGen
        numInvestmentBoxes <- positiveTinyIntGen
      } yield ProgramCreation(
        executionBuilder,
        (0 until numInvestmentBoxes).map { _ => positiveLongGen.sample.get -> positiveLongGen.sample.get },
        parties,
        parties.map { case (_, v) => (v, signatureGen.sample.get) },
        parties.map { case (_, v) => v -> (0 until numFeeBoxes).map { _ => preFeeBoxGen().sample.get } },
        parties.map { case (_, v) => v -> positiveTinyIntGen.sample.get.toLong },
        timestamp
      )
    ) {
      cc: ProgramCreation =>
        val semanticValid = BifrostState.semanticValidity(cc)
        semanticValid.isSuccess shouldBe false
        semanticValid.failed.get shouldBe an[IllegalArgumentException]
    }
  }*/

}
