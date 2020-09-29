package bifrost

import bifrost.modifier.ModifierId
import bifrost.modifier.box.{ Box, ProgramBox }
import com.google.common.primitives.Ints

import scala.util.{ Failure, Success, Try }

package object state {
  /** This function will modify the state storage directly without returning a new instance of state
   * USE WITH EXTREME CAUTION!! */
  def directlyAddStateStorage( version: Int, boxes: Set[Box], state: State): Unit = {
    // Manually manipulate state
    val boxSC = StateChanges(Set(), boxes)
    val versionId = ModifierId(Ints.toByteArray(version))

    // this works by updating the underlying storage object directly and ignoring the updated state instance
    state.applyChanges(versionId, boxSC) match {
      case Success(_) => Unit
      case Failure(ex)    => throw ex
    }
  }

  /** This function will modify the PBR storage directly without returning a new instance of the registry
   * USE WITH EXTREME CAUTION!! */
  def directlyAddPBRStorage[BX: ProgramBox] ( version: Int, boxes: Set[BX], state: State): Unit = {
    // Manually manipulate state
    val versionId = ModifierId(Ints.toByteArray(version))

    val updates = boxes.map(bx => bx.value -> BoxId(bx.id)).toMap
    val pbrSC = ProgramRegistryChanges(Map(), updates)

    // this works by updating the underlying storage object directly and ignoring the updated state instance
    directlyAddStateStorage(version, boxes, state)
    state.pbrOpt.get.update(versionId, pbrSC.toRemove, pbrSC.toUpdate) match {
      case Success(_) => Unit
      case Failure(ex)  => throw ex
    }
  }
}