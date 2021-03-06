package co.topl.network

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import co.topl.network.message._
import co.topl.network.peer.{ConnectedPeer, PeerInfo, PeerSpec, PenaltyType}
import co.topl.settings.{AppContext, NetworkSettings}
import co.topl.utils.Logging
import shapeless.syntax.typeable._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.postfixOps

/**
 * Responsible for discovering and sharing new peers.
 */
class PeerSynchronizer ( networkControllerRef: ActorRef,
                         peerManager         : ActorRef,
                         settings            : NetworkSettings,
                         appContext      : AppContext
                       )( implicit
                          ec: ExecutionContext
                       ) extends Synchronizer
                                 with Logging {

  // Import the types of messages this actor can SEND

  import PeerManager.ReceivableMessages.{AddPeerIfEmpty, RecentlySeenPeers}
  import co.topl.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessageSpecs, SendToNetwork}

  private implicit val timeout: Timeout = Timeout(settings.syncTimeout.getOrElse(5 seconds))

  // types of remote messages to be handled by this synchronizer
  protected val peersSpec: PeersSpec = appContext.peerSyncRemoteMessages.peersSpec
  protected val getPeersSpec: GetPeersSpec = appContext.peerSyncRemoteMessages.getPeersSpec

  // partial functions for identifying local method handlers for the messages above
  protected val msgHandlers: PartialFunction[(MessageSpec[_], _, ConnectedPeer), Unit] = {
    case (_: PeersSpec, data: PeersData, _) => addNewPeers(data.peers)
    case (_: GetPeersSpec, _, remote)       => gossipPeers(remote)
  }

  override def preStart: Unit = {
    networkControllerRef ! RegisterMessageSpecs(
      appContext.peerSyncRemoteMessages.toSeq,
      self
      )

    val msg = Message[Unit](getPeersSpec, Right(Unit), None)
    context.system.scheduler.scheduleWithFixedDelay(2.seconds,
                                                    settings.getPeersInterval,
                                                    networkControllerRef,
                                                    SendToNetwork(msg, SendToRandom))
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT && MESSAGE PROCESSING FUNCTIONS
  override def receive: Receive = {
    processDataFromPeer orElse
      nonsense
  }

  /**
   * Handles adding new peers to the peer database if they were previously unknown
   *
   * @param peers sequence of peer specs describing a remote peers details
   */
  private def addNewPeers ( peers: Seq[PeerSpec] ): Unit =
    if ( peers.cast[Seq[PeerSpec]].isDefined ) {
      peers.foreach(peerSpec => peerManager ! AddPeerIfEmpty(peerSpec))
    }

  /**
   * Handles gossiping about the locally known peer set to a given remote peer
   *
   * @param remote the remote peer to be informed of our local peers
   */
  private def gossipPeers ( remote: ConnectedPeer ): Unit =
    (peerManager ? RecentlySeenPeers(settings.maxPeerSpecObjects))
      .mapTo[Seq[PeerInfo]]
      .foreach
      { peers =>
        val msg = Message(peersSpec, Right(PeersData(peers.map(_.peerSpec))), None)
        networkControllerRef ! SendToNetwork(msg, SendToPeer(remote))
      }

  override protected def penalizeMaliciousPeer ( peer: ConnectedPeer ): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.PermanentPenalty)
  }

  protected def nonsense: Receive = { case nonsense: Any =>
    log.warn(s"NodeViewSynchronizer: got unexpected input $nonsense from ${sender()}")
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object PeerSynchronizer {

  case class RemoteMessageHandler ( peersSpec: PeersSpec, getPeersSpec: GetPeersSpec ) {

    def toSeq: Seq[MessageSpec[_]] = Seq(peersSpec, getPeersSpec)
  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object PeerSynchronizerRef {

  def apply ( networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings,
              appContext      : AppContext
            )(
              implicit
              system: ActorSystem,
              ec    : ExecutionContext
            ): ActorRef =
    system.actorOf(
      props(networkControllerRef, peerManager, settings, appContext)
      )

  def apply (
              name                : String,
              networkControllerRef: ActorRef,
              peerManager         : ActorRef,
              settings            : NetworkSettings,
              appContext      : AppContext
            )( implicit system: ActorSystem, ec: ExecutionContext ): ActorRef =
    system.actorOf(
      props(networkControllerRef, peerManager, settings, appContext),
      name
      )

  def props ( networkControllerRef: ActorRef, peerManager: ActorRef, settings: NetworkSettings,
              appContext      : AppContext
            )(
              implicit ec: ExecutionContext
            ): Props =
    Props(
      new PeerSynchronizer(
        networkControllerRef,
        peerManager,
        settings,
        appContext
        )
      )

}
