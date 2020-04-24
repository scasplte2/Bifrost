package bifrost.history


import bifrost.BifrostGenerators
import bifrost.NodeViewModifier.ModifierId
import bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.iodb.ByteArrayWrapper
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StorageCacheSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with Matchers
  with BifrostGenerators {

  var history: BifrostHistory = generateHistory

  private val conf: Config = ConfigFactory.load("application")
  private val cacheSize: Int = conf.getInt("cache.cacheSize")

  property("The genesis block is stored in cache") {
    val genesisBlockId = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))

    history.storage.blockCache.getIfPresent(genesisBlockId) shouldEqual history.storage.storage.get(genesisBlockId)
  }

  property("Cache should invalidate all entry when it's rolled back in storage") {
    val bestBlockIdKey = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))

    /* Append a new block, make sure it is updated in cache, then drop it */
    val fstBlock:BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(fstBlock).get._1

    history.storage.blockCache.getIfPresent(bestBlockIdKey) should not be null

    history = history.drop(fstBlock.id)

    /* After dropping the new block, the best block should be its parent block, the genesis block */
    /* The block Id for best block in cache should be invalidated */
    history.storage.blockCache.getIfPresent(bestBlockIdKey) shouldBe null

    /* Append multiple times */
    forAll(bifrostBlockGen) { blockTemp =>
      val block:BifrostBlock = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1
    }

    /* Drop all new blocks */
    val height: Int = history.height.toInt
    (2 to height) foreach { _ =>
      history = history.drop(history.bestBlockId)
    }
    /* Same checks as above */
    history.storage.blockCache.getIfPresent(bestBlockIdKey) shouldBe null

    /* Maybe also check other entries like blockScore */
  }

  property("The new block updated is stored in cache") {

    forAll(bifrostBlockGen) { blockTemp =>
      val block:BifrostBlock = blockTemp.copy(parentId = history.bestBlockId)

      history = history.append(block).get._1
      history.storage.blockCache.getIfPresent(ByteArrayWrapper(block.id)) shouldEqual
        history.storage.storage.get(ByteArrayWrapper(block.id))
    }
  }

  property("Appending more entries than the maximum cache size will drop a portion of existing cache") {
    /* Append one block */
    val fstBlock: BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
    history = history.append(fstBlock).get._1

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id)) should not be null

    /* Append a number of new blocks, so that we store more entries than the cache size limit */
    /* Assuming an average new block creates more than 50 entries */
    val numOfBlocks:Int = cacheSize / 50
    (1 to numOfBlocks) foreach { _ =>
      val oneBlock:BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
      history = history.append(oneBlock).get._1
    }

    history.storage.blockCache.getIfPresent(ByteArrayWrapper(fstBlock.id)) shouldBe null
  }

  property("blockLoader should correctly return a block from storage not found in cache") {
    val block: BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
    val tempHistory = history.append(block).get._1

    tempHistory.storage.blockCache.invalidateAll()
    tempHistory.storage.modifierById(block.id) should not be empty
  }

  /* TODO: Benchmarking */

  //  property("Load blocks and read the last ones and compare the performance between cache and storage") {
  //    val numOfBlocks:Int = 550
  //    val numToGoBack:Int = 500
  //    val listBlockId: List[ByteArrayWrapper] = (for (i <- 1 to numOfBlocks) yield {
  //      val oneBlock: BifrostBlock = bifrostBlockGen.sample.get.copy(parentId = history.bestBlockId)
  //      history = history.append(oneBlock).get._1
  //      println(s"forging====$i====${ByteArrayWrapper(oneBlock.id)}")
  //      ByteArrayWrapper(oneBlock.id)
  //    }).take(numToGoBack).toList
  //
  //    val bestBlockIdKey = ByteArrayWrapper(Array.fill(history.storage.storage.keySize)(-1: Byte))
  //    var storageCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
  //    var cacheCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
  //    var tempCurBlockId: ModifierId = history.storage.storage.get(bestBlockIdKey).get.data
  //
  //    /* Read from storage */
  //    val t1 = System.currentTimeMillis
  //    for (i <- 1 to numToGoBack) {
  //      val currentBlock: BifrostBlock = history.storage.storage.get(ByteArrayWrapper(storageCurBlockId)).map { bw =>
  //        val bytes = bw.data
  //        BifrostBlockCompanion.parseBytes(bytes.tail).get
  //      }.get
  //      storageCurBlockId = currentBlock.parentId
  //    }
  //    val storageDuration = (System.currentTimeMillis - t1) / 1e3d
  //
  //    /* Read from cache */
  //    val t2 = System.currentTimeMillis
  //    for (i <- 1 to numToGoBack) {
  //      val currentBlock: BifrostBlock = history.storage.blockCache.getIfPresent(ByteArrayWrapper(cacheCurBlockId)).map {
  //        bw =>
  //          val bytes = bw.data
  //          BifrostBlockCompanion.parseBytes(bytes.tail).get
  //      }.get
  //      cacheCurBlockId = currentBlock.parentId
  //    }
  //    val cacheDuration = (System.currentTimeMillis - t2) / 1e3d
  //
  //    println(s"cache------:$cacheDuration---storage------:$storageDuration")
  //
  //    /* Testing only accessing the storage and cache, not parsing the serialized data */
  //    val t3 = System.currentTimeMillis
  //    for (id <- listBlockId) {
  //      val smt = history.storage.storage.get(id)
  //    }
  //    val tempStorageDuration = (System.currentTimeMillis - t3) / 1e3d
  //
  //    val t4 = System.currentTimeMillis
  //    for (id <- listBlockId) {
  //      val smt = history.storage.blockCache.getIfPresent(id)
  //    }
  //    val tempCacheDuration = (System.currentTimeMillis - t4) / 1e3d
  //
  //    println(s"only  cache:$tempCacheDuration---only storage:$tempStorageDuration")
}
}