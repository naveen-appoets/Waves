package com.wavesplatform.it.sync.matcher.smartcontracts

import com.typesafe.config.Config
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.api.SyncMatcherHttpApi._
import com.wavesplatform.it.matcher.MatcherSuiteBase
import com.wavesplatform.it.sync._
import com.wavesplatform.it.util._
import com.wavesplatform.state.ByteStr
import com.wavesplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.wavesplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import scala.concurrent.duration._

class ProofAndAssetPairTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
  nodes.waitForHeightArise()

  val predefAssetPair    = wavesUsdPair
  val unallowedAssetPair = wctWavesPair

  val aliceAsset =
    aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, issueFee, 2).id
  nodes.waitForHeightAriseAndTxPresent(aliceAsset)
  val aliceWavesPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

  "Proofs and AssetPairs verification with SmartContracts" - {
    val sc1 = s"""true"""
    val sc2 = s"""false"""
    val sc3 = s"""
                 |match tx {
                 | case t : Order =>
                 |   let id = t.id == base58''
                 |   let assetPair1Amount = isDefined(t.assetPair.amountAsset)
                 |   let assetPair1Price = if (isDefined(t.assetPair.priceAsset)) then extract(t.assetPair.priceAsset) == base58'${UsdId.toString}' else false
                 |   let assetPair2Amount = if (isDefined(t.assetPair.amountAsset)) then extract(t.assetPair.amountAsset) == base58'${aliceAsset}' else false
                 |   let assetPair2Price = isDefined(t.assetPair.priceAsset)
                 |   (!assetPair1Amount && assetPair1Price) || (assetPair2Amount && !assetPair2Price)
                 | case other => throw()
                 | }
                 |""".stripMargin

    "place order and then setscript on predefAssetPair" - {
      val aliceOrd1 = matcherNode
        .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)
        .message
        .id
      matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

      val aliceOrd2 = matcherNode
        .placeOrder(aliceAcc, aliceWavesPair, OrderType.SELL, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)
        .message
        .id
      matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd2, "Accepted", 1.minute)

      setContract(sc2, aliceAcc)

      val bobOrd1 = matcherNode
        .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)
        .message
        .id
      val bobOrd2 = matcherNode
        .placeOrder(bobAcc, aliceWavesPair, OrderType.BUY, 2.waves * Order.PriceConstant, 500, version = 1, 10.minutes)
        .message
        .id

      matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
      matcherNode.waitOrderStatus(aliceWavesPair, aliceOrd1, "Filled", 1.minute)
      matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
      matcherNode.waitOrderStatus(aliceWavesPair, bobOrd2, "Filled", 1.minute)

      val exchangeTx1 = matcherNode.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx1.id)

      val exchangeTx2 = matcherNode.transactionsByOrder(bobOrd2).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx2.id)

      matcherNode.reservedBalance(bobAcc) shouldBe empty

    }
  }
}
