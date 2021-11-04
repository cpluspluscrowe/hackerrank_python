class VideoGamePurchaseCounter {
  def howManyGames(
      price: Int,
      decrease: Int,
      baseline: Int,
      money: Int
  ): Int = {
    var cost = 0
    var buyCount = 0
    var currentCost = p
    if (money < price) return 0
    while (currentCost > baseline) {
      if (cost + currentCost >= money) return buyCount
      cost = cost + currentCost
      buyCount = buyCount + 1
      currentCost = Math.max(currentCost - decrease, baseline)
    }
    var leftoverMoney = money - cost
    val videoGamePurchaseCountAtAFixedCost = leftoverMoney / baseline
    buyCount + videoGamePurchaseCountAtAFixedCost
  }
}
