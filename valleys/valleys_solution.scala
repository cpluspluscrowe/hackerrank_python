class Solution {
  // solution:
  /*
   turn U and D into the current elevation.  Look for valid valleys.

   */
  def buildElevationString(path: String): Seq[Int] = {
    var elevation: Int = 0
    var elevationPath: Seq[Int] = Seq()
    for (stepType <- path.toCharArray) {
      var stepIncrement: Int = 0
      if (stepType.equals('U')) {
        stepIncrement = 1
      } else {
        stepIncrement = -1
      }
      elevation = elevation + stepIncrement
      elevationPath = elevationPath :+ elevation
    }
    return elevationPath
  }
  def countingValleys(steps: Int, path: String): Int = {
    var valleyCount = 0
    val elevations = buildElevationString(path)
    var index = 0
    while (index < elevations.size) {
      if (elevations(index) < 0) {
        valleyCount = valleyCount + 1
        while (elevations(index) < 0) {
          index = index + 1
        }
      }
      index = index + 1
    }
    valleyCount
  }
}
