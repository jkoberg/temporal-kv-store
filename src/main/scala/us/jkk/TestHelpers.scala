package us.jkk

import java.time.{Instant, InstantSource}

object TestHelpers {
  /**
   * An InstantSource which maintains a mocked time. For convenience, the time may be set with a single number
   * of seconds, and the current instant will be that number of seconds from a convenient fixed point in time.
   * @param testEpoch The instant serving as the reference point for mocked times.
   */
  class TestClock(testEpoch: Instant = Instant.parse("2024-01-01T00:00:00Z")) extends InstantSource {
    private var mockedInstant: Instant = testEpoch

    override def instant(): Instant = mockedInstant

    /**
     * Get the mock time corresponding to a certain number of seconds from the reference.
     */
    def fromEpoch(seconds: Int): Instant = testEpoch.plusSeconds(seconds)

    /**
     * Set the mocked time to a certain number of seconds from the reference, and return the set value.
     */
    def set(seconds: Int): Instant =
      mockedInstant = fromEpoch(seconds)
      mockedInstant
  }


  /**
   * Ensure two values are equal. If not, print them out for comparison and raise an exception.
   */
  def ensure[T](actual: T, expected: T): Unit = {
    if (actual != expected) {
      println(s"assertion failed!")
      println(s"    expected $expected")
      println(s"         got $actual")
    }
    assert(actual == expected)
  }

}
