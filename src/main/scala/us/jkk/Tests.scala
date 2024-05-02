package us.jkk

import java.time.{Instant, InstantSource}

object Tests {
  import API._
  import TestHelpers._

  case class TestDeps(store: KVStore, clock: TestClock)

  // get a key that was never inserted.
  def test_get_missing_key(deps: TestDeps): Unit = {
    import deps.*
    val result = store.get("Unknown")
    assert(result.isEmpty)
  }

  // Delete a key that doesn't exist
  def test_delete_missing_key(deps: TestDeps): Unit = {
    import deps.*
    val result = store.delete("Unknown")
    assert(result == DeleteResult.NotPresent(clock.instant()))
  }

  // Test that:
  //   * add() returns current time
  //   * delete() returns current time
  //   * get() after delete () returns no item
  //   * add() after delete returns current time
  //   * get_at_effective_date() with timestamp after add and before delete returns previously set value
  //   * get() after add, delete, add returns the second value.
  //   * get_at_effective_date() with timestamp after add, delete, add returns second value
  //   * get_at_effective_date() with timestamp before second add returns no value

  def test_temporal_behavior(deps: TestDeps): Unit = {
    import deps.*
    val t10 = clock.set(10)
    assert(store.add("key-1", 100) == AddResult.Added(t10))
    val t20 = clock.set(20)
    assert(store.delete("key-1") == DeleteResult.Deleted(t20))
    val t30 = clock.set(30)
    assert(store.get("key-1").isEmpty)
    val t40 = clock.set(40)
    assert(store.add("key-1", 200) == AddResult.Added(t40))
    val t15 = clock.fromEpoch(15)
    ensure(store.get_at_effective_date("key-1", t15), Some(TimestampedValue(t10, 100)))
    val t25 = clock.fromEpoch(25)
    assert(store.get_at_effective_date("key-1", t25).isEmpty)
    val t50 = clock.set(50)
    ensure(store.get("key-1"), Some(TimestampedValue(t40, 200)))
    val t60 = clock.fromEpoch(60)
    ensure(store.get_at_effective_date("key-1", t60), Some(TimestampedValue(t40, 200)))
  }

  // Insert a key that doesn't exist
  def test_insert_missing_key(deps: TestDeps): Unit = {
    import deps.*
    val result = store.add("key-1", 100)
    assert(result == AddResult.Added(clock.instant()))
  }

  // Get a key that does exist
  def test_get_new_key(deps: TestDeps): Unit = {
    import deps.*
    store.add("key-1", 100)
    ensure(store.get("key-1"), Some(TimestampedValue(clock.instant(), 100)))
  }


  // Get a key that does exist, that has been updated
  def test_get_updated_key(deps: TestDeps): Unit = {
    import deps.*
    val t10 = clock.set(10)
    store.add("key-1", 100)
    val t20 = clock.set(20)
    store.add("key-1", 200)
    ensure(store.get("key-1"), Some(TimestampedValue(t20, 200)))
  }

  val testSuite = Seq(
    test_get_missing_key,
    test_delete_missing_key,
    test_temporal_behavior,
    test_insert_missing_key,
    test_get_new_key,
    test_get_updated_key,
  )


  def run_tests(): Unit = {
    for {
      test <- testSuite
    } {
      val clock = TestClock()
      val deps = TestDeps(InMemoryImplementation.InMemoryKVStore(clock), clock)
      try {
        test(deps)
      } catch {
        case e: AssertionError =>
          println(s"Test failed: ${e.getMessage}\n${e.printStackTrace()}\n\n")
      }
    }
    println("All tests ran")
  }

}
