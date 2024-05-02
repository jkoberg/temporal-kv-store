package us.jkk

import java.time.{Instant, InstantSource}
import scala.collection.Searching.{Found, InsertionPoint}
import scala.collection.mutable

object InMemoryImplementation {

  import API.*

  /**
   * Implements KVStore with an in-memory data structure.
   * Assumptions:
   * * Every call to clock.instant() will return a larger value.
   * * Store is accesses in a single-threaded manner; due to mutable data structures and lack of
   * serialization, this implementation is not thread-safe.
   *
   * @param clock The system clock to be used to generate timestamps for mutations.
   */
  class InMemoryKVStore(clock: InstantSource) extends KVStore:
    /**
     * This differs from `TimestampedValue` because we want to record None values separately
     * from their timestamp - in other words, a tombstone is recorded as TimestampedEntry(ts, None)
     */
    private case class TimestampedEntry(timestamp: Instant, value: Option[Value])

    /**
     * A mapping from key to in-order array of timestamped values.
     *
     * New arrays are created at a key when an item is inserted, and deletion will insert a tombstone
     * record, so it's never the case that a key holds an empty array, or a key is deleted.
     *
     * Because the user doesn't specify the mutation timestamp, and we assume our clock is monotonic,
     * it's guaranteed that array entries are in increasing-timestamp order.
     */
    private val store = mutable.HashMap.empty[String, mutable.ArrayBuffer[TimestampedEntry]]

    override def add(key: String, value: Int): AddResult =
      val now = clock.instant()
      val newEntry = TimestampedEntry(now, Some(value))
      val arr = store.getOrElseUpdate(key, mutable.ArrayBuffer()).addOne(newEntry)
      AddResult.Added(now)

    override def get(key: String): Option[TimestampedValue] =
      for
        arr <- store.get(key)
        lastItem = arr.last
        // if the last retrieved item has a None value, it's a deletion tombstone, so the for-comprehension
        // will end up returning None, and we don't need to otherwise handle it.
        v <- lastItem.value
      yield
        TimestampedValue(lastItem.timestamp, v)

    override def delete(key: String): DeleteResult =
      val now = clock.instant()
      store.get(key) match
        case None =>
          // Nothing ever inserted at this key. No need to do anything. Just return NotPresent.
          DeleteResult.NotPresent(now)
        case Some(arr) =>
          arr.last.value match
            case None =>
              // Last added entry was a deletion tombstone. No need to delete again.
              // Return the deletion time of the last deletion.
              DeleteResult.Deleted(arr.last.timestamp)
            case Some(value) =>
              // Last added entry was a value. Insert a deletion tombstone
              val tombstone = TimestampedEntry(now, None)
              arr.addOne(tombstone)
              DeleteResult.Deleted(now)

    override def get_at_effective_date(key: String, asOf: Instant): Option[TimestampedValue] =
      // Since the last-inserted entry for a key may be a deletion tombstone, we use flatMap
      // to handle the None values present in the tombstones, without additional special logic.
      store.get(key).flatMap { arr =>
        val queryPoint = TimestampedEntry(asOf, None) // Because our Ordering ignores the value, we just use None.
        arr.search(queryPoint)(Ordering.by(e => e.timestamp)) match
          case Found(foundIndex) =>
            val item = arr(foundIndex)
            item.value.map(v => TimestampedValue(item.timestamp, v)) // Deleted (None) will map to None
          case InsertionPoint(i) if i == 0 =>
            // Array search left us before the first element. No value as of the query time.
            None
          case InsertionPoint(i) =>
            val latest = arr(i - 1)
            latest.value.map(v => TimestampedValue(latest.timestamp, v)) // Deleted (None) will map to None
      }
}
