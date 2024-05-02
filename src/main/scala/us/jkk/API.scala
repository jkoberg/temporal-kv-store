package us.jkk

import java.time.Instant

object API {
  type Key = String
  type Value = Int

  case class TimestampedValue(timestamp: Instant, value: Value)

  enum AddResult:
    case Added(at: Instant)

  enum DeleteResult:
    case NotPresent(at: Instant)
    case Deleted(at: Instant)

  /**
   * A temporal key-value store.  When items are added or deleted, the change is tracked with the
   * current time.  Previous values may be found as-of a given instant with `get_at_effective_date`
   */
  trait KVStore:
    def add(key: String, value: Int): AddResult

    def get(key: String): Option[TimestampedValue]

    def delete(key: String): DeleteResult

    def get_at_effective_date(key: String, asOf: Instant): Option[TimestampedValue]
}
