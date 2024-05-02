package us.jkk

import us.jkk.Main.AddResult.Updated

import scala.collection.mutable

object Main {

  enum AddResult:
    case Added
    case Updated

  enum DeleteResult:
    case NotPresent
    case Deleted

  trait KVStore {
    def add(key: String, value: Int): AddResult
    def get(key: String): Option[Int]
    def delete(key: String): DeleteResult
  }


  class InMemoryKVStore extends KVStore {
    private val store: mutable.HashMap[String, Int] = mutable.HashMap.empty

    override def add(key: String, value: Int): AddResult = {
      val alreadyExisted = store.contains(key)
      store.addOne(key -> value)
      if(alreadyExisted) AddResult.Updated else AddResult.Added
    }

    override def get(key: String): Option[Int] = {
      store.get(key)
    }

    override def delete(key: String): DeleteResult = {
      store.remove(key) match
        case Some(value) => DeleteResult.Deleted
        case None => DeleteResult.NotPresent
    }
  }


  object TestCases {
    def test_get_missing_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      val result = store.get("Unknown")
      assert(result.isEmpty)
    }

    // Delete a key that doesn't exist
    def test_delete_missing_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      val result = store.delete("Unknown")
      assert(result == DeleteResult.NotPresent)
    }

    // Delete an inserted key
    def test_delete_present_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      store.add("key-1", 100)
      val result = store.delete("key-1")
      assert(result == DeleteResult.Deleted)
      val getResult = store.get("key-1")
      assert(getResult.isEmpty)
    }

    // Insert a key that doesn't exist
    def test_insert_missing_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      val result = store.add("key-1", 100)
      assert(result == AddResult.Added)
    }

    // Insert a key that does exist (update)
    def test_insert_existing_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      store.add("key-1", 100)
      val result = store.add("key-1", 200)
      assert(result == AddResult.Updated)
    }

    // Get a key that does exist
    def test_get_new_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      store.add("key-1", 100)
      val result = store.get("key-1")
      assert(result.contains(100))
    }


    // Get a key that does exist, that has been updated
    def test_get_updated_key(): Unit = {
      val store: KVStore = InMemoryKVStore()
      store.add("key-1", 100)
      store.add("key-1", 200)
      val result = store.get("key-1")
      assert(result.contains(200))
    }
  }


  def main(args: Array[String]): Unit = {
    import TestCases._

    def run_tests(): Unit = {
      test_get_missing_key()
      test_delete_missing_key()
      test_delete_present_key()
      test_insert_missing_key()
      test_insert_existing_key()
      test_get_new_key()
      test_get_updated_key()
      println("All tests ran")
    }

    run_tests()
  }
}
