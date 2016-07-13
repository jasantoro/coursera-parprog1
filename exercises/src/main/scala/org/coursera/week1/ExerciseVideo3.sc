class HelloWorld extends Thread {
  override def run(): Unit = {
    println("Hello World!")
  }
}
val t = new HelloWorld
t.start()
t.join()

private var uidCount = 0L
def getUniqueId(): Long = {
  uidCount = uidCount + 1
  uidCount
}

val x = new AnyRef {}
def getUniqueIdSync(): Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}

def startThread() = {
  val t = new Thread {
    override def run(): Unit = {
      val uids = for (i <- 0 until 10) yield getUniqueIdSync()
      println(uids)
    }
  }
  t.start()
  t
}

val t1 = startThread()
val t2 = startThread()
t1.join()
t2.join()

