private var uidCount = 0L
val x = new AnyRef {}
def getUniqueId(): Long = x.synchronized {
  uidCount = uidCount + 1
  uidCount
}

class Account(private var amount: Int = 0) {
  val uid = getUniqueId()
  private def lockAndtransfer(target: Account, n: Int) =
    this.synchronized {
      target.synchronized {
        this.amount -= n
        target.amount += n
      }
    }
  def transfer(target: Account, n: Int) =
    if(this.uid < target.uid) this.lockAndtransfer(target, n)
    else target.lockAndtransfer(this, -n)
}

def startThread(a: Account, b: Account, n: Int) = {
  val t = new Thread {
    override def run(): Unit = {
      for (i <- 0 until n) {
        a.transfer(b, 1)
      }
    }
  }
  t.start()
  t
}

val a1 = new Account(500)
val a2 = new Account(700)

val t1 = startThread(a1, a2, 150)
val t2 = startThread(a2, a1, 150)
t1.join()
t2.join()