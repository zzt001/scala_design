package week4

class BankAccount {
  val balance = Var(0)
  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      balance() = balance() + amount
    }
  }
}
