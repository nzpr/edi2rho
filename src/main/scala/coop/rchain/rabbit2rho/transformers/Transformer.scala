package coop.rchain.rabbit2rho.transformers

trait Transformer[I, R] {
  def contractName: String
  def contractVersion: String
  def contract: Contract[I, R]
  def inputNames: Seq[String]
  def inputTags: Seq[String]
}
