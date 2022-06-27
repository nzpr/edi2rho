package coop.rchain.rabbit2rho.transformers

import coop.rchain.models.Par
import dev.profunktor.fs2rabbit.model.QueueName

trait Codec[I, R] {
  // for a tag that identifies the source - decoder from I to String
  def input: Map[String, I => String]
  // decoder of data on result channel
  def result: Seq[Par] => Option[R]
}

final case class Contract[I, R](codec: Codec[I, R], term: String, name: String)
